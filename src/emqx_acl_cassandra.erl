%% Copyright (c) 2018 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_acl_cassandra).

-behaviour(emqx_acl_mod).

-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").
%% ACL Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

-record(state, {acl_query}).

-define(CPHONETYPE, <<1>>).

init(AclQuery) ->
    {ok, #state{acl_query = AclQuery}}.

check_acl({#{username := <<$$, _/binary>>}, _PubSub, _Topic}, _State) ->
    ignore;

check_acl({Credentials, PubSub, Topic}, #state{acl_query = {AclSql, AclParams}}) ->
    io:format("Creds: ~p ~nPubSub: ~p~nTopic ~p ~n", [Credentials, PubSub, Topic]),
    %% So the logic:
    %%  1. phone devices can pubsub to subtopics of devices they own
    %%  2. non-phone devices can only pubsub to sub-topics of their own
    [TopL, _] = string:split(Topic, "/"),
    case uuid:to_binary(binary_to_list(TopL)) of
        badarg  -> ignore;
        TopLevel -> 
        case check_self(Credentials, TopLevel) of
            ok      -> allow;
            nomatch -> 
            % case is_phone_client(Credentials) of
            %     false -> deny;
            %     true  ->
                case emqx_auth_cassandra_cli:query(AclSql, AclParams, Credentials) of
                    {ok, {result, {_, _, _, _}, 1, []}}               -> ignore;
                    {ok, {result, {_, _, _, _}, 1, [[_, _]]}}            -> ignore;
                    {ok, {result, {_, _, _, _}, 1, [[_, ClientType, DeviceIDs]]}} ->
                    case ClientType of
                        ?CPHONETYPE ->
                            %% get all devices belonging to this user
                            <<_:32, DevUUIDs/binary>> = DeviceIDs,
                            case device_owned_by_phone(TopLevel, DevUUIDs) of
                                true -> allow;
                                _    -> deny
                            end;
                        _ -> deny
                    end;
                        % Rules = filter(PubSub, compile(Rows)),
                        % case match(Credentials, Topic, Rules) of
                        %     {matched, allow} -> allow;
                        %     {matched, deny}  -> deny;
                        %     nomatch          -> ignore
                        % end;
                    {error, Reason} ->
                        logger:error("cassandra check_acl error: ~p~n", [Reason]),
                        deny
                end
            % end
        end
    end.

check_self(Credentials=#{username := Username}, Topic) ->
    case string:equal(Username, Topic) of
        true  -> ok;
        false -> nomatch
    end.

is_phone_client(Credentials=#{username := Username}) ->
    case string:split(Username, "phone") of
        nomatch -> false;
        _       -> true
    end.

device_owned_by_phone(_, <<>>) -> 
    false;
device_owned_by_phone(TopicUUID, DeviceIDs) ->
    <<0,0,0,16, Duuid:128, Rest/binary>> = DeviceIDs,
    case binary:encode_unsigned(Duuid) =:= TopicUUID of 
        true  -> true;
        false -> device_owned_by_phone(TopicUUID, Rest)
    end.


match(_Credentials, _Topic, []) ->
    nomatch;

match(Credentials, Topic, [Rule|Rules]) ->
    case emqx_access_rule:match(Credentials, Topic, Rule) of
        nomatch ->
            match(Credentials, Topic, Rules);
        {matched, AllowDeny} ->
            {matched, AllowDeny}
    end.

filter(PubSub, Rules) ->
    [Term || Term = {_, _, Access, _} <- Rules,
             Access =:= PubSub orelse Access =:= pubsub].

compile(Rows) ->
    compile(Rows, []).
compile([], Acc) ->
    Acc;
compile([[Allow, IpAddr, Username, ClientId, Access, Topic]|T], Acc) ->
    Who  = who(IpAddr, Username, ClientId),
    Term = {allow(Allow), Who, access(Access), [topic(Topic)]},
    compile(T, [emqx_access_rule:compile(Term) | Acc]).

who(_, <<"$all">>, _) ->
    all;
who(null, null, null) ->
    throw(undefined_who);
who(CIDR, Username, ClientId) ->
    Cols = [{ipaddr, b2l(CIDR)}, {user, Username}, {client, ClientId}],
    case [{C, V} || {C, V} <- Cols, not empty(V)] of
        [Who] -> Who;
        Conds -> {'and', Conds}
    end.

allow(1)  -> allow;
allow(0)  -> deny;
allow(<<"1">>)  -> allow;
allow(<<"0">>)  -> deny.

access(1) -> subscribe;
access(2) -> publish;
access(3) -> pubsub;
access(<<"1">>) -> subscribe;
access(<<"2">>) -> publish;
access(<<"3">>) -> pubsub.

topic(<<"eq ", Topic/binary>>) ->
    {eq, Topic};
topic(Topic) ->
    Topic.

reload_acl(_State) ->
    ok.

description() ->
    "ACL with cassandra".

b2l(null) -> null;
b2l(B)    -> binary_to_list(B).

empty(null) -> true;
empty("")   -> true;
empty(<<>>) -> true;
empty(_)    -> false.
