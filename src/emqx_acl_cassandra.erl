
-module(emqx_acl_cassandra).

-behaviour(emqx_acl_mod).

-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").
-include_lib("common_test/include/ct.hrl").

%% ACL Callbacks
-export([init/1, check_acl/5, register_metrics/0]).

-record(state, {acl_query}).

-define(CPHONETYPE, <<1>>).


-define(ACL_METRICS,
        ['acl.cassandra.allow',
         'acl.cassandra.deny',
         'acl.cassandra.ignore'
        ]).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?ACL_METRICS).

init(AclQuery) ->
    {ok, #state{acl_query = AclQuery}}.


is_a_valid_uuid(Bin) when is_binary(Bin) ->
    % is_a_valid_uuid(binary_to_list(Bin));
    case erlcass_uuid:get_version(Bin) of
      {error, _} -> false;
      {ok,4}     -> true
    end.

check_acl(ClientInfo, PubSub, Topic, NoMatchAction, State) ->
    R = do_check_acl(ClientInfo, PubSub, Topic, NoMatchAction, State),
    ?LOG(error, "check_acl: ~p", [R]),
    case do_check_acl(ClientInfo, PubSub, Topic, NoMatchAction, State) of
        ok -> emqx_metrics:inc('acl.cassandra.ignore'), ok;
        {stop, allow} -> emqx_metrics:inc('acl.cassandra.allow'), {stop, allow};
        {stop, deny} -> emqx_metrics:inc('acl.cassandra.deny'), {stop, deny}
    end.

do_check_acl(ClientInfo, PubSub, Topic, _NoMatchAction, #{acl_query := {AclSql, AclParams}}) ->
    %% So the logic:
    %%  0. topcis are generally of the form CliendID/topic to simplify ACL!
    %%  1. webservice should be able to subscribe to anything
    %%  2. devices can only pubsub to sub-topics of their own
    case string:split(Topic, "/") of
      [TopicRoot, Tail] -> ok;
      [TopicRoot] -> ok
    end,
    {ok, WebSerivceId} = application:get_env(?APP, webservice_client_id),
    ClientId = binary_to_list(maps:get(clientid, ClientInfo)),
    case string:equal(WebSerivceId, ClientId) of
      true -> ok;
      false ->
        % devices from here on
        case is_a_valid_uuid(TopicRoot) of
             %% check whether topic matches with client id
             true -> case string:equal(binary_to_list(TopicRoot), ClientId) of
               true -> {stop, allow};
               false -> {stop, deny}
             end;
              %% for general topics, only allow subscription for devices
             _-> case PubSub of
               publish -> {stop, deny};
               subscribe -> {stop, allow}
             end
        end
    end.

% check_self(Creds, Topic = <<CLid/binary>>) ->
%     check_self(Creds, uuid:to_string(CLid));
%
% check_self(Credentials=#{username := Username}, Topic) ->
%     case string:equal(Username, Topic) of
%         true  -> ok;
%         false -> nomatch
%     end.

% is_phone_client(Credentials=#{username := Username}) ->
%     case string:split(Username, "phone") of
%         nomatch -> false;
%         _       -> true
%     end.
%
% device_owned_by_phone(_, <<>>) ->
%     false;
%
% device_owned_by_phone(TopicUUID, DeviceIDs) ->
%     <<0,0,0,16, Duuid:128, Rest/binary>> = DeviceIDs,
%     case binary:encode_unsigned(Duuid) =:= TopicUUID of
%         true  -> true;
%         false -> device_owned_by_phone(TopicUUID, Rest)
%     end.
