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

-module(emqx_auth_cassandra_cli).

-include_lib("emqx/include/emqx.hrl").
-include_lib("marina/include/marina.hrl").
-include("emqx_cassandra_backend.hrl").

-export([parse_query/1]).
-export([query/3]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
    undefined;
parse_query(Sql) ->
    case re:run(Sql, "'%[uca]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {re:replace(Sql, "'%[uca]'", "?", [global, {return, list}]), Params};
        nomatch ->
            {Sql, []}
    end.

%%--------------------------------------------------------------------
%% cassandra Query
%%--------------------------------------------------------------------
%
query(Sql, Params, Credentials) ->
    cassandra_cli:query(list_to_binary(Sql), #{values => replvar(Params, Credentials), timeout => 1000}).

%marina:query(<<"select password from smartpot.user where username = ? limit 1 ALLOW FILTERING">>, #{values => <<"test">>, timeout => 1000}).

replvar(Params, Credentials) ->
    replvar(Params, Credentials, []).

replvar([], _Credentials, Acc) ->
    lists:reverse(Acc);
replvar(["'%u'" | Params], Credentials = #{client_id := ClientId}, Acc) ->
    replvar(Params, Credentials, [uuid:to_binary(binary_to_list(ClientId)) | Acc]);
replvar(["'%c'" | Params], Credentials = #{client_id := ClientId}, Acc) ->
    replvar(Params, Credentials, [ClientId | Acc]);
replvar(["'%a'" | Params], Credentials = #{peername := {IpAddr, _}}, Acc) ->
    replvar(Params, Credentials, [list_to_binary(inet_parse:ntoa(IpAddr)) | Acc]);
replvar([Param | Params], Credentials, Acc) ->
    replvar(Params, Credentials, [Param | Acc]).

