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

-module(emqx_cassandra_backend_app).

-include("emqx_cassandra_backend.hrl").

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

start(_Start, _State) ->
    {ok, Sup} = emqx_cassandra_backend_sup:start_link(),
    emqx_cassandra_backend:load(),
    emqx_cassandra_backend_cfg:register(),
    marina_sup:start_link(),
    if_enabled(auth_query, fun reg_authmod/1),
    if_enabled(acl_query,  fun reg_aclmod/1),
    %io:format("~p ~n", application:get_all_env()),
    %{ok, Sup2} = marina_sup:start_link(),
    {ok, Sup}.

reg_authmod(AuthQuery) ->
	%io:format("register cassandra_auth ~n"),
    SuperQuery = emqx_auth_cassandra_cli:parse_query(application:get_env(?APP, super_query, undefined)),
    {ok, HashType} = application:get_env(?APP, password_hash),
    AuthEnv = {AuthQuery, SuperQuery, HashType},
    emqx_access_control:register_mod(auth, emqx_auth_cassandra, AuthEnv).

reg_aclmod(AclQuery) ->
    emqx_access_control:register_mod(acl, emqx_acl_cassandra, AclQuery).

stop(_State) ->
    prep_stop(_State),
    emqx_cassandra_backend:unload(),
    emqx_cassandra_backend_cfg:unregister(),
    ok.

if_enabled(Cfg, Fun) ->
	%Ss = application:get_env(?APP, Cfg),
	%io:format("this is ~p ~p ~p ~n", [?APP, Cfg, Ss]),
    case application:get_env(?APP, Cfg) of
        {ok, Query} -> Fun(emqx_auth_cassandra_cli:parse_query(Query));
        undefined   -> ok
    end.

prep_stop(State) ->
    emqx_access_control:unregister_mod(auth, emqx_auth_cassandra),
    emqx_access_control:unregister_mod(acl, emqx_acl_cassandra),
    emqx_auth_cassandra_cfg:unregister(),
    State.
