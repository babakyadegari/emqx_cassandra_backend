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

-behaviour(application).

-export([start/2, stop/1]).

start(_Start, _State) ->
    {ok, Sup} = emqx_cassandra_backend_sup:start_link(),
    emqx_cassandra_backend_cfg:register(),
    %emqx_logger:error("cassandra_backend: ~p ~n", [application:get_all_env()]),
    %io:format("~p ~n", application:get_all_env()),
    %cassandra_client:load(application:get_all_env()),
    emqx_cassandra_backend:load(),
    {ok, Sup}.

stop(_State) ->
    emqx_cassandra_backend:unload(),
    emqx_cassandra_backend_cfg:unregister(),
    ok.

