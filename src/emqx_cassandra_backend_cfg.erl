
-module(emqx_cassandra_backend_cfg).

-export([register/0, unregister/0]).

-define(APP, emqx_cassandra_backend).

register() ->
    clique_config:load_schema([code:priv_dir(?APP)], ?APP),
    register_config().

unregister() ->
    unregister_config(),
    clique_config:unload_schema(?APP).

%%--------------------------------------------------------------------
%% Set ENV Register Config
%%--------------------------------------------------------------------
register_config() ->
    Keys = keys(),
    [clique:register_config(Key , fun config_callback/2) || Key <- Keys],
    clique:register_config_whitelist(Keys, ?APP).

config_callback([_, Key], Value) ->
    application:set_env(?APP, list_to_atom(Key), Value),
    " successfully\n".

%%--------------------------------------------------------------------
%% UnRegister config
%%--------------------------------------------------------------------
unregister_config() ->
    Keys = keys(),
    [clique:unregister_config(Key) || Key <- Keys],
    clique:unregister_config_whitelist(Keys, ?APP).

keys() ->
    ["cassandra_backend.bootstrap_ips",
    "cassandra_backend.port",
    "cassandra_backend.username",
    "cassandra_backend.password",
    "cassandra_backend.keyspace"
    ].
