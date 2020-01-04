
-module(emqx_cassandra_backend_app).

-include("emqx_cassandra_backend.hrl").

-behaviour(application).

-import(emqx_auth_cassandra_cli, [parse_query/1]).

-export([start/2, stop/1, prep_stop/1]).

start(_Start, _State) ->
    {ok, Sup} = emqx_cassandra_backend_sup:start_link(),
    emqx_cassandra_backend_cfg:register(),

    {ok, ErlcassConf} = application:get_env(?APP, erlcass),
    application:set_env([{erlcass, ErlcassConf}]),
    ok = application:start(erlcass),

    if_enabled(auth_query, fun reg_authmod/1),
    if_enabled(acl_query,  fun reg_aclmod/1),

    {ok, Sup}.

reg_authmod(AuthQuery) ->
    ok = emqx_auth_cassandra:register_metrics(),
    SuperQuery = parse_query(application:get_env(?APP, super_query, undefined)),
    {ok, HashType} = application:get_env(?APP, password_hash),
    Params = #{auth_query  => AuthQuery,
               super_query => SuperQuery,
               hash_type   => HashType},

    ok = emqx:hook('client.authenticate', fun emqx_auth_cassandra:check/3, [Params]),
    {Pquery, _} =  AuthQuery,
    ok = cassandra_cli:register_a_query(cassandra_auth_query, Pquery),
    {SPquery, _} =  SuperQuery,
    cassandra_cli:register_a_query(cassandra_auth_super_query, SPquery).

reg_aclmod(AclQuery) ->
    %emqx_access_control:register_mod(acl, emqx_acl_cassandra, AclQuery).
    ok = emqx_acl_cassandra:register_metrics(),
    ok = emqx:hook('client.check_acl', fun emqx_acl_cassandra:check_acl/5, [#{acl_query => AclQuery}]),
    {Pquery, _} =  AclQuery,
    cassandra_cli:register_a_query(cassandra_acl_query, Pquery).


stop(_State) ->
    prep_stop(_State),
    erlcass:stop(),
    emqx_cassandra_backend:unload(),
    emqx_cassandra_backend_cfg:unregister(),
    ok.

if_enabled(Cfg, Fun) ->
    case application:get_env(?APP, Cfg) of
        {ok, Query} -> Fun(parse_query(Query));
        undefined   -> ok
    end.

prep_stop(State) ->
    % emqx_access_control:unregister_mod(auth, emqx_auth_cassandra),
    % emqx_access_control:unregister_mod(acl, emqx_acl_cassandra),
    % emqx_auth_cassandra_cfg:unregister(),

    emqx:unhook('client.authenticate', fun emqx_auth_cassandra:check/3),
    emqx:unhook('client.check_acl', fun emqx_acl_cassandra:check_acl/5),
    emqx_cassandra_backend_cfg:unregister(),
    State.
