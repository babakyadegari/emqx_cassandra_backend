
-module(emqx_auth_cassandra).

-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").

-export([init/1, check/3, description/0, register_metrics/0]).

-record(state, {auth_query, super_query, hash_type}).

-define(EMPTY(Username), (Username =:= undefined orelse Username =:= <<>>)).

init({AuthQuery, SuperQuery, HashType}) ->
    {ok, #state{auth_query = AuthQuery, super_query = SuperQuery, hash_type = HashType}}.


-define(AUTH_METRICS,
        ['auth.cassandra.success',
         'auth.cassandra.failure',
         'auth.cassandra.ignore'
        ]).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).

check(ClientInfo = #{password := Password, clientid := ClientId}, AuthResult,
                    #{auth_query  := {AuthSql, AuthParams},
                      super_query := SuperQuery,
                      hash_type   := HashType}) ->
    Result = case emqx_auth_cassandra_cli:query(cassandra_auth_query, AuthParams, ClientInfo) of
          {ok, [<<"password">>, <<"salt">>], [[PassHash, Salt]]} ->
              emqx_passwd:check_pass({PassHash, Salt, Password}, HashType);
          {ok, [{<<"password">>, text}],[[PassHash]]} ->
              emqx_passwd:check_pass({PassHash, Password}, HashType);
          {ok, _Columns, []} ->
              {error, not_found};
          {error, Reason} ->
              ?LOG(error, "[CASSANDRA] auth query '~p' failed: ~p", [AuthSql, Reason]),
              {error, Reason}
      end,
    ?LOG(error, "Results: ~p", [Result]),
    case Result of
      ok ->
          emqx_metrics:inc('auth.cassandra.success'),
          {stop, AuthResult#{is_superuser => is_superuser(SuperQuery, ClientInfo),
                              anonymous => false,
                              auth_result => success}};
      {error, not_found} ->
          emqx_metrics:inc('auth.cassandra.failed'), ok;
      {error, ResultCode} ->
          ?LOG(debug, "[CASSANDRA] auth from cassandra failed: ~p", [ResultCode]),
          emqx_metrics:inc('auth.cassandra.failure'),
          {stop, AuthResult#{auth_result => ResultCode, anonymous => false}}
    end.

%%--------------------------------------------------------------------
%% Is Superuser?
%%--------------------------------------------------------------------

-spec(is_superuser(undefined | {string(), list()}, emqx_types:credentials()) -> boolean()).
is_superuser(undefined, _Credentials) ->
    false;
is_superuser({SuperSql, Params}, ClientInfo) ->
    case emqx_auth_cassandra_cli:query(cassandra_auth_super_query, Params, ClientInfo) of
        {ok, [{<<"is_superuser">>, tinyint}], [[1]]} ->
            true;
        {ok, [{<<"is_superuser">>, tinyint}], [[_]]} ->
            false;
        {error, _Error} ->
            ?LOG(error, "[CASSANDRA] super_user query '~p' failed: ~p", [SuperSql, _Error]),
            false
    end.

description() -> "Authentication with cassandra".
