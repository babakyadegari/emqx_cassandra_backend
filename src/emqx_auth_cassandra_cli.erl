
-module(emqx_auth_cassandra_cli).

-include_lib("emqx/include/emqx.hrl").
-include_lib("erlcass/include/erlcass.hrl").
-include("emqx_cassandra_backend.hrl").

-export([parse_query/1]).
-export([query/3, replvar/3]).

%%--------------------------------------------------------------------
%% Avoid SQL Injection: Parse SQL to Parameter Query.
%%--------------------------------------------------------------------

parse_query(undefined) ->
    undefined;
parse_query(Sql) ->
    case re:run(Sql, "'%[ucCad]'", [global, {capture, all, list}]) of
        {match, Variables} ->
            Params = [Var || [Var] <- Variables],
            {re:replace(Sql, "'%[ucCad]'", "?", [global, {return, list}]), Params};
        nomatch ->
            {Sql, []}
    end.

%%--------------------------------------------------------------------
%% cassandra Query
%%--------------------------------------------------------------------
%
query(Query, Params, Credentials) ->
    erlcass:execute(Query, replvar(Params, Credentials)).


replvar(Params, ClientInfo) ->
    replvar(Params, ClientInfo, []).

replvar([], _ClientInfo, Acc) ->
    lists:reverse(Acc);

replvar(["'%u'" | Params], ClientInfo = #{username := Username}, Acc) ->
    replvar(Params, ClientInfo, [Username | Acc]);
replvar(["'%c'" | Params], ClientInfo = #{client_id := ClientId}, Acc) ->
    replvar(Params, ClientInfo, [ClientId | Acc]);
replvar(["'%a'" | Params], ClientInfo = #{peername := {IpAddr, _}}, Acc) ->
    replvar(Params, ClientInfo, [inet_parse:ntoa(IpAddr) | Acc]);
replvar(["'%C'" | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [safe_get(cn, ClientInfo)| Acc]);
replvar(["'%d'" | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [safe_get(dn, ClientInfo)| Acc]);
replvar([Param | Params], ClientInfo, Acc) ->
    replvar(Params, ClientInfo, [Param | Acc]).

safe_get(K, ClientInfo) ->
    bin(maps:get(K, ClientInfo, undefined)).

bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
bin(B) when is_binary(B) -> B;
bin(X) -> X.
