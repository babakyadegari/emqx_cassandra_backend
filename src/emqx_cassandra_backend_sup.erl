-module(emqx_cassandra_backend_sup).
-include("emqx_cassandra_backend.hrl").

-export([
    start_link/0
]).

-behaviour(supervisor).

-export([
    init/1
]).

%% public
-spec start_link() -> {ok, pid()}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, Opts} = application:get_env(?APP, erlcass),
    {ok, {{one_for_one, 10, 100}, [#{id => cassandra_cli,
                    start => {cassandra_cli, connect, [Opts]},
                    restart => permanent,
                    shutdown => brutal_kill,
                    type => worker,
                    modules => [cassandra_cli]}]}}.
                    
    %PoolSpec = ecpool:pool_spec(?APP, ?APP, cassandra_cli, Opts),
    %{ok, {{one_for_one, 10, 100}, [PoolSpec]}}.

%{ok, Opts} = application:get_env(emqx_cassandra_backend, erlcass).
%application:set_env([{erlcass, Opts}]).
