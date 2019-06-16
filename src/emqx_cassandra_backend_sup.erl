-module(emqx_cassandra_backend_sup).

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
    {ok, {{one_for_all, 0, 1}, []}}.
    
% init([]) ->
%     {ok, {{one_for_all, 0, 1}, 
%     	[{serv,
% 			{marina_sup, start_link, []},
% 			permanent,
% 			5000, % Shutdown time
% 			supervisor,
% 				[]}]}}.
