-module(cassandra_cli).

-include_lib("erlcass/include/erlcass.hrl").
-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").

-export([publish/1, query/2,
					register_a_query/2,
					register_queries/0,
					week_of_year/1,
					connect/1,
					action_types/1,
					log_events/3,
					log_events/4]).


-define(INSERTQSTR, <<"INSERT INTO smartpot.thing_msgs(device_id, week, ts, msg_id, payload, topic) values(?, ?, ?, ?, ?, ?)">>).
-define(ACTIONQSTR, <<"INSERT INTO smartpot.thing_actions(device_id, week, action, ts) values(?, ?, ?, ?)">>).
-define(ACTIONREASONQSTR, <<"INSERT INTO smartpot.thing_actions(device_id, week, action, ts, reason) values(?, ?, ?, ?, ?)">>).
-define(SELECTQUEUED, <<"SELECT ts,topic,msg FROM smartpot.msg_to_things_queue WHERE device_id = ? AND queued = 1">>).


connect(Opts) ->
	Erlcass = proplists:get_value(cluster_options, Opts, []),
	application:set_env([{erlcass, Opts}]),
	ok = erlcass_cluster:create(),
	erlcass_cluster:set_log_level(5),

	erlcass_cluster:set_options(Erlcass),
	ok = erlcass_stm_cache:create(),
	erlcass_sup:start_link().

week_of_year({Year, Week}) ->
	list_to_binary(integer_to_list(Year)++"-"++integer_to_list(Week)).

%% queries for auth and acl modules
query(Query, QOpts) ->
    ?LOG(info, "DB QUERY: ~p ~p ~n", [Query, QOpts]),
		erlcass:execute(Query, QOpts).

register_queries() ->
		ok = erlcass:add_prepare_statement(insert_msg, {?INSERTQSTR, ?CASS_CONSISTENCY_ANY}),
		ok = erlcass:add_prepare_statement(insert_action, {?ACTIONQSTR, ?CASS_CONSISTENCY_ANY}),
		ok = erlcass:add_prepare_statement(insert_action_reason, {?ACTIONREASONQSTR, ?CASS_CONSISTENCY_ANY}),
	  ok = erlcass:add_prepare_statement(retrieve_queued_msgs, {?SELECTQUEUED, ?CASS_CONSISTENCY_ANY}).

register_a_query(QueryName, QueryStr) ->
	  ok = erlcass:add_prepare_statement(QueryName, QueryStr).

publish(Msg) ->
  <<MID:128>> = Msg#message.id,
	try
	  Res = query(insert_msg,
			[Msg#message.from, week_of_year(calendar:iso_week_number()),
			Msg#message.timestamp, list_to_binary(lists:flatten(io_lib:format("~32.16.0b", [MID]))),
			Msg#message.payload, Msg#message.topic]),
		case Res of
			ok-> ok;
			{error, Reason} ->
	          {error, Reason}
				end
	  catch
			_:_:Stacktrace ->
	      erlang:display(Stacktrace)
		end.

log_events(ClientId, Action, Ts) ->
	try
		Res = query(insert_action,
			[ClientId, week_of_year(calendar:iso_week_number()), Action,
			Ts]),
		case Res of
			ok-> ok;
			{error, Reason} ->
						{error, Reason}
		end
	catch
		_:_:Stacktrace ->
			erlang:display(Stacktrace)
	end.

log_events(ClientId, Action, R, Ts) when is_atom(R) ->
	log_events(ClientId, Action, atom_to_list(R), Ts);
log_events(ClientId, Action, R, Ts) ->
	try
		Res = query(insert_action_reason,
			[ClientId, week_of_year(calendar:iso_week_number()), Action,
			Ts, R]),
		case Res of
			ok-> ok;
			{error, Reason} ->
					{error, Reason}
		end
	catch
		_:_:Stacktrace ->
			erlang:display(Stacktrace)
	end.

action_types(Action) ->
	case Action of
		message_publish 		-> 0;
		client_subscribe 	-> 1;
		client_unsubscribe -> 2;
		client_connect 		-> 3;
		client_disconnect 	-> 4
	end.
