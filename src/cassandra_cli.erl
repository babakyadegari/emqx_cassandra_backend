-module(cassandra_cli).

-include_lib("erlcass/include/erlcass.hrl").
-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").

-export([publish/1, query/2, register_a_query/2, register_queries/0, week_of_year/1]).

-define(PUBLISH, 0).

-define(INSERTQSTR, <<"INSERT INTO smartpot.thing_msgs(device_id, week, action, ts, flags, headers, msg_id, payload, qos, topic) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>).

week_of_year({Year, Week}) ->
	list_to_binary(integer_to_list(Year)++"-"++integer_to_list(Week)).

%% queries for auth and acl modules
query(Query, QOpts) ->
    %?LOG(info, "DB QUERY: ~p ~p ~n", [Query, QOpts]).
		erlcass:execute(Query, QOpts).

register_queries() ->
		ok = erlcass:add_prepare_statement(insert_msg, {?INSERTQSTR, ?CASS_CONSISTENCY_ANY}).

register_a_query(QueryName, QueryStr) ->
	  ok = erlcass:add_prepare_statement(QueryName, QueryStr).

publish(Msg) ->
  <<MID:128>> = Msg#message.id,
  {ok, Ts} = erlcass_uuid:gen_from_ts(Msg#message.timestamp),
  Res = query(insert_msg,
		[Msg#message.from, week_of_year(calendar:iso_week_number()), ?PUBLISH,
		Ts, <<"Msg#message.flags">>,
		<<"Msg#message.headers">>, list_to_binary(lists:flatten(io_lib:format("~32.16.0b", [MID]))),
		Msg#message.payload, Msg#message.qos, Msg#message.topic]),
	case Res of
		{ok, _} -> ok;
		{error, Reason} ->
					ct:print("Error ~p ", [Reason]),
          {error, Reason}

  end.
