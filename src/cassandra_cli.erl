-module(cassandra_cli).

-include_lib("marina/include/marina.hrl").
-include_lib("emqx/include/emqx.hrl").
-include("emqx_cassandra_backend.hrl").

-export([save_msg/1, query/2]).

-define(INSERTQSTR, <<"INSERT INTO smartpot.pot_msgs(device_id, week, ts, flags, headers, msg_id, payload, qos, topic) values(?, ?, ?, ?, ?, ?, ?, ?, ?)">>).

week_of_year({Year, Week}) ->
	list_to_binary(integer_to_list(Year)++"-"++integer_to_list(Week)).

%% queries for auth and acl modules
query(Query, QOpts) ->
    ?LOG(info, " SENDING QUERY: ~p ~p ~n", [Query, QOpts]),
    marina:query(Query, QOpts).


save_msg(Msg) ->
	[DevId, Topic] = string:split(binary_to_list(Msg#message.topic), "/"),
	case uuid:is_valid(DevId) of
		true ->
    		case marina:query(?INSERTQSTR, #{values => 
    			[uuid:to_binary(binary_to_list(Msg#message.from)), week_of_year(calendar:iso_week_number()), 
    			uuid:timeuuid(Msg#message.timestamp), Msg#message.flags, Msg#message.headers, Msg#message.id, 
    			Msg#message.payload, binary:encode_unsigned(Msg#message.qos), list_to_binary(Topic)], timeout => 1000}) of
    		{ok, _} -> ok;
    		{error, Reason} -> {error, Reason}
    	end;
    	false -> ignore
    end.