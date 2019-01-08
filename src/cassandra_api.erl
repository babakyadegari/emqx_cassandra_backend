-module(cassandra_api).

-include_lib("marina/include/marina.hrl").
-include_lib("emqx/include/emqx.hrl").

-export([save_msg/1]).


save_msg(Msg) ->
    marina:query(<<"INSERT INTO test.msg() values(0, '12', 0, '34')">>, #{timeout => 1000}),
    ok.

send_query(query) -> 
    marina:query(<<"SELECT * FROM test.msg LIMIT 1;">>, #{timeout => 1000}).
