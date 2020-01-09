
-module(emqx_cassandra_backend).

-include_lib("emqx/include/emqx.hrl").

-define(APP, emqx_cassandra_backend).

-export([load/1, unload/0]).

%-export([on_client_connected/4, on_client_disconnected/3]).
%-export([on_client_subscribe/3, on_client_unsubscribe/3]).
%-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4,
%         on_session_terminated/3]).
%-export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

-export([on_message_publish/1]).

-define(LOG(Level, Format, Args), emqx_logger:Level("cassandra_backend: " ++ Format, Args)).

load(Env) ->
    %emqx:hook('client.connected', fun ?MODULE:on_client_connected/4, [Env]),
    %emqx:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
    %emqx:hook('client.subscribe', fun ?MODULE:on_client_subscribe/3, [Env]),
    %emqx:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/3, [Env]),
    %emqx:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
    %emqx:hook('session.resumed', fun ?MODULE:on_session_resumed/3, [Env]),
    %emqx:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
    %emqx:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
    %emqx:hook('session.terminated', fun ?MODULE:on_session_terminated/3, [Env]),
    emqx:hook('message.publish', fun ?MODULE:on_message_publish/1),
    %emqx:hook('message.delivered', fun ?MODULE:on_message_delivered/3, [Env]),
    %emqx:hook('message.acked', fun ?MODULE:on_message_acked/3, [Env]),
    %emqx:hook('message.dropped', fun ?MODULE:on_message_dropped/3, [Env]).
    ok = cassandra_cli:register_queries(),
    ok.
%
%
%
%%%--------------------------------------------------------------------
%%% Message publish
%%%--------------------------------------------------------------------
%
on_message_publish(Msg = #message{topic = <<"$SYS/", _/binary>>}) ->
    {ok, Msg};
on_message_publish(Msg) ->
    Res = cassandra_cli:publish(Msg),
    case Res of
        ok -> {ok, Msg};
        {error, Reason} ->
            % should pass along the message
            ?LOG(error, "Error saving message: ~p ~n", [Reason]),
            {ok, Msg}
    end.


%%--------------------------------------------------------------------
%% Client connected
%%--------------------------------------------------------------------
%{
%on_client_connected(#{client_id := ClientId, username := Username}, 0, _ConnInfo, _Env) ->
%    Params = [{action, client_connected},
%              {client_id, ClientId},
%              {username, Username},
%              {conn_ack, 0}],
%    send_http_request(Params),
%    ok;

%on_client_connected(#{}, _ConnAck, _ConnInfo, _Env) ->
%    ok.
%}
%%--------------------------------------------------------------------
%% Client disconnected
%%--------------------------------------------------------------------
%
%on_client_disconnected(#{}, auth_failure, _Env) ->
%    ok;
%on_client_disconnected(Client, {shutdown, Reason}, Env) when is_atom(Reason) ->
%    on_client_disconnected(Reason, Client, Env);
%on_client_disconnected(#{client_id := ClientId, username := Username}, Reason, _Env)
%    when is_atom(Reason) ->
%    Params = [{action, client_disconnected},
%              {client_id, ClientId},
%              {username, Username},
%              {reason, Reason}],
%    send_http_request(Params),
%    ok;
%on_client_disconnected(_, Reason, _Env) ->
%    ?LOG(error, "Client disconnected, cannot encode reason: ~p", [Reason]),
%    ok.
%
%%%--------------------------------------------------------------------
%%% Client subscribe
%%%--------------------------------------------------------------------
%
%on_client_subscribe(#{client_id := ClientId, username := Username}, TopicTable, {Filter}) ->
%    lists:foreach(fun({Topic, Opts}) ->
%      with_filter(
%        fun() ->
%          Params = [{action, client_subscribe},
%                    {client_id, ClientId},
%                    {username, Username},
%                    {topic, Topic},
%                    {opts, Opts}],
%          send_http_request(Params)
%        end, Topic, Filter)
%    end, TopicTable).
%
%%%--------------------------------------------------------------------
%%% Client unsubscribe
%%%--------------------------------------------------------------------
%
%on_client_unsubscribe(#{client_id := ClientId, username := Username}, TopicTable, {Filter}) ->
%    lists:foreach(fun({Topic, Opts}) ->
%      with_filter(
%        fun() ->
%          Params = [{action, client_unsubscribe},
%                    {client_id, ClientId},
%                    {username, Username},
%                    {topic, Topic},
%                    {opts, Opts}],
%          send_http_request(Params)
%        end, Topic, Filter)
%    end, TopicTable).
%
%%%--------------------------------------------------------------------
%%% Session created
%%%--------------------------------------------------------------------
%
%on_session_created(#{client_id := ClientId}, SessInfo, _Env) ->
%    Params = [{action, session_created},
%              {client_id, ClientId},
%              {username, proplists:get_value(username, SessInfo)}],
%    send_http_request(Params),
%    ok.
%
%%%--------------------------------------------------------------------
%%% Session subscribed
%%%--------------------------------------------------------------------
%
%on_session_subscribed(#{client_id := ClientId}, Topic, Opts, {Filter}) ->
%    with_filter(
%      fun() ->
%        Params = [{action, session_subscribed},
%                  {client_id, ClientId},
%                  {topic, Topic},
%                  {opts, Opts}],
%        send_http_request(Params)
%      end, Topic, Filter).
%
%%%--------------------------------------------------------------------
%%% Session unsubscribed
%%%--------------------------------------------------------------------
%
%on_session_unsubscribed(#{client_id := ClientId}, Topic, _Opts, {Filter}) ->
%    with_filter(
%      fun() ->
%        Params = [{action, session_unsubscribed},
%                  {client_id, ClientId},
%                  {topic, Topic}],
%        send_http_request(Params)
%      end, Topic, Filter).
%
%%%--------------------------------------------------------------------
%%% Session terminated
%%%--------------------------------------------------------------------
%
%on_session_terminated(Info, {shutdown, Reason}, Env) when is_atom(Reason) ->
%    on_session_terminated(Info, Reason, Env);
%on_session_terminated(#{client_id := ClientId}, Reason, _Env) when is_atom(Reason) ->
%    Params = [{action, session_terminated},
%              {client_id, ClientId},
%              {reason, Reason}],
%    send_http_request(Params),
%    ok;
%on_session_terminated(#{}, Reason, _Env) ->
%    ?LOG(error, "Session terminated, cannot encode the reason: ~p", [Reason]),
%    ok.
%
%%%--------------------------------------------------------------------
%%% Message delivered
%%%--------------------------------------------------------------------
%
%on_message_delivered(#{client_id := ClientId, username := Username}, Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
%  with_filter(
%    fun() ->
%      {FromClientId, FromUsername} = format_from(Message),
%      Params = [{action, message_delivered},
%                {client_id, ClientId},
%                {username, Username},
%                {from_client_id, FromClientId},
%                {from_username, FromUsername},
%                {topic, Message#message.topic},
%                {qos, Message#message.qos},
%                {retain, Retain},
%                {payload, Message#message.payload},
%                {ts, emqx_time:now_secs(Message#message.timestamp)}],
%      send_http_request(Params)
%    end, Topic, Filter).
%
%%%--------------------------------------------------------------------
%%% Message acked
%%%--------------------------------------------------------------------
%
%on_message_acked(#{client_id := ClientId}, Message = #message{topic = Topic, flags = #{retain := Retain}}, {Filter}) ->
%    with_filter(
%      fun() ->
%        {FromClientId, FromUsername} = format_from(Message),
%        Params = [{action, message_acked},
%                  {client_id, ClientId},
%                  {from_client_id, FromClientId},
%                  {from_username, FromUsername},
%                  {topic, Message#message.topic},
%                  {qos, Message#message.qos},
%                  {retain, Retain},
%                  {payload, Message#message.payload},
%                  {ts, emqx_time:now_secs(Message#message.timestamp)}],
%        send_http_request(Params)
%      end, Topic, Filter).
%
%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
%
%send_http_request(Params) ->
%    Params1 = jsx:encode(Params),
%    Url = application:get_env(?APP, url, "http://127.0.0.1"),
%    ?LOG(debug, "Url:~p, params:~s", [Url, Params1]),
%    case request_(post, {Url, [], "application/json", Params1}, [{timeout, 5000}], [], 0) of
%        {ok, _} -> ok;
%        {error, Reason} ->
%            ?LOG(error, "HTTP request error: ~p", [Reason]), ok %% TODO: return ok?
%    end.
%
%request_(Method, Req, HTTPOpts, Opts, Times) ->
%    %% Resend request, when TCP closed by remotely
%    case httpc:request(Method, Req, HTTPOpts, Opts) of
%        {error, socket_closed_remotely} when Times < 3 ->
%            timer:sleep(trunc(math:pow(10, Times))),
%            request_(Method, Req, HTTPOpts, Opts, Times+1);
%        Other -> Other
%    end.
%
%parse_rule(Rules) ->
%    parse_rule(Rules, []).
%parse_rule([], Acc) ->
%    lists:reverse(Acc);
%parse_rule([{Rule, Conf} | Rules], Acc) ->
%    Params = jsx:decode(iolist_to_binary(Conf)),
%    Action = proplists:get_value(<<"action">>, Params),
%    Filter = proplists:get_value(<<"topic">>, Params),
%    parse_rule(Rules, [{list_to_atom(Rule), Action, Filter} | Acc]).
%
%with_filter(Fun, _, undefined) ->
%    Fun(), ok;
%with_filter(Fun, Topic, Filter) ->
%    case emqx_topic:match(Topic, Filter) of
%        true  -> Fun(), ok;
%        false -> ok
%    end.
%
%with_filter(Fun, _, _, undefined) ->
%    Fun();
%with_filter(Fun, Msg, Topic, Filter) ->
%    case emqx_topic:match(Topic, Filter) of
%        true  -> Fun();
%        false -> {ok, Msg}
%    end.
%
%format_from(Message = #message{from = From}) when is_atom(From) ->
%    format_from(Message#message{from = a2b(From)});
%format_from(#message{from = ClientId, headers = #{username := Username}}) ->
%    {ClientId, Username}.
%
%a2b(A) -> erlang:atom_to_binary(A, utf8).
%
unload() ->
    emqx:unhook('message.publish', fun ?MODULE:on_message_publish/2).
