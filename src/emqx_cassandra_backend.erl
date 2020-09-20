
-module(emqx_cassandra_backend).

-include_lib("emqx/include/emqx.hrl").

-define(APP, emqx_cassandra_backend).

-export([load/1, unload/0]).

-export([on_client_connected/3, on_client_disconnected/4]).
-export([on_client_subscribe/4, on_client_unsubscribe/4]).
%-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4,
%         on_session_terminated/3]).
%-export([on_message_publish/2, on_message_delivered/3, on_message_acked/3]).

-export([on_message_publish/2]).

-define(LOG(Level, Format, Args), emqx_logger:Level("cassandra_backend: " ++ Format, Args)).

load(Env) ->

    % emqx:hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
    % emqx:hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
    % emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
    emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    % emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    % emqx:hook('client.check_acl',    {?MODULE, on_client_check_acl, [Env]}),
    emqx:hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
    emqx:hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
    % emqx:hook('session.created',     {?MODULE, on_session_created, [Env]}),
    % emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    % emqx:hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
    % emqx:hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
    % emqx:hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
    % emqx:hook('session.takeovered',  {?MODULE, on_session_takeovered, [Env]}),
    % emqx:hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    % emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    % emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
    % emqx:hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

    ok = cassandra_cli:register_queries().
%
%
%
%%%--------------------------------------------------------------------
%%% Message publish
%%%--------------------------------------------------------------------
%
on_message_publish(Msg = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Msg};
on_message_publish(Msg, _Env) ->
    Res = cassandra_cli:publish(Msg),
    case Res of
        ok -> {ok, Msg};
        {error, Reason} ->
            % should pass along the message
            ?LOG(error, "Error saving message: ~p ~p ~n", [Reason,Msg]),
            {ok, Msg}
    end.


%--------------------------------------------------------------------
% Client connected
%--------------------------------------------------------------------
on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
   case cassandra_cli:log_event(ClientId, cassandra_cli:action_types(client_connect), maps:get(connected_at, ConnInfo)*1000) of
     ok -> ok;
     {error, Reason} ->
         ?LOG(error, "Error logging action: ~p ~n", [Reason]),
         ok
   end.
%--------------------------------------------------------------------
% Client disconnected
%--------------------------------------------------------------------

on_client_disconnected(ClientInfo = #{clientid := ClientId}, Reason, ConnInfo, _Env) ->
  ct:print("times ~p ~p", [erlang:system_time(millisecond), maps:get(disconnected_at, ConnInfo)]),
   case cassandra_cli:log_event(ClientId, cassandra_cli:action_types(client_disconnect), Reason, maps:get(disconnected_at, ConnInfo)*1000) of
     ok -> ok;
     {error, Reas} ->
         ?LOG(error, "Error logging action: ~p ~n", [Reas]),
         ok
   end,
 ok.

%%--------------------------------------------------------------------
%% Client subscribe
%%--------------------------------------------------------------------
on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
   lists:foreach(fun({Topic, Opts}) ->
     case cassandra_cli:log_event(ClientId, cassandra_cli:action_types(client_subscribe),
       Topic, erlang:system_time(millisecond)) of
       ok -> ok;
       {error, Reas} ->
           ct:print("Error ~p ", [Reas]),
           ?LOG(error, "Error logging action: ~p ~n", [Reas]),
           ok
     end,
     Cols = [{<<"ts">>, timestamp},{<<"topic">>, text}, {<<"msg">>, text}],
     case Topic of
       <<"config">> ->
         case cassandra_cli:query(retrieve_queued_msgs, [ClientId]) of
           {ok, Cols, [[null]]} -> ok,
           {ok, Cols, [[]]}     -> ok,
           {ok, Cols, [[Rows]]} -> PublishQueuedMsgs(Rows)
         end
     end
   end, TopicFilters),
   {ok, TopicFilters}.

PublishQueuedMsgs(Rows) ->
  {R, Rest} = Rows,
  {Ts, Topic, Payload} = R,
  emqx:publish(emqx_message:make(Topic, Payload)),
  PublishQueuedMsgs(Rest),

PublishQueuedMsgs([]) ->
  ok.

%%--------------------------------------------------------------------
%% Client unsubscribe
%%--------------------------------------------------------------------

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
   lists:foreach(fun({Topic, Opts}) ->
     case cassandra_cli:log_event(ClientId, cassandra_cli:action_types(client_unsubscribe),
       Topic, erlang:system_time(millisecond)) of
       ok -> ok;
       {error, Reas} ->
           ct:print("Error ~p ", [Reas]),
           ?LOG(error, "Error logging action: ~p ~n", [Reas]),
           ok
     end
   end, TopicFilters),
   {ok, TopicFilters}.

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
with_filter(Fun, _, undefined) ->
   Fun(), ok;
with_filter(Fun, Topic, Filter) ->
   case emqx_topic:match(Topic, Filter) of
       true  -> Fun(), ok;
       false -> ok
   end.

with_filter(Fun, _, _, undefined) ->
   Fun();
with_filter(Fun, Msg, Topic, Filter) ->
   case emqx_topic:match(Topic, Filter) of
       true  -> Fun();
       false -> {ok, Msg}
   end.
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
