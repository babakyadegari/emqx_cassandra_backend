-module(emqx_cassandra_backend_SUITE).

-compile(export_all).

%-define(PID, emqx_cassandra_backend).

-define(APP, emqx_cassandra_backend).

-include_lib("emqx/include/emqx.hrl").
-include_lib("eunit/include/eunit.hrl").
% -include_lib("common_test/include/ct.hrl").
-include_lib("emqx/include/emqx_mqtt.hrl").
%-include("emqx_mqtt.hrl")
all() ->
    [{group, emqx_cassandra_backend_acl},
     {group, emqx_cassandra_backend_auth},
     {group, emqx_cassandra_backend_log}
    ].

groups() ->
     [{emqx_cassandra_backend_acl, [sequence], [test_auth]},
      {emqx_cassandra_backend_auth, [sequence], [check_acl]},
      {emqx_cassandra_backend_log, [sequence], [test_message]}

     ].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx, emqx_cassandra_backend], fun set_special_configs/1),
    {ok, D1} = gen_random_mac(),
    {ok, D2} = gen_random_mac(),
    % ct:print("D1: ", [D1]),
    % ct:print("D2: ", [D2]),
    User1 = #{clientid => D1, username => D1, password => D1, zone => external},
    User2 = #{clientid => D2, username => D2, password => D2, zone => external},
    APIService = #{clientid => <<"APIService">>, username => <<"APIService">>,
        password => <<"password">>, zone => external},
    application:set_env(?APP, mock_users, [User1,User2,APIService]),
    {ok, Hash} = application:get_env(?APP, password_hash),
    ok = cassandra_cli:register_a_query(insert_dev, <<"insert into smartpot.mqtt_auth(id, password, is_superuser) values(?, ?, ?)">>),
    %% mock data to DB!
    ok = cassandra_cli:query(insert_dev, [D1, emqx_passwd:hash(Hash, D1), 0]),
    ok = cassandra_cli:query(insert_dev, [D2, emqx_passwd:hash(Hash, D2), 0]),
    ok = cassandra_cli:query(insert_dev, [<<"APIService">>, emqx_passwd:hash(Hash, "password"), 1]),
    Config.

end_per_suite(_Config) ->
    %cassandra_cli:register_a_query(delete_user, <<"delete from smartpot.user where id = ?">>),
    %cassandra_cli:register_a_query(detele_dev, <<"delete from smartpot.mqtt_auth where id = ?">>),
    %cassandra_cli:query(delete_user, [P1]),
    %cassandra_cli:query(delete_dev, [D1]),
    %emqx_ct_helpers:stop_apps([emqx_cassandra_backend]).
    ok.

test_auth(_) ->
  {ok, [U1, U2, APIService]} = application:get_env(?APP, mock_users),
  UserBadPass = #{clientid => maps:get(clientid, U1), username => maps:get(username, U1),
    password => <<"badpasswd">>, zone => external},
  {ok,#{is_superuser := false}} = emqx_access_control:authenticate(U1),
  {ok,#{is_superuser := false}} = emqx_access_control:authenticate(U2),
  {ok,#{is_superuser := true}} = emqx_access_control:authenticate(APIService),
  BadUser = #{clientid => <<"BADUSER">>, username => <<"BADUSER">>, password => <<"passwd">>, zone => external},
  {error,not_authorized} = emqx_access_control:authenticate(BadUser),
  {error,password_error} = emqx_access_control:authenticate(UserBadPass).


check_acl(_) ->
    {ok, [U1, U2, APIService]} = application:get_env(?APP, mock_users),
    % User1 = #{clientid => D1, username => D1, password => D1, zone => external},
    % User2 = #{clientid => D2, username => D2, password => D2, zone => external},
    % WebService = #{clientid => <<"APIService">>, username => <<"APIService">>,
        % password => <<"password">>, zone => external},

  allow = emqx_access_control:check_acl(U1, subscribe, <<"t1/t2">>),
  allow = emqx_access_control:check_acl(U2, subscribe, <<"some/general/topic">>),
  deny  = emqx_access_control:check_acl(U1, publish, <<"some/general/topic">>),
  D1 = maps:get(clientid, U1),
  D2 = maps:get(clientid, U2),
  allow = emqx_access_control:check_acl(U2, subscribe, <<D2/binary,"/t1">>),
  deny  = emqx_access_control:check_acl(U2, subscribe, <<D1/binary,"/t1">>),
  {ok, #{is_superuser := IsSuperUser}} = emqx_access_control:authenticate(APIService),

  APIServiceAuth = APIService#{is_superuser => IsSuperUser},
  allow = emqx_access_control:check_acl(APIServiceAuth, subscribe, <<D1/binary,"/t1">>),
  allow = emqx_access_control:check_acl(APIServiceAuth, publish, <<D1/binary,"/t1">>),
  allow = emqx_access_control:check_acl(APIServiceAuth, subscribe, <<"some/general/topic">>),
  allow = emqx_access_control:check_acl(APIServiceAuth, publish, <<"some/general/topic">>).


test_message(_) ->
  % test_save_msg(),
  {ok, [U1, _, APIService]} = application:get_env(?APP, mock_users),
  {ok, Client} = emqtt:start_link(U1#{host => "localhost"}),
  {ok, WebService} = emqtt:start_link(APIService#{host => "localhost", is_superuser => false}),
  {ok, _} = emqtt:connect(Client),
  {ok, _} = emqtt:connect(WebService),
  timer:sleep(10),
  D1 = maps:get(clientid, U1),
  emqtt:subscribe(Client, <<D1/binary,"/control">>, qos2),
  timer:sleep(1000),
  emqtt:publish(WebService, <<D1/binary,"/control">>, <<"Payload">>, qos2),
  timer:sleep(1000),
  receive
      {publish, #{payload := Payload}} ->
          ?assertEqual(<<"Payload">>, Payload)
  after
      1000 ->
          ct:fail({receive_timeout, <<"Payload">>}),
          ok
  end,
  emqtt:unsubscribe(Client, <<D1/binary,"/control">>),
  emqtt:publish(Client, <<D1/binary,"/sensor/1">>, <<"data">>, qos2),
  timer:sleep(1000),
  emqtt:disconnect(Client),
  emqtt:disconnect(WebService).

test_save_msg() ->
  Msg = {message,<<0,5,155,131,207,145,92,85,180,75,0,0,3,246,0,1>>,
                   2,<<"4132686A-21E8-11EA-B6DA-1B3D8B8235C1">>,
                   #{dup => false,retain => false},
                   #{peerhost => {127,0,0,1},
                     proto_ver => 4,protocol => mqtt,
                     username => <<"4132686A-21E8-11EA-B6DA-1B3D8B8235C1">>},
                   <<"1587711e-e321-48a7-92ac-ddf191361598/control">>,
                   <<"Payload">>,1578365308984},
  <<MID:128>> = Msg#message.id,
  A = [Msg#message.from, cassandra_cli:week_of_year(calendar:iso_week_number()),
    erlcass_uuid:gen_from_ts(Msg#message.timestamp), <<"Msg#message.flags">>,
    <<"Msg#message.headers">>, list_to_binary(lists:flatten(io_lib:format("~32.16.0b", [MID]))),
    Msg#message.payload, Msg#message.qos, Msg#message.topic],
  ct:print("A: ~p", [A]),
  {ok, Ts} = erlcass_uuid:gen_from_ts(Msg#message.timestamp),
  Res = cassandra_cli:query(insert_msg,
    [Msg#message.from, cassandra_cli:week_of_year(calendar:iso_week_number()),
    Ts, <<"Msg#message.flags">>,
    <<"Msg#message.headers">>, list_to_binary(lists:flatten(io_lib:format("~32.16.0b", [MID]))),
    Msg#message.payload, Msg#message.qos, Msg#message.topic]),
  ct:print("Res ~p", [Res]),
  ok.

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
    emqx_ct_helpers:deps_path(emqx, "deps/emqx/test/emqx_SUITE_data/loaded_plugins"));

set_special_configs(_App) ->
    ok.

gen_random_mac() ->
  {ok, list_to_binary(string:join([integer_to_list(rand:uniform(255), 16) || X <- [1,2,3,4,5,6]], ":"))}.
