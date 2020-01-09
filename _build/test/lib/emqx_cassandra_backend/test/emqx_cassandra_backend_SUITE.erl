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
     [{emqx_cassandra_backend_acl, [sequence], [check_acl]},

      {emqx_cassandra_backend_log, [sequence], [test_message]},
      {emqx_cassandra_backend_auth, [sequence], [test_auth]}
     ].

init_per_suite(Config) ->
    emqx_ct_helpers:start_apps([emqx_cassandra_backend], fun set_special_configs/1),
    {ok, D1} = erlcass_uuid:gen_random(),
    {ok, D2} = erlcass_uuid:gen_random(),
    application:set_env(?APP, dev_ids, [D1, D2]),
    {ok, Hash} = application:get_env(?APP, password_hash),
    cassandra_cli:register_a_query(insert_dev, <<"insert into smartpot.mqtt_auth(id, password, is_superuser) values(?, ?, ?)">>),
    %% mock data to DB!
    PHash = emqx_passwd:hash(Hash, "passwd"),
    cassandra_cli:query(insert_dev, [D1, PHash, 0]),
    cassandra_cli:query(insert_dev, [D2, PHash, 1]),
    Config.

end_per_suite(_Config) ->
    %cassandra_cli:register_a_query(delete_user, <<"delete from smartpot.user where id = ?">>),
    %cassandra_cli:register_a_query(detele_dev, <<"delete from smartpot.mqtt_auth where id = ?">>),
    %cassandra_cli:query(delete_user, [P1]),
    %cassandra_cli:query(delete_dev, [D1]),
    %emqx_ct_helpers:stop_apps([emqx_cassandra_backend]).
    ok.

check_acl(_) ->
    {ok, [D1, D2]} = application:get_env(?APP, dev_ids),
    {ok, WebServiceId} = application:get_env(?APP, webservice_client_id),
    User1 = #{clientid => D1, username => D1, password => <<"passwd">>, zone => external},
    User2 = #{clientid => D2, username => D2, password => <<"passwd">>, zone => external},
    WebService = #{clientid => list_to_binary(WebServiceId), username => list_to_binary(WebServiceId),
        password => <<"dummy">>, zone => external},
    allow = emqx_access_control:check_acl(User1, subscribe, <<"t1/t2">>),
    allow = emqx_access_control:check_acl(User2, subscribe, <<"t1">>),
    deny  = emqx_access_control:check_acl(User1, publish, <<"t1">>),
    allow = emqx_access_control:check_acl(User2, subscribe, <<D2/binary,"/t1">>),
    deny  = emqx_access_control:check_acl(User2, subscribe, <<D1/binary,"/t1">>),
    allow = emqx_access_control:check_acl(WebService, subscribe, <<D1/binary,"/t1">>),
    allow = emqx_access_control:check_acl(WebService, publish, <<D1/binary,"/t1">>).


test_auth(_) ->
  {ok, [D1, D2]} = application:get_env(?APP, dev_ids),
  {ok, WebServiceId} = application:get_env(?APP, webservice_client_id),
  User1 = #{clientid => D1, username => D1, password => <<"passwd">>, zone => external},
  User2 = #{clientid => D2, username => D2, password => <<"passwd">>, zone => external},
  UserBadPass = #{clientid => D2, username => D2, password => <<"badpasswd">>, zone => external},
  {ok,#{is_superuser := false}} = emqx_access_control:authenticate(User1),
  {ok,#{is_superuser := true}} = emqx_access_control:authenticate(User2),
  {ok,#{is_superuser := true}} = emqx_access_control:authenticate(#{clientid =>
      list_to_binary(WebServiceId), username => list_to_binary(WebServiceId),
      password => <<"passwd">>, zone => external}),
  BadUser = #{clientid => <<"BADUSER">>, username => <<"BADUSER">>, password => <<"passwd">>, zone => external},
  {error,badarg} = emqx_access_control:authenticate(BadUser),
  {error,password_error} = emqx_access_control:authenticate(UserBadPass).


test_message(_) ->
  {ok, [D1, _]} = application:get_env(?APP, dev_ids),
  {ok, WebServiceId} = application:get_env(?APP, webservice_client_id),
  {ok, Client} = emqtt:start_link([{host, "localhost"},
                              {clientid, D1},
                              {username, D1},
                              {password, <<"passwd">>}]),
  {ok, WebService} = emqtt:start_link(
                              [{host, "localhost"},
                              {clientid, list_to_binary(WebServiceId)},
                              {username, list_to_binary(WebServiceId)},
                              {password, <<"123">>}]),
  {ok, _} = emqtt:connect(Client),
  {ok, _} = emqtt:connect(WebService),
  timer:sleep(10),
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
  emqtt:disconnect(Client),
  emqtt:disconnect(WebService).

set_special_configs(emqx) ->
    application:set_env(emqx, allow_anonymous, false),
    application:set_env(emqx, enable_acl_cache, false),
    application:set_env(emqx, plugins_loaded_file,
                          emqx_ct_helpers:deps_path(emqx, "deps/emqx/test/emqx_SUITE_data/loaded_plugins"));

set_special_configs(_App) ->
    ok.
