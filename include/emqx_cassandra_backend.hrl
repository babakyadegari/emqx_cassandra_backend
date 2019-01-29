-define(APP, emqx_cassandra_backend).
-define(KEYSPACE, application:get_var(?APP, "keyspace")).
-define(USERTABLE, application:get_var(?APP, "usertable")).
-define(MQTT_AUTH_TABLE, application:get_var(?APP, "mqtt_auth")).

-define(LOG(Level, Format, Args), emqx_logger:Level("cassandra_backend: " ++ Format, Args)).
