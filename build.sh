#!/bin/bash


# cd /
#
# ln -s /opt/emqx/bin/* /usr/local/bin
#
# cp /emqx_cassandra_backend/docker-entrypoint.sh /emqx_cassandra_backend/start.sh /usr/bin/
#
# chmod +x /usr/bin/docker-entrypoint.sh /usr/bin/start.sh

cp -r /emqx_cassandra_backend /emqx-rel/_build/emqx/lib/

cd /emqx-rel

make


#rebar3 as emqx ct
