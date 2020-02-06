#!/bin/bash

cd /emqx_cassandra_backend

rebar3 as emqx ct

tail -f /dev/null
