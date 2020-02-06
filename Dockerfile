ARG BUILD_FROM=alpine:3.10
ARG RUN_FROM=alpine:3.10

FROM ${BUILD_FROM} AS cass-cpp
LABEL builder=true
#ENV CPP_DIR /cpp-driver

RUN apk add git \
    curl \
    gcc \
    g++ \
    make \
    perl \
    ncurses-dev \
    openssl-dev \
    coreutils \
    bsd-compat-headers \
    libuv-dev \
    zlib \
    cmake

RUN mkdir -p cpp-driver \
  && git clone https://github.com/datastax/cpp-driver.git cpp-driver \
  && cd cpp-driver \
  && git checkout 3a5d3a50299fafc9c02c2dd42447e617593a19e9

WORKDIR cpp-driver/build

RUN cmake .. -DCASS_BUILD_STATIC=ON -DCASS_BUILD_SHARED=OFF -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_C_FLAGS=-fPIC \
  && make -j 4

RUN rm -rf cpp-driver/.git



FROM erlang:22.1-alpine as emqx-base

RUN apk add git \
    curl \
    gcc \
    g++ \
    make \
    ncurses-dev \
    openssl-dev \
    coreutils \
    bsd-compat-headers \
    bash \
    libuv-dev \
    zlib \
    libc-dev \
    sed


#COPY . /emqx_rel

RUN mkdir -p /emqx-rel
RUN git clone https://github.com/emqx/emqx-rel.git /emqx-rel
ARG EMQX_DEPS_DEFAULT_VSN=develop
ARG EMQX_NAME=emqx

RUN cd /emqx-rel && make

#BUILD ERLCASS STUFF
RUN git clone --branch 'v4.0.0' https://github.com/silviucpp/erlcass.git /erlcass
WORKDIR /erlcass

RUN echo '{base_dir, "/emqx-rel/_build"}.' >> rebar.config

COPY --from=cass-cpp /cpp-driver /erlcass/_build/deps/cpp-driver

RUN rebar3 as emqx compile

#COPY deploy/docker/docker-entrypoint.sh deploy/docker/start.sh /usr/bin/
#COPY --from=builder //_build/emqx*/rel/emqx /opt/emqx

#RUN ln -s /opt/emqx/bin/* /usr/local/bin/
#RUN apk add --no-cache ncurses-libs openssl sudo

#WORKDIR /opt/emqx

#RUN adduser -D -u 1000 emqx \
#    && echo "emqx ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers

#RUN chgrp -Rf emqx /opt/emqx && chmod -Rf g+w /opt/emqx \
#      && chown -Rf emqx /opt/emqx

#USER emqx

#VOLUME ["/opt/emqx/log", "/opt/emqx/data", "/opt/emqx/lib", "/opt/emqx/etc"]

# emqx will occupy these port:
# - 1883 port for MQTT
# - 8080 for mgmt API
# - 8083 for WebSocket/HTTP
# - 8084 for WSS/HTTPS
# - 8883 port for MQTT(SSL)
# - 11883 port for internal MQTT/TCP
# - 18083 for dashboard
# - 4369 for port mapping
# - 5369 for gen_rpc port mapping
# - 6369 for distributed node
EXPOSE 1883 8080 8083 8084 8883 11883 18083 4369 5369 6369

#ENTRYPOINT ["/usr/bin/docker-entrypoint.sh"]
#CMD ["/usr/bin/start.sh"]

FROM erlang:22.1-alpine as emqx-cass-dev
VOLUME ["/emqx_cassandra_backend"]
COPY --from=emqx-base /emqx-rel /emqx-rel
COPY --from=emqx-base /erlcass /emqx-rel/_build/default/lib/erlcass/
#COPY --from=emqx-base /erlcass/_build/deps/ /emqx-rel/_build/default/lib/deps/erlcass/_build/deps

RUN apk add git \
    curl \
    gcc \
    g++ \
    make \
    bsd-compat-headers \
    zlib \
    libuv-dev \
    zlib \
    libc-dev \
    bash

ENTRYPOINT ["/emqx_cassandra_backend/build.sh"]

CMD ["tail", "-f", "/dev/null"]
