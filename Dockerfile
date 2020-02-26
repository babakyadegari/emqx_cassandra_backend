ARG BUILD_FROM=erlang:22.1-alpine
ARG RUN_FROM=alpine:3.10

FROM ${BUILD_FROM} AS emqx-builder
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
    cmake \
    sed \
    bash \
    libc-dev \
    vim


RUN mkdir -p cpp-driver \
  && git clone https://github.com/datastax/cpp-driver.git cpp-driver \
  && cd cpp-driver \
  && git checkout 3a5d3a50299fafc9c02c2dd42447e617593a19e9

WORKDIR cpp-driver/build

RUN cmake .. -DCASS_BUILD_STATIC=ON -DCASS_BUILD_SHARED=OFF -DCMAKE_BUILD_TYPE=RELEASE -DCMAKE_CXX_FLAGS=-fPIC -DCMAKE_C_FLAGS=-fPIC
RUN make -j 8

RUN mkdir -p /emqx-rel
RUN git clone https://github.com/emqx/emqx-rel.git /emqx-rel
ARG EMQX_DEPS_DEFAULT_VSN=develop
ARG EMQX_NAME=emqx


#BUILD ERLCASS STUFF
RUN git clone --branch 'v4.0.0' https://github.com/silviucpp/erlcass.git /emqx-rel/_build/emqx/lib/erlcass/

#RUN echo '{base_dir, "/emqx-rel/_build"}.' >> rebar.config
RUN rm /emqx-rel/_build/emqx/lib/erlcass/src/erlcass_app.erl
RUN sed -i '/{mod,/d' /emqx-rel/_build/emqx/lib/erlcass/src/erlcass.app.src

RUN mkdir -p /emqx-rel/_build/emqx/lib/erlcass/_build/deps/
RUN cp -r /cpp-driver /emqx-rel/_build/emqx/lib/erlcass/_build/deps/
WORKDIR /emqx-rel

COPY ./rebar.patch /emqx-rel
COPY ./makefile.patch /emqx-rel
RUN patch < ./rebar.patch
RUN patch < ./makefile.patch


RUN make

RUN cp -r /emqx-rel/_build/emqx/ /emqx-rel/_build/emqx+test/
RUN cp -r /emqx-rel/_build/emqx/lib/ /emqx-rel/_build/default/

VOLUME ["/emqx-rel/_build/emqx/lib/emqx_cassandra_backend"]
        #"/emqx-rel/_build/emqx+test/lib/emqx_cassandra_backend", \
        #"/emqx-rel/_build/default/lib/emqx_cassandra_backend"]

CMD ["/emqx-rel/_build/emqx/rel/emqx/bin/emqx", "start"]
CMD ["tail", "-f", "/dev/null"]



ENTRYPOINT ["/emqx_cassandra_backend/build.sh"]

CMD ["tail", "-f", "/dev/null"]


FROM erlang:22.1-alpine as emqx-cass-prod
COPY --from=emqx-builder /emqx-rel/_build/emqx/rel/emqx /opt/emqx
COPY ./docker-entrypoint.sh start.sh /usr/bin/

#COPY /emqx-rel/_build/emqx/rel/emqx/ /opt/emqx/
#COPY ./emqx_cassandra_backend /emqx_cassandra_backend



RUN ln -s /opt/emqx/bin/* /usr/local/bin/
RUN apk add --no-cache ncurses-libs openssl sudo

WORKDIR /opt/emqx

#RUN adduser -D -u 1000 emqx \
#    && echo "emqx ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers

#RUN chgrp -Rf emqx /opt/emqx && chmod -Rf g+w /opt/emqx \
#      && chown -Rf emqx /opt/emqx

RUN chmod +x /usr/bin/docker-entrypoint.sh /usr/bin/start.sh

RUN echo '{emqx_cassandra_backend, true}.' >> /opt/emqx/data/loaded_plugins

#USER emqx

RUN apk add openssl \
    bash \
    libuv-dev \
    libstdc++

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

VOLUME ["/opt/emqx/log", "/opt/emqx/data", "/opt/emqx/lib", "/opt/emqx/etc"]

ENTRYPOINT ["/usr/bin/docker-entrypoint.sh"]
CMD ["/usr/bin/start.sh"]
