FROM erlang as emqx-cass-prod
COPY --from=emqx-builder /emqx-rel/_build/emqx/rel/emqx /opt/emqx
COPY ./docker-entrypoint.sh start.sh /usr/bin/

#COPY /emqx-rel/_build/emqx/rel/emqx/ /opt/emqx/
#COPY ./emqx_cassandra_backend /emqx_cassandra_backend



RUN ln -s /opt/emqx/bin/* /usr/local/bin/
#RUN apk add --no-cache ncurses-libs openssl sudo

WORKDIR /opt/emqx

#RUN adduser -D -u 1000 emqx \
#    && echo "emqx ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers

#RUN chgrp -Rf emqx /opt/emqx && chmod -Rf g+w /opt/emqx \
#      && chown -Rf emqx /opt/emqx

RUN chmod +x /usr/bin/docker-entrypoint.sh /usr/bin/start.sh

RUN echo '{emqx_cassandra_backend, true}.' >> /opt/emqx/data/loaded_plugins

#USER emqx

RUN apt-get update && apt-get install -y openssl \
    bash \
    libuv1

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
