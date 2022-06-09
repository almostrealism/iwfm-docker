FROM ashesfall/iwfm-pest:latest

WORKDIR /

COPY sharedlib /sharedlib
COPY build.sh /build.sh

RUN /build.sh

COPY post.sh /post.sh

ENTRYPOINT /post.sh & /usr/sbin/apache2 -DFOREGROUND