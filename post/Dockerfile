FROM ashesfall/iwfm-post:latest

WORKDIR /

COPY pestbin /pestbin
COPY C2VSimFG_01.pst /C2VSimFG_01.pst
COPY Texture2Par.tpl /Simulation/Texture2Par.tpl
COPY GWHMultiLayer.ins /Simulation/GWHMultiLayer.ins

COPY post.sh /post.sh

ENTRYPOINT /post.sh & /usr/sbin/apache2 -DFOREGROUND