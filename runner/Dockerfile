FROM ashesfall/iwfm-base

RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y wget zip dos2unix

WORKDIR /

# RUN wget https://data.cnra.ca.gov/dataset/31f3ddf8-752a-4b04-99e0-2a9f9139817a/resource/bc00cfa5-86ac-4e95-acda-6df1f3d85a73/download/c2vsimfg_version1.01.zip
COPY c2vsimfg_version1.01.zip /
RUN unzip c2vsimfg_version1.01.zip

COPY run.sh /run.sh

ENTRYPOINT /run.sh