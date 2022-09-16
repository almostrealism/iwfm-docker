FROM intel/oneapi-hpckit:2022.1.2-devel-ubuntu18.04

WORKDIR /

# Install dependencies
RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y wget zip dos2unix python3

RUN apt-get update
RUN apt-get install -y --fix-missing libxml2-dev
RUN apt-get install -y sqlite3 libsqlite3-dev
RUN apt-get install -y libapr1-dev libaprutil1-dev
RUN apt-get install -y libzip-dev

# Install Python packages
RUN pip3 install pandas
RUN pip3 install geopandas
RUN pip3 install matplotlib
RUN pip3 install descartes
RUN pip3 install pyshp
RUN pip3 install awswrangler

# Include all the tools
COPY tools/szip szip
COPY tools/h5fortran h5fortran
COPY tools/zlib-ng-2.0.6.tar.gz /build/hdf5/ZLIB-prefix/src/2.0.6.tar.gz
COPY tools/hdf5-hdf5-1_12_1.tar.gz /build/hdf5/HDF5-prefix/src/hdf5-1_12_1.tar.gz
COPY tools/heclib heclib
COPY tools/iwfm iwfm
COPY tools/iwfm2obs iwfm2obs
COPY tools/srs srs
COPY tools/mlt mlt
COPY tools/mfUSGLib mfUSGLib
COPY tools/t2p t2p
COPY tools/cth cth
COPY tools/build.sh /build.sh

# Build all the tools
RUN /build.sh

# Java
RUN wget https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz
RUN tar -zxvf openjdk-17.0.2_linux-x64_bin.tar.gz
RUN mv jdk-17.0.2 /opt

ENV JAVA_HOME=/opt/jdk-17.0.2

COPY tools/ar-flowtree-shaded-0.13.jar /flowtree-shaded.jar

# Install Apache2
RUN wget https://dlcdn.apache.org/httpd/httpd-2.4.54.tar.gz
RUN tar -zxvf httpd-2.4.54.tar.gz
RUN cd httpd-2.4.54 ; ./configure --enable-so ; make ; make install

# Configure the webserver
COPY tools/apache2/autoindex.conf /usr/local/apache2/conf/extra/httpd-autoindex.conf
COPY tools/apache2/httpd.conf /usr/local/apache2/conf/httpd.conf
RUN touch /index.html

# Install PHP
RUN wget https://www.php.net/distributions/php-8.1.9.tar.gz
RUN tar -zxvf php-8.1.9.tar.gz
RUN cd php-8.1.9 ; ./configure --with-apxs2=/usr/local/apache2/bin/apxs --with-zip; make ; make install ; cp php.ini-development /usr/local/lib/php.ini

# Install DirectoryLister
RUN wget https://github.com/DirectoryLister/DirectoryLister/releases/download/3.12.0/DirectoryLister-3.12.0.tar.gz
RUN tar -zxvf DirectoryLister-3.12.0.tar.gz
RUN chmod -R 777 /app/

RUN apt-get install -y nodejs-dev node-gyp libssl1.0-dev
RUN apt-get install -y npm
RUN node --version
RUN npm install @terraformer/arcgis

COPY runner/GW_Obs.smp /Simulation/GW_Obs.smp
COPY runner/iwfm2obs_2015.in /Simulation/iwfm2obs_2015.in
COPY runner/MultiLayerTarget.in /Simulation/MultiLayerTarget.in
COPY runner/ObservationConstruction.txt /Simulation/ObservationConstruction.txt
COPY runner/settings.fig /Simulation/settings.fig
COPY runner/STR_Obs.smp /Simulation/STR_Obs.smp

COPY runner/*.dat /Simulation/
COPY runner/Texture2Par.in /Simulation/Texture2Par.in
COPY runner/zones.txt /Simulation/zones.txt

COPY runner/CalcTypeHyd_All_sim.in /Simulation/CalcTypeHyd_All_sim.in
COPY runner/CalcTypeHyd_Sub1Sub2_sim.in /Simulation/CalcTypeHyd_Sub1Sub2_sim.in
COPY runner/CalcTypeHyd_Sub3Sub4_sim.in /Simulation/CalcTypeHyd_Sub3Sub4_sim.in
COPY runner/CalcTypeHyd_Sub5_sim.in /Simulation/CalcTypeHyd_Sub5_sim.in
COPY runner/CalcTypeHyd_Sub6Sub7_sim.in /Simulation/CalcTypeHyd_Sub6Sub7_sim.in
COPY runner/CalcTypeHyd_Sub19Sub20Sub21_sim.in /Simulation/CalcTypeHyd_Sub19Sub20Sub21_sim.in
COPY runner/CalcTypeHyd_Sub14Sub15_sim.in /Simulation/CalcTypeHyd_Sub14Sub15_sim.in
COPY runner/CalcTypeHyd_Sub16Sub17Sub18_sim.in /Simulation/CalcTypeHyd_Sub16Sub17Sub18_sim.in
COPY runner/CalcTypeHyd_Sub11Sub12Sub13_sim.in /Simulation/CalcTypeHyd_Sub11Sub12Sub13_sim.in
COPY runner/CalcTypeHyd_Sub10_sim.in /Simulation/CalcTypeHyd_Sub10_sim.in
COPY runner/CalcTypeHyd_Sub8Sub9_sim.in /Simulation/CalcTypeHyd_Sub8Sub9_sim.in
COPY runner/CalcTypeHyd_CC_sim.in /Simulation/CalcTypeHyd_CC_sim.in

# PEST++ Binaries
COPY tools/pestbin /pestbin

# Files for PEST++ (necessary?)
COPY tools/pest/Texture2Par.tpl /Simulation/Texture2Par.tpl
COPY tools/pest/GWHMultiLayer.ins /Simulation/GWHMultiLayer.ins

# Scripts for running parts of the process
COPY runner/init.sh /init.sh
COPY runner/run.sh /run.sh
COPY runner/run_model.sh /run_model.sh
COPY runner/run_simulation.sh /run_simulation.sh
COPY post/post.sh /post.sh

# Post processing with Python
COPY postprocessing /scripts
RUN cd /scripts ; pip install -e pywfm ; cd /

ENTRYPOINT /init.sh & /usr/local/apache2/bin/httpd -DFOREGROUND