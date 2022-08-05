FROM intel/oneapi-hpckit:2022.1.2-devel-ubuntu18.04

WORKDIR /

RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y wget zip dos2unix python3
RUN apt-get install -y apache2

RUN pip3 install pandas
RUN pip3 install matplotlib
RUN pip3 install descartes
RUN pip3 install pyshp
RUN pip3 install awswrangler

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

RUN /build.sh

RUN wget https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz
RUN tar -zxvf openjdk-17.0.2_linux-x64_bin.tar.gz
RUN mv jdk-17.0.2 /opt

ENV JAVA_HOME=/opt/jdk-17.0.2

COPY tools/ar-flowtree-shaded-0.13.jar /flowtree-shaded.jar

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

COPY tools/pestbin /pestbin

COPY tools/pest/Texture2Par.tpl /Simulation/Texture2Par.tpl
COPY tools/pest/GWHMultiLayer.ins /Simulation/GWHMultiLayer.ins

COPY runner/init.sh /init.sh
COPY runner/run.sh /run.sh
COPY runner/run_model.sh /run_model.sh
COPY runner/run_simulation.sh /run_simulation.sh
COPY post/post.sh /post.sh

COPY postprocessing /scripts

COPY tools/apache2/autoindex.conf /etc/apache2/mods-available/autoindex.conf
ENV APACHE_RUN_DIR=/
ENV APACHE_RUN_USER=www-data
ENV APACHE_RUN_GROUP=www-data
ENV APACHE_LOG_DIR=/var/www

ENTRYPOINT /init.sh ; /post.sh & /usr/sbin/apache2 -DFOREGROUND