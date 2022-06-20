FROM intel/oneapi-hpckit

WORKDIR /

RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y wget zip dos2unix python3
RUN apt-get install -y apache2

RUN pip3 install pandas
RUN pip3 install matplotlib
RUN pip3 install descartes
RUN pip3 install pyshp

COPY szip szip
COPY h5fortran h5fortran
COPY zlib-ng-2.0.6.tar.gz /build/hdf5/ZLIB-prefix/src/2.0.6.tar.gz
COPY hdf5-hdf5-1_12_1.tar.gz /build/hdf5/HDF5-prefix/src/hdf5-1_12_1.tar.gz
COPY heclib heclib
COPY iwfm iwfm
COPY iwfm2obs iwfm2obs
COPY srs srs
COPY mlt mlt
COPY mfUSGLib mfUSGLib
COPY t2p t2p
COPY cth cth
COPY build.sh /build.sh

RUN /build.sh

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

COPY pestbin /pestbin
COPY pest/C2VSimFG_01.pst /C2VSimFG_01.pst
COPY pest/Texture2Par.tpl /Simulation/Texture2Par.tpl
# COPY pest/*.ins /Simulation/
COPY pest/GWHMultiLayer.ins /Simulation/GWHMultiLayer.ins

COPY init.sh /init.sh
COPY run.sh /run.sh
COPY run_model.sh /run_model.sh
COPY run_simulation.sh /run_simulation.sh

COPY post/scripts /scripts

ENV APACHE_RUN_DIR=/
ENV APACHE_RUN_USER=www-data
ENV APACHE_RUN_GROUP=www-data
ENV APACHE_LOG_DIR=/var/www

ENTRYPOINT /init.sh