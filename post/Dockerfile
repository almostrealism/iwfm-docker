FROM ashesfall/iwfm-runner-final

WORKDIR /

COPY iwfm2obs build/iwfm2obs
COPY mlt build/mlt
COPY GW_Obs.smp /Simulation/GW_Obs.smp
COPY iwfm2obs_2015.in /Simulation/iwfm2obs_2015.in
COPY MultiLayerTarget.in /Simulation/MultiLayerTarget.in
COPY ObservationConstruction.txt /Simulation/ObservationConstruction.txt
COPY settings.fig /Simulation/settings.fig
COPY STR_Obs.smp /Simulation/STR_Obs.smp

COPY post.sh post.sh

ENTRYPOINT /post.sh