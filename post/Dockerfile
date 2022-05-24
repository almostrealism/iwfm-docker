FROM ashesfall/iwfm-post:latest

WORKDIR /

COPY runner/allsubs.dat /Simulation/allsubs.dat
COPY runner/sub10.dat /Simulation/sub10.dat
COPY runner/sub11sub12sub13.dat /Simulation/sub11sub12sub13.dat
COPY runner/sub14sub15.dat /Simulation/sub14sub15.dat
COPY runner/sub16sub17sub18.dat /Simulation/sub16sub17sub18.dat
COPY runner/sub19sub20sub21.dat /Simulation/sub19sub20sub21.dat
COPY runner/sub1sub2.dat /Simulation/sub1sub2.dat
COPY runner/sub3sub4.dat /Simulation/sub3sub4.dat
COPY runner/sub5.dat /Simulation/sub5.dat
COPY runner/sub6sub7.dat /Simulation/sub6sub7.dat
COPY runner/sub8sub9.dat /Simulation/sub8sub9.dat
COPY runner/subCC.dat /Simulation/subCC.dat

COPY post.sh post.sh

ENTRYPOINT /post.sh