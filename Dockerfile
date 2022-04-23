FROM intel/oneapi-hpckit
WORKDIR /
COPY h5fortran h5fortran
COPY zlib-ng-2.0.6.tar.gz /build/hdf5/ZLIB-prefix/src/2.0.6.tar.gz
COPY hdf5-hdf5-1_12_1.tar.gz /build/hdf5/HDF5-prefix/src/hdf5-1_12_1.tar.gz
COPY heclib heclib
COPY iwfm iwfm
COPY build.sh /build.sh

RUN /build.sh