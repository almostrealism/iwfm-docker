FROM intel/oneapi-hpckit:2022.1.2-devel-ubuntu18.04 as build-env

WORKDIR /

# Install dependencies
RUN apt-get update
RUN apt-get install -y curl wget zip

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

FROM php:7.4-apache

# Copy necessary fields from build environment
COPY --from=build-env /build /build
COPY --from=build-env /libs /libs
COPY --from=build-env /opt/intel/oneapi/compiler /opt/intel/oneapi/compiler
COPY --from=build-env /opt/intel/oneapi/intelpython /opt/intel/oneapi/intelpython

ENV LD_LIBRARY_PATH=/opt/intel/oneapi/compiler/2022.0.2/linux/compiler/lib/intel64_lin

# Install packages
RUN apt-get update
RUN apt-get install -y curl
RUN apt-get install -y wget zip dos2unix python3 python3-pip
RUN DEBIAN_FRONTEND=noninteractive apt-get install -y --fix-missing libxml2-dev
RUN apt-get install -y --fix-missing libxml2-dev
RUN apt-get install -y sqlite3 libsqlite3-dev
RUN apt-get install -y libapr1-dev libaprutil1-dev
RUN apt-get install -y libzip-dev
RUN apt-get install -y pcre2-utils

WORKDIR /

# Install DirectoryLister
RUN wget https://github.com/DirectoryLister/DirectoryLister/releases/download/3.12.0/DirectoryLister-3.12.0.tar.gz
RUN tar -zxvf DirectoryLister-3.12.0.tar.gz
RUN chmod -R 777 /app/

# Install arcgis
#RUN apt-get install -y nodejs-dev node-gyp libssl1.0-dev
#RUN apt-get install -y npm
#RUN node --version
#RUN npm install @terraformer/arcgis

# Python
COPY postprocessing /scripts

# Upgrade pip
RUN python3 -m pip install --upgrade pip

# Install Python packages
RUN cd /scripts ; pip install -r requirements.txt ; cd /

# Install Java
RUN wget https://download.java.net/java/GA/jdk17.0.2/dfd4a8d0985749f896bed50d7138ee7f/8/GPL/openjdk-17.0.2_linux-x64_bin.tar.gz
RUN tar -zxvf openjdk-17.0.2_linux-x64_bin.tar.gz
RUN mv jdk-17.0.2 /opt

ENV JAVA_HOME=/opt/jdk-17.0.2

# PEST++ Binaries
COPY tools/pestbin /pestbin

# Scripts for running parts of the process
COPY runner/init.sh /init.sh
COPY runner/run.sh /run.sh
COPY runner/run_model.sh /run_model.sh
COPY runner/run_simulation.sh /run_simulation.sh

# Serve files from the root directory
RUN sed -ri -e 's!/var/www/html!/!g' /etc/apache2/sites-available/*.conf
RUN sed -ri -e 's!/var/www/!/!g' /etc/apache2/apache2.conf /etc/apache2/conf-available/*.conf

# Enable PHP modules
RUN docker-php-ext-install zip

ENV APACHE_RUN_DIR=/var/run/apache2
ENV APACHE_PID_FILE=/var/run/apache2/apache2.pid
ENV APACHE_RUN_USER=www-data
ENV APACHE_RUN_GROUP=www-data
ENV APACHE_LOG_DIR=/var/log/apache2
ENV APACHE_LOCK_DIR=/var/lock/apache2
ENV WORKING_PATH=/

ENTRYPOINT /init.sh & apache2 -D FOREGROUND
