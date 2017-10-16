FROM debian:stretch

RUN apt-get update && \
    apt install -y \
        gcc \
        g++ \
        gfortran \
        git \
        m4 \
        make \
        wget && \
    DIR=/usr/local && \
    v=1.2.8 && \
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4/zlib-${v}.tar.gz && \
    tar -xf zlib-${v}.tar.gz && \
    cd zlib-${v} && \
    ./configure --prefix=$DIR && \
    make install && cd .. && \
    rm zlib-${v}.tar.gz && \
    v=1.8.13 && \
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4/hdf5-${v}.tar.gz && \
    tar -xf hdf5-${v}.tar.gz && \
    cd hdf5-${v} && \
    ./configure --with-zlib=$DIR --prefix=$DIR && \
    make && make install && cd .. && \
    rm hdf-5-${v}.tar.gz && \
    v=4.4.1.1 && \
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-${v}.tar.gz && \
    tar -xf netcdf-${v}.tar.gz && \
    cd netcdf-${v} && \
    CPPFLAGS=-I${DIR}/include LDFLAGS=-L${DIR}/lib && \
    ./configure --prefix=${DIR} && \
    make && make install && cd .. && \
    rm netcdf-${v}.tar.gz && \
    v=4.4.4 && \
    wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-${v}.tar.gz && \
    tar -xf netcdf-fortran-${v}.tar.gz && \
    cd netcdf-fortran-${v} && \
    export LD_LIBRARY_PATH=${DIR}/lib:${LD_LIBRARY_PATH} && \
    CPPFLAGS=-I${DIR}/include LDFLAGS=-L${DIR}/lib && \
    ./configure --prefix=${DIR} && \
    make && make install && cd .. && \
    rm netcdf-fortran-${v}.tar.gz

VOLUME /nanofase
