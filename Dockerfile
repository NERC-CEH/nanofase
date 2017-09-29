FROM debian:stretch

RUN apt-get update && \
    apt install -y \
        curl \
        gcc \
        g++ \
        gfortran \
        m4 \
        make \
        wget && \
    BASHRC="~/.bashrc" && \
    v=1.2.8 && wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4/zlib-${v}.tar.gz && \
    tar -xf zlib-${v}.tar.gz && cd zlib-${v} && \
    ./configure --prefix=/usr/local && \
    make install && cd .. && \
    v=1.8.13 && wget ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-4/hdf5-${v}.tar.gz && \
    tar -xf hdf5-${v}.tar.gz && cd hdf5-${v} && \
    prefix="/usr/local/hdf5-$v" && \
    HDF5_DIR=$prefix && \
    ./configure --enable-shared --enable-hl --prefix=$HDF5_DIR && \
    make -j 2 && make install && cd .. && \
    v=4.4.1.1 && wget http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-${v}.tar.gz && \
    tar -xf netcdf-${v}.tar.gz && cd netcdf-${v} && \
    prefix="/usr/local/" && \
    NETCDF4_DIR=$prefix && \
    CPPFLAGS=-I$HDF5_DIR/include LDFLAGS=-L$HDF5_DIR/lib ./configure --enable-netcdf-4 --enable-shared --enable-dap --prefix=$NETCDF4_DIR && \
    make && make install && cd .. && \
    v=4.4.1 && wget http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-fortran-${v}.tar.gz && \
    tar -xf netcdf-fortran-${v}.tar.gz && cd netcdf-fortran-${v} && \
    NCDIR=/usr/local/ && NFDIR=/usr/local/ && CPPFLAGS=-I${NCDIR}/include LDFLAGS=-L${NCDIR}/lib && \
    ./configure --prefix=${NFDIR} && \
    make check && make install

COPY . /nanofase    
