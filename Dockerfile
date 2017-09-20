FROM ubuntu

RUN apt-get update && apt-get install -y software-properties-common && \
    add-apt-repository ppa:ubuntu-toolchain-r/test -y && \
    apt update && \
    apt install -y \
        gfortran-7 \
        make

COPY . /nanofase    
