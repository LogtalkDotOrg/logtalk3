#############################################################################
## 
##   Logtalk Dockerfile
##   Last updated on January 30, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 2017-2024 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identifier: Apache-2.0
##   
##   Licensed under the Apache License, Version 2.0 (the "License");
##   you may not use this file except in compliance with the License.
##   You may obtain a copy of the License at
##   
##       http://www.apache.org/licenses/LICENSE-2.0
##   
##   Unless required by applicable law or agreed to in writing, software
##   distributed under the License is distributed on an "AS IS" BASIS,
##   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
##   See the License for the specific language governing permissions and
##   limitations under the License.
## 
#############################################################################


FROM ubuntu:jammy

LABEL org.opencontainers.image.authors="pmoura@logtalk.org"

VOLUME ["/source"]

# Arguments

# Logtalk
ARG LOGTALK_VERSION=master
ARG LOGTALK_TEXINFO_VERSION=3.89.0
# Backends
ARG B_VERSION=81
ARG CIAO_VERSION=v1.23.0-m1
ARG CX_VERSION=0.98.3
ARG ECLIPSE_VERSION=7.1_13
ARG GNU_VERSION=master
ARG SWI_VERSION=master
ARG TREALLA_VERSION=main
ARG XSB_VERSION=git-origin
ARG YAP_VERSION=master

# Logtalk environment
ENV LOGTALKHOME=/usr/local/share/logtalk
ENV PATH=$PATH:$LOGTALKHOME/tools/diagrams:$LOGTALKHOME/tools/lgtdoc/xml:$LOGTALKHOME/scripts:$LOGTALKHOME/integration
ENV MANPATH=$LOGTALKHOME/man:
ENV INFOPATH=$LOGTALKHOME/docs:$LOGTALKHOME/manuals:

WORKDIR /downloads

# Install Logtalk developer tools dependencies
RUN apt-get update && apt-get install --no-install-recommends -y \
    curl \
    direnv \
    fop \
    git \
    gnupg2 \
    graphviz \
    info \
    libarchive-tools \
    python3-pip \
    software-properties-common \
    texinfo \
    texlive \
    xsltproc \
&& rm -rf /var/lib/apt/lists/*
RUN apt-add-repository ppa:qameta/allure \
    && apt-get install --no-install-recommends -y allure \
    && add-apt-repository --remove ppa:qameta/allure
RUN python3 -m pip install --upgrade pygments
RUN python3 -m pip install --upgrade sphinx
RUN python3 -m pip install --upgrade sphinx-rtd-theme

# Install Jupyter and the Logtalk kernel
RUN python3 -m pip install --upgrade jupyter_core
RUN python3 -m pip install --upgrade jupyterlab
RUN python3 -m pip install --upgrade notebook
RUN python3 -m pip install --upgrade jupytext
RUN curl -sOL https://logtalk.org/files/python/jupytext-1.16.6-py3-none-any.whl
RUN python3 -m pip install --force-reinstall jupytext-1.16.6-py3-none-any.whl
RUN rm -f jupytext-1.16.6-py3-none-any.whl
RUN python3 -m pip install --upgrade logtalk-jupyter-kernel
RUN python3 -m logtalk_kernel.install

# Install backend dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install \
    tzdata \
&& rm -rf /var/lib/apt/lists/*
RUN apt-get update && apt-get install --no-install-recommends -y \
    build-essential \
    cmake \
    default-jdk \
    junit4 \
    libarchive-dev \
    libdb-dev \
    libedit-dev \
    libffi-dev \
    libgmp-dev \
    libgoogle-perftools-dev \
    libice-dev \
    libjpeg-dev \
    libossp-uuid-dev \
    libpcre2-dev \
    libreadline-dev \
    libssl-dev \
    libxext-dev \
    libxft-dev \
    libxinerama-dev \
    libxpm-dev \
    libxt-dev \
    libyaml-dev \
    nano \
    ncurses-dev \
    ninja-build \
    pkg-config \
    rlwrap \
    tcl \
    tk \
    unixodbc-dev \
    xxd \
    zlib1g-dev \
&& rm -rf /var/lib/apt/lists/*

# Install Logtalk
RUN curl -sL https://github.com/LogtalkDotOrg/logtalk3/archive/${LOGTALK_VERSION}.tar.gz | tar xz
RUN logtalk3-${LOGTALK_VERSION}/scripts/install.sh -p /usr/local
RUN rm -rf logtalk3-${LOGTALK_VERSION}
# Create the LOGTALKUSER directory for the root user
RUN logtalk_user_setup
# Download and install the Texinfo files for the latest stable version
RUN cd /root/logtalk/manuals && curl -O https://logtalk.org/manuals/TheLogtalkHandbook-${LOGTALK_TEXINFO_VERSION}.info
RUN cd /root/logtalk/docs && curl -O https://logtalk.org/docs/LogtalkAPIs-${LOGTALK_TEXINFO_VERSION}.info

# Install B-Prolog
WORKDIR /downloads
RUN if [ "$B_VERSION" != "none" ] ; then curl -sL http://www.picat-lang.org/bprolog/download/bp${B_VERSION}_linux64.tar.gz | tar xz ; fi
ENV PATH=$PATH:/downloads/BProlog/

# Install Ciao Prolog
WORKDIR /downloads
RUN if [ "$CIAO_VERSION" != "none" ] ; then git clone --depth 1 --branch ${CIAO_VERSION} https://github.com/ciao-lang/ciao ; fi
WORKDIR ciao
RUN if [ "$CIAO_VERSION" != "none" ] ; then ./ciao-boot.sh global-install && ./ciao-boot.sh realclean; fi
RUN rm -rf /downloads/ciao

# Install CxProlog
WORKDIR /downloads
RUN if [ "$CX_VERSION" != "none" ] ; then curl -sL http://ctp.di.fct.unl.pt/~amd/cxprolog/cxunix/cxprolog-${CX_VERSION}.src.tgz | tar xz ; fi
WORKDIR cxprolog-${CX_VERSION}
RUN if [ "$CX_VERSION" != "none" ] ; then make install && make clean ; fi
RUN rm -rf /downloads/cxprolog-${CX_VERSION}

# Install ECLiPSe
WORKDIR /downloads/eclipse
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/eclipse_basic.tgz ; fi
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/eclipse_doc.tgz ; fi
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/UNPACK ; fi
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then chmod a+x UNPACK ; fi
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then ./UNPACK ; fi
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then echo -n "\n\n\n\n\n\n" | ./RUNME ; fi
ENV PATH=$PATH:/downloads/eclipse/bin/x86_64_linux
RUN rm eclipse_basic.tgz eclipse_doc.tgz

# Install GNU Prolog
WORKDIR /downloads
RUN if [ "$GNU_VERSION" != "none" ] ; then git clone --depth 1 --branch ${GNU_VERSION} https://github.com/didoudiaz/gprolog ; fi
WORKDIR gprolog/src
RUN if [ "$GNU_VERSION" != "none" ] ; then ./configure --prefix=/usr/local --without-doc-dir --without-html-dir && make && make install && make distclean ; fi
RUN rm -rf /downloads/gprolog

# Install SWI-Prolog
WORKDIR /downloads
RUN if [ "$SWI_VERSION" != "none" ] ; then git clone --depth 1 --branch ${SWI_VERSION} https://github.com/SWI-Prolog/swipl-devel.git ; fi
WORKDIR swipl-devel
RUN if [ "$SWI_VERSION" != "none" ] ; then git submodule update --init --depth 1 ; fi
WORKDIR build
RUN if [ "$SWI_VERSION" != "none" ] ; then cmake -DCMAKE_INSTALL_PREFIX=/usr/local -G Ninja .. ; fi
RUN if [ "$SWI_VERSION" != "none" ] ; then ninja && ninja install && ninja clean ; fi
RUN rm -rf /downloads/swipl-devel

# Install Trealla Prolog
WORKDIR /downloads
RUN if [ "$TREALLA_VERSION" != "none" ] ; then git clone --depth 1 --branch ${TREALLA_VERSION} https://github.com/trealla-prolog/trealla ; fi
WORKDIR trealla
RUN if [ "$TREALLA_VERSION" != "none" ] ; then make clean && make ; fi
ENV PATH=$PATH:/downloads/trealla

# Install XSB
WORKDIR /downloads
RUN if [ "$XSB_VERSION" != "none" ] ; then git clone --depth 1 --branch ${XSB_VERSION} https://git.code.sf.net/p/xsb/code xsb ; fi
RUN if [ "$XSB_VERSION" != "none" ] ; then cd xsb/XSB/build && ./configure && ./makexsb && ./makexsb clean ; fi
ENV PATH=$PATH:/downloads/xsb/XSB/bin

# Install YAP
WORKDIR /downloads
RUN if [ "$YAP_VERSION" != "none" ] ; then git clone --depth 1 --branch ${YAP_VERSION} https://github.com/vscosta/yap ; fi
WORKDIR yap/build
RUN if [ "$YAP_VERSION" != "none" ] ; then cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. && make && make install && make clean ; fi
RUN rm -rf /downloads/yap

# Add the Logtalk environment variables to .bashrc and .profile
RUN echo "export LOGTALKUSER=/root/logtalk" >> ~/.bashrc
RUN echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.bashrc
RUN echo "export LOGTALKUSER=/root/logtalk" >> ~/.profile
RUN echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.profile

# Define locales (required e.g. by Sphinx)
RUN apt-get update && apt-get install --no-install-recommends -y \
    locales \
    locales-all \
&& rm -rf /var/lib/apt/lists/*
ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

# Install support for man pages
RUN sed -i 's:^path-exclude=/usr/share/man:#path-exclude=/usr/share/man:' /etc/dpkg/dpkg.cfg.d/excludes
RUN apt-get update && apt --yes upgrade && apt-get install -y \
        apt-utils \
        dialog \
        manpages \
        manpages-posix \
        man-db
RUN rm -f /usr/bin/man
RUN dpkg-divert --quiet --remove --rename /usr/bin/man
RUN rm -f /usr/share/man/man1/sh.1.gz
RUN dpkg-divert --quiet --remove --rename /usr/share/man/man1/sh.1.gz

WORKDIR /source
ENTRYPOINT ["bash"]
