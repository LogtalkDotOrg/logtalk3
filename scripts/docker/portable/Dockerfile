#############################################################################
## 
##   Logtalk Dockerfile
##   Last updated on June 6, 2024
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

MAINTAINER Paulo Moura <pmoura@logtalk.org>

VOLUME ["/source"]

# Arguments

# Logtalk
ARG LOGTALK_VERSION=master
# Backends
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
ENV MANPATH=$MANPATH:$LOGTALKHOME/man
ENV INFOPATH=$INFOPATH:$LOGTALKHOME/docs:$LOGTALKHOME/manuals

WORKDIR /downloads

# Install Logtalk developer tools dependencies
RUN apt-get update
RUN apt-get install -y graphviz info xsltproc fop libarchive-tools gnupg2 git curl direnv texlive texinfo
RUN apt-get install -y software-properties-common
RUN apt-add-repository ppa:qameta/allure
RUN apt-get install -y allure
RUN apt-get install -y python3-pip
RUN python3 -m pip install --upgrade pygments
RUN python3 -m pip install --upgrade sphinx
RUN python3 -m pip install --upgrade sphinx-rtd-theme
# Install Jupyter and the Logtalk kernel
RUN python3 -m pip install --upgrade jupyter_core
RUN python3 -m pip install --upgrade jupyterlab
RUN python3 -m pip install --upgrade notebook
RUN python3 -m pip install --upgrade logtalk-jupyter-kernel
RUN python3 -m logtalk_kernel.install
# Install backend dependencies
RUN DEBIAN_FRONTEND=noninteractive apt-get -y install tzdata
RUN apt-get install -y cmake
RUN apt-get install -y xxd
RUN apt-get install -y build-essential
RUN apt-get install -y ninja-build
RUN apt-get install -y pkg-config
RUN apt-get install -y ncurses-dev
RUN apt-get install -y libreadline-dev
RUN apt-get install -y libedit-dev
RUN apt-get install -y libgoogle-perftools-dev
RUN apt-get install -y libgmp-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y unixodbc-dev
RUN apt-get install -y zlib1g-dev
RUN apt-get install -y libarchive-dev
RUN apt-get install -y libossp-uuid-dev
RUN apt-get install -y libxext-dev
RUN apt-get install -y libice-dev
RUN apt-get install -y libjpeg-dev
RUN apt-get install -y libxinerama-dev
RUN apt-get install -y libxft-dev
RUN apt-get install -y libxpm-dev libxt-dev
RUN apt-get install -y libdb-dev
RUN apt-get install -y libpcre2-dev
RUN apt-get install -y libyaml-dev
RUN apt-get install -y default-jdk
RUN apt-get install -y junit4
RUN apt-get install -y tcl
RUN apt-get install -y tk
RUN apt-get install -y libffi-dev
RUN apt-get install -y rlwrap
RUN apt-get install -y emacs
RUN apt-get install -y imagemagick

# Install Logtalk
RUN curl -sL https://github.com/LogtalkDotOrg/logtalk3/archive/${LOGTALK_VERSION}.tar.gz | tar xz
RUN logtalk3-${LOGTALK_VERSION}/scripts/install.sh -p /usr/local
#RUN logtalk_user_setup.sh

# Install Ciao Prolog
WORKDIR /downloads
RUN git clone --depth 1 --branch ${CIAO_VERSION} https://github.com/ciao-lang/ciao
WORKDIR ciao
RUN ./ciao-boot.sh global-install

# Install CxProlog
WORKDIR /downloads
RUN curl -sL http://ctp.di.fct.unl.pt/~amd/cxprolog/cxunix/cxprolog-${CX_VERSION}.src.tgz | tar xz
WORKDIR cxprolog-${CX_VERSION}
RUN make install

# Install ECLiPSe
WORKDIR /downloads/eclipse
RUN curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/eclipse_basic.tgz
RUN curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/eclipse_doc.tgz
RUN curl -O http://eclipseclp.org/Distribution/Builds/${ECLIPSE_VERSION}/x86_64_linux/UNPACK
RUN chmod a+x UNPACK
RUN ./UNPACK
RUN echo -n "\n\n\n\n\n\n" | ./RUNME
ENV PATH=$PATH:/downloads/eclipse/bin/x86_64_linux

# Install GNU Prolog
WORKDIR /downloads
RUN git clone --depth 1 --branch ${GNU_VERSION} https://github.com/didoudiaz/gprolog
WORKDIR gprolog/src
RUN ./configure --prefix=/usr/local --without-doc-dir --without-html-dir && make && make install

# Install SWI-Prolog
WORKDIR /downloads
RUN git clone --depth 1 --branch ${SWI_VERSION} https://github.com/SWI-Prolog/swipl-devel.git
WORKDIR swipl-devel
RUN git submodule update --init
WORKDIR build
RUN cmake -DCMAKE_INSTALL_PREFIX=/usr/local -G Ninja ..
RUN ninja
RUN ninja install

# Install Trealla Prolog
WORKDIR /downloads
RUN git clone --depth 1 --branch ${TREALLA_VERSION} https://github.com/trealla-prolog/trealla
WORKDIR trealla
RUN make clean && make
ENV PATH=$PATH:/downloads/trealla

# Install XSB
WORKDIR /downloads
RUN git clone --depth 1 --branch ${XSB_VERSION} https://git.code.sf.net/p/xsb/code xsb
RUN cd xsb/XSB/build && ./configure && ./makexsb
ENV PATH=$PATH:/downloads/xsb/XSB/bin

# Install YAP
WORKDIR /downloads
RUN git clone --depth 1 --branch ${YAP_VERSION} https://github.com/vscosta/yap
WORKDIR yap/build
RUN cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. && make && make install

# Add the Logtalk environment variables to .bashrc and .profile
RUN echo "export LOGTALKUSER=/root/logtalk" >> ~/.bashrc
RUN echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.bashrc
RUN echo "export LOGTALKUSER=/root/logtalk" >> ~/.profile
RUN echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.profile

# Define locales (required e.g. by Sphinx)
RUN apt-get install -y locales locales-all
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

WORKDIR /source
ENTRYPOINT ["bash"]
