#############################################################################
## 
##   Logtalk Dockerfile
##   Last updated on February 27, 2026
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
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
ARG LOGTALK_TEXINFO_VERSION=3.98.0
# Backends
ARG B_VERSION=81
ARG CIAO_VERSION=v1.23.0-m1
ARG CX_VERSION=0.98.3
ARG ECLIPSE_VERSION=7.2_10
ARG GNU_VERSION=master
ARG SWI_VERSION=master
ARG TAU_VERSION=0.3.4
ARG TREALLA_VERSION=main
ARG XSB_VERSION=git-origin
ARG YAP_VERSION=master

# Logtalk environment
ENV LOGTALKHOME=/usr/local/share/logtalk
ENV LOGTALKUSER=/root/logtalk

WORKDIR /downloads

# Install Logtalk developer tools dependencies
RUN apt-get update \
    && apt-get install --no-install-recommends -y \
        curl \
        wget \
        direnv \
        fop \
        git \
        gnupg2 \
        graphviz \
        lynx \
        w3m \
        links2 \
        info \
        libarchive-tools \
        python3-pip \
        software-properties-common \
        texinfo \
        texlive-latex-extra \
        tex-gyre \
        latexmk \
        xsltproc \
        gh \
    && rm -rf /var/lib/apt/lists/* \
    && python3 -m pip install pygments==2.19.1 \
    && python3 -m pip install sphinx==7.4.7 \
    && python3 -m pip install sphinx_rtd_theme==2.0.0 \
    && python3 -m pip install sphinxcontrib-spelling==8.0.1

# Install glab
RUN curl -OL https://gitlab.com/gitlab-org/cli/-/releases/v1.55.0/downloads/glab_1.55.0_linux_amd64.deb \
    && dpkg -i glab_1.55.0_linux_amd64.deb \
    && rm -f glab_1.55.0_linux_amd64.deb

# Install Allure
RUN curl -OL https://github.com/allure-framework/allure2/releases/download/2.33.0/allure_2.33.0-1_all.deb \
    && dpkg -i allure_2.33.0-1_all.deb \
    && rm -f allure_2.33.0-1_all.deb

# Install Jupyter and the Logtalk kernel
RUN python3 -m pip install --upgrade jupyter_core \
    && python3 -m pip install --upgrade jupyterlab \
    && python3 -m pip install --upgrade notebook \
    && python3 -m pip install --upgrade jupytext \
    && python3 -m pip install --upgrade logtalk-jupyter-kernel \
    && python3 -m logtalk_kernel.install \
    && mkdir -p /root/.jupyter \
    && cd /root/.jupyter \
    && curl -O https://raw.githubusercontent.com/LogtalkDotOrg/logtalk-jupyter-kernel/master/logtalk_kernel/logtalk_kernel_config.py

# Install backend dependencies
RUN rm -rf /var/lib/apt/lists/* \
    && apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends -y install tzdata \
    && apt-get install --no-install-recommends -y \
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
        pandoc \
        pkg-config \
        rlwrap \
        tcl \
        tk \
        unixodbc-dev \
        xxd \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Install Logtalk
RUN curl -sL https://github.com/LogtalkDotOrg/logtalk3/archive/${LOGTALK_VERSION}.tar.gz | tar xz \
    && logtalk3-${LOGTALK_VERSION}/scripts/install.sh -p /usr/local \
    && rm -rf logtalk3-${LOGTALK_VERSION} \
    && logtalk_user_setup

# Download and install the Texinfo files for the latest stable version
WORKDIR /root/logtalk/docs/handbook
RUN curl -O https://logtalk.org/docs/handbook/TheLogtalkHandbook-${LOGTALK_TEXINFO_VERSION}.info \
    && cd /root/logtalk/docs/apis \
    && curl -O https://logtalk.org/docs/apis/LogtalkAPIs-${LOGTALK_TEXINFO_VERSION}.info

# Download required JAR files for the apache_poi example
RUN cd /usr/local/share/logtalk/examples/apache_poi/jars \
    && curl -OL https://repo1.maven.org/maven2/commons-codec/commons-codec/1.18.0/commons-codec-1.18.0.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/commons/commons-collections4/4.4/commons-collections4-4.4.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/commons/commons-compress/1.27.1/commons-compress-1.27.1.jar \
    && curl -OL https://repo1.maven.org/maven2/commons-io/commons-io/2.19.0/commons-io-2.19.0.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar \
    && curl -OL https://repo1.maven.org/maven2/org/lucee/curvesapi/1.6.0/curvesapi-1.6.0.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.24.3/log4j-api-2.24.3.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.24.3/log4j-core-2.24.3.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi/5.4.1/poi-5.4.1.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi-excelant/5.4.1/poi-excelant-5.4.1.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi-ooxml/5.4.1/poi-ooxml-5.4.1.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi-ooxml-full/5.4.1/poi-ooxml-full-5.4.1.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi-ooxml-schemas/4.1.2/poi-ooxml-schemas-4.1.2.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/poi/poi-scratchpad/5.4.1/poi-scratchpad-5.4.1.jar \
    && curl -OL https://repo1.maven.org/maven2/com/zaxxer/SparseBitSet/1.3/SparseBitSet-1.3.jar \
    && curl -OL https://repo1.maven.org/maven2/org/apache/xmlbeans/xmlbeans/5.3.0/xmlbeans-5.3.0.jar

# Download required JAR files for the clustering example
RUN cd /usr/local/share/logtalk/examples/clustering/jars \
    && curl -OL https://repo1.maven.org/maven2/org/apache/commons/commons-math3/3.6.1/commons-math3-3.6.1.jar

# Download required JAR files for the document_converter example
RUN cd /usr/local/share/logtalk/examples/document_converter/jars \
    && curl -OL https://repo1.maven.org/maven2/org/apache/tika/tika-app/2.8.0/tika-app-2.8.0.jar

# Create the LOGTALKUSER directory for the root user
RUN logtalk_user_setup

# Install d2
WORKDIR /downloads
RUN curl -fsSLO https://github.com/terrastruct/d2/releases/download/v0.6.9/d2-v0.6.9-linux-amd64.tar.gz \
    && tar -xzf d2-v0.6.9-linux-amd64.tar.gz \
    && make -sC "d2-v0.6.9" install \
    && rm -rf d2-v0.6.9-linux-amd64.tar.gz

# Install B-Prolog
WORKDIR /downloads
RUN if [ "$B_VERSION" != "none" ] ; then \
        curl -sL http://www.picat-lang.org/bprolog/download/bp${B_VERSION}_linux64.tar.gz | tar xz ; \
    fi
ENV PATH=$PATH:/downloads/BProlog/

# Install Ciao Prolog
WORKDIR /downloads
RUN if [ "$CIAO_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${CIAO_VERSION} https://github.com/ciao-lang/ciao \
        && cd ciao \
        && ./ciao-boot.sh global-install && ./ciao-boot.sh realclean \
        && rm -rf /downloads/ciao ; \
    fi

# Install CxProlog
WORKDIR /downloads
RUN if [ "$CX_VERSION" != "none" ] ; then \
        curl -sL http://ctp.di.fct.unl.pt/~amd/cxprolog/cxunix/cxprolog-${CX_VERSION}.src.tgz | tar xz \
        && cd cxprolog-${CX_VERSION} \
        && make install && make clean \
        && rm -rf /downloads/cxprolog-${CX_VERSION} ; \
    fi

# Install ECLiPSe
WORKDIR /downloads/eclipse
RUN if [ "$ECLIPSE_VERSION" != "none" ] ; then \
        curl -O http://eclipseclp.org/Distribution/Dev/${ECLIPSE_VERSION}/x86_64_linux/eclipse_basic.tgz \
        && curl -O http://eclipseclp.org/Distribution/Dev/${ECLIPSE_VERSION}/x86_64_linux/eclipse_doc.tgz \
        && curl -O http://eclipseclp.org/Distribution/Dev/${ECLIPSE_VERSION}/x86_64_linux/UNPACK \
        && chmod a+x UNPACK \
        && ./UNPACK \
        && echo -n "\n\n\n\n\n\n" | ./RUNME \
        && rm -f eclipse_basic.tgz eclipse_doc.tgz ; \
    fi
ENV PATH=$PATH:/downloads/eclipse/bin/x86_64_linux

# Install GNU Prolog
WORKDIR /downloads
RUN if [ "$GNU_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${GNU_VERSION} https://github.com/didoudiaz/gprolog \
        && cd gprolog/src \
        && ./configure --prefix=/usr/local --without-doc-dir --without-html-dir && make && make install && make distclean \
        && rm -rf /downloads/gprolog \
        && cd /usr/local/share/logtalk/scripts/embedding/gprolog \
        && ./gprolog_embed_logtalk.sh -c -n gplgtnc -s none -d /usr/local/bin -- --new-top-level --no-susp-warn ; \
    fi

# Install SWI-Prolog
WORKDIR /downloads
RUN if [ "$SWI_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${SWI_VERSION} https://github.com/SWI-Prolog/swipl-devel.git \
        && cd swipl-devel \
        && git submodule update --init --depth 1 \
        && mkdir -p build \
        && cd build \
        && cmake -DCMAKE_INSTALL_PREFIX=/usr/local -G Ninja .. \
        && ninja && ninja install && ninja clean \
        && rm -rf /downloads/swipl-devel ; \
    fi

# Install Tau Prolog
WORKDIR /downloads
RUN if [ "$TAU_VERSION" != "none" ] ; then \
        apt-get update \
        && apt-get install --no-install-recommends -y nodejs npm \
        && npm install -g readline-sync \
        && npm install -g tau-prolog@${TAU_VERSION} \
        && rm -rf /var/lib/apt/lists/* ; \
    fi
ENV NODE_PATH=/usr/local/lib/node_modules

# Install Trealla Prolog
WORKDIR /downloads
RUN if [ "$TREALLA_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${TREALLA_VERSION} https://github.com/trealla-prolog/trealla \
        && cd trealla \
        && make clean && make ; \
    fi
ENV PATH=$PATH:/downloads/trealla

# Install XSB
WORKDIR /downloads
RUN if [ "$XSB_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${XSB_VERSION} https://git.code.sf.net/p/xsb/code xsb \
        && cd xsb/XSB/build \
        && ./configure && ./makexsb && ./makexsb clean ; \
    fi
ENV PATH=$PATH:/downloads/xsb/XSB/bin

# Install YAP
WORKDIR /downloads
RUN if [ "$YAP_VERSION" != "none" ] ; then \
        git clone --depth 1 --branch ${YAP_VERSION} https://github.com/vscosta/yap \
        && mkdir -p yap/build \
        && cd yap/build \
        && cmake -DCMAKE_INSTALL_PREFIX=/usr/local .. \
        && make && make install && make clean \
        && rm -rf /downloads/yap ; \
    fi

# Add the Logtalk environment variables to .bashrc and .profile
RUN echo "export LOGTALKUSER=/root/logtalk" >> ~/.bashrc \
    && echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.bashrc \
    && echo "export LOGTALKUSER=/root/logtalk" >> ~/.profile \
    && echo "export LOGTALKHOME=/usr/local/share/logtalk" >> ~/.profile

# Define locales (required e.g. by Sphinx)
RUN apt-get update \
    && apt-get install --no-install-recommends -y locales locales-all \
    && rm -rf /var/lib/apt/lists/*
ENV LC_ALL=en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US.UTF-8

# Install support for man pages
RUN sed -i 's:^path-exclude=/usr/share/man:#path-exclude=/usr/share/man:' /etc/dpkg/dpkg.cfg.d/excludes \
    && apt-get update \
    && apt-get --yes upgrade \
    && apt-get install --no-install-recommends -y \
        apt-utils \
        dialog \
        manpages \
        manpages-posix \
        man-db \
    && rm -rf /var/lib/apt/lists/* \
    && rm -f /usr/bin/man \
    && dpkg-divert --quiet --remove --rename /usr/bin/man \
    && rm -f /usr/share/man/man1/sh.1.gz \
    && dpkg-divert --quiet --remove --rename /usr/share/man/man1/sh.1.gz

# Install SSH server
RUN apt-get update \
    && apt-get install --no-install-recommends -y openssh-server \
    && rm -rf /var/lib/apt/lists/* \
    && mkdir -p /var/run/sshd \
    && echo 'root:portable' | chpasswd \
    && sed -i 's/#PermitRootLogin prohibit-password/PermitRootLogin yes/' /etc/ssh/sshd_config \
    && sed -i 's/#PasswordAuthentication yes/PasswordAuthentication yes/' /etc/ssh/sshd_config
EXPOSE 22

# Copy the README.md into the image
WORKDIR /root
ADD README.md /root/README.md

WORKDIR /source
ENTRYPOINT ["bash"]
