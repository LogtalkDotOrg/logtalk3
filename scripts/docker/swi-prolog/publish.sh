#!/usr/bin/env bash

#############################################################################
##
##   Logtalk Docker publishing script for stable versions
##   Last updated on February 20, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   Copyright 2017 Sergio Castro <sergioc78@gmail.com> and
##   Paulo Moura <pmoura@logtalk.org>
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


# allow using this script from any directory
cd "$(dirname "$0")" || exit 1

TAG=$(cat ../../../VERSION.txt | sed -e 's/-stable$//' | sed -e 's/\.//g')
VERSION=lgt${TAG}stable
docker build --no-cache --build-arg LOGTALK_VERSION=${VERSION} -t logtalk/logtalk3-swi -t logtalk/logtalk3-swi:${TAG} .
docker push logtalk/logtalk3-swi
docker push logtalk/logtalk3-swi:${TAG}
# https://github.com/christian-korneck/docker-pushrm
docker pushrm logtalk/logtalk3-swi
