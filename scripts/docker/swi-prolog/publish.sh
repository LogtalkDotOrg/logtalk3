#!/bin/bash

#############################################################################
##
##   Logtalk Docker publishing script
##   Last updated on February 3, 2017
##
##   This file is part of Logtalk <http://logtalk.org/>
##   Copyright 2017 Sergio Castro <sergioc78@gmail.com> and
##   Paulo Moura <pmoura@logtalk.org>
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

TAG="3091"

docker build -t logtalk/logtalk3-swi -t logtalk/logtalk3-swi:${TAG} .
docker push logtalk/logtalk3-swi
docker push logtalk/logtalk3-swi:${TAG}
