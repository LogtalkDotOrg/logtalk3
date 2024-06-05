#!/usr/bin/env bash

#############################################################################
##
##   Logtalk Docker publishing script for stable versions
##   Last updated on June 5, 2024
##
##   This file is part of Logtalk <https://logtalk.org/>
##   Copyright 2024 Paulo Moura <pmoura@logtalk.org>
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

docker build -t logtalk/logtalk3 -t logtalk/logtalk3:latest .
docker push logtalk/logtalk3
docker push logtalk/logtalk3:latest
