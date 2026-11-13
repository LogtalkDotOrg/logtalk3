#!/bin/sh

#############################################################################
##
##   MiniZinc solver CLI integration script
##   Last updated on November 13, 2026
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


exec 2>&1

case "$1" in
    allocate)
        umask 077
        exec "$2" -d "$3/logtalk-minizinc.XXXXXXXXXX"
        ;;
    run)
        shift
        output=$1
        errors=$2
        shift 2
        exec "$@" >"$output" 2>"$errors"
        ;;
    *)
        printf '%s\n' 'Invalid MiniZinc launcher operation'
        exit 125
        ;;
esac
