#!/bin/sh

#############################################################################
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

case "$1" in
    --mode)
        if [ "$2" = 1 ]; then
            printf 's SATISFIABLE\nc s exact arb frac 1/4\n'
        else
            printf 's SATISFIABLE\nc s exact arb int 1\n'
        fi
        ;;
    -tmpdir)
        mkdir "$2/nested" || exit 1
        printf 'temporary\n' > "$2/nested/data" || exit 1
        printf '%s\n' "${2%/*}" >&2
        case "$4" in
            1)
                printf 's SATISFIABLE\nc s exact arb int 1\n'
                ;;
            2)
                exec sleep 20
                ;;
            3)
                trap '' TERM
                while :; do sleep 20; done
                ;;
            *)
                exit 1
                ;;
        esac
        ;;
    *)
        exit 1
        ;;
esac
