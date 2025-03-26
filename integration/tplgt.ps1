#############################################################################
##
##   Integration script for Trealla Prolog
##   Last updated on March 22, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   Copyright 2022-2025 Paulo Moura <pmoura@logtalk.org>
##   Copyright 2022 Hans N. Beck
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


#Requires -Version 7.3

Import-Module (Join-Path $PSScriptRoot "LogtalkSetupEnv.psm1")
Initialize-LogtalkEnvironment

$source = $env:LOGTALKHOME + '\integration\logtalk_tp.pl'

if ($args.Count -gt 2 -and $args[$args.Count-2] -eq "--%") {
    $n = $args.Count - 3
    tpl.exe --library "(Split-Path (where.exe tpl.exe)\library" -l $source $args[0..$n] -- (-Split $args[$args.Count-1])
} elseif ($args.Count -eq 2 -and $args[0] -eq "--%") {
    tpl.exe --library "(Split-Path (where.exe tpl.exe)\library" -l $source -- (-Split $args[$args.Count-1])
} else {
    tpl.exe --library "(Split-Path (where.exe tpl.exe)\library" -l $source $args
}
