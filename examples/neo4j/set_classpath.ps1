#############################################################################
##
##   Set CLASSPATH environment variable
##   Last updated on March 18, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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

$first = $true

Get-ChildItem -Path jars\* -Filter *.jar |
Foreach-Object {
    if ($first) {
        $classpath = $_.FullName
        $first = $false
    } else {
        $classpath += ";" + $_.FullName
    }
}

[System.Environment]::setEnvironmentVariable("CLASSPATH", $classpath, "Process")
