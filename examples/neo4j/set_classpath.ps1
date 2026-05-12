#############################################################################
##
##   Set CLASSPATH environment variable
##   Last updated on May 12, 2026
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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

$neo4j = neo4j status --verbose |
    Select-String -Pattern '-Dapp.home=([^,]+)' |
    ForEach-Object { $_.Matches[0].Groups[1].Value } |
    Select-Object -First 1

$jarPatterns = @(
    'neo4j-java-driver-*.jar',
    'neo4j-bolt-connection*.jar',
    'netty-*.jar',
    'reactive-streams-*.jar',
    'reactor-*.jar',
    'slf4j-api-*.jar'
)

$jars = foreach ($pattern in $jarPatterns) {
    Get-ChildItem -Path (Join-Path $neo4j lib) -Filter $pattern |
        Sort-Object -Property Name |
        ForEach-Object { $_.FullName }
}

$classpath = @($jars) -join [IO.Path]::PathSeparator

[System.Environment]::setEnvironmentVariable("CLASSPATH", $classpath, "Process")
