#############################################################################
## 
##   Set CLASSPATH environment variable for YAP
##   Last updated on March 16, 2023
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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

& yap -dump-runtime-variables > (Join-Path $pwd "yap_runtime_variables.txt")
$line = (Get-Content (Join-Path $pwd "yap_runtime_variables.txt") | Select-String -Pattern 'PLBASE' -CaseSensitive -SimpleMatch -Raw).split("=")

$classpath = ((($line[1] -replace ";", "") -replace "/", "\") -replace "`"", "") + "\lib\jpl.jar"

Get-ChildItem -Path jars\* -Filter *.jar |
Foreach-Object {
	$classpath += ";" + $_.FullName
}

[System.Environment]::setEnvironmentVariable("CLASSPATH", $classpath, "Process")
