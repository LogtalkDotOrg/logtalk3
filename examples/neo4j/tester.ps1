#############################################################################
## 
##   PowerShell script to be sourced by the logtalk_tester script
##   Last updated on April 13, 2023
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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

[CmdletBinding()]
param(
	[Parameter()]
	[String]$p,
	[String]$o,
	[String]$m,
	[String]$f,
	[String]$d,
	[String]$t,
	[String]$n,
	[String]$s,
	[String]$b,
	[String]$u,
	[String]$c,
	[String]$l,
	[String]$e,
	[String]$i,
	[String]$g,
	[String]$r,
	[Switch]$w,
	[String[]]$a
)

if ($p -eq "swi") {
	& .\set_classpath_swi.ps1
} elseif ($p -eq "yap") {
	& .\set_classpath_yap.ps1
} else {
	Exit 1
}

Exit 0
