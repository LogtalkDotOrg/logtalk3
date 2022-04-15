#############################################################################
## 
##   Logtalk script for updating the HTML core, library, tools, ports,
##   contributions, and (optionally) packs documentation
## 
##   Last updated on April 15, 2022
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


[CmdletBinding()]
param(
	[Parameter()]
	# default to SWI-Prolog as some of the documentation should be
	# generated using a multi-threaded backend Prolog compiler
	[String]$p = "swi",
	[Switch]$i,
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 0.21")
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script updates the HTML documentation of the library and the development tools."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ("  " + $myName + " [-p prolog] [-i]")
	Write-Output ("  " + $myName + " -v")
	Write-Output ("  " + $myName + " -h")
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output ("  -p backend Prolog compiler (default is " + $p + ")")
	Write-Output "     (valid values are b, ciao, cx, eclipse, gnu, ji, lvm, scryer, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	Write-Output "  -i include all installed packs"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}


if ($v -eq $true) {
	Write-Script-Version
	Exit
}

if ($h -eq $true) {
	Write-Usage-Help
	Exit
}

if ("$p" -eq "b") {
	$prolog='B-Prolog'
	$logtalk="bplgt -g"
} elseif ("$p" -eq "ciao") {
	$prolog='Ciao Prolog'
	$logtalk="ciaolgt -e"
} elseif ("$p" -eq "cx") {
	$prolog='CxProlog'
	$logtalk="cxlgt --goal"
} elseif ("$p" -eq "eclipse") {
	$prolog='ECLiPSe'
	$logtalk="eclipselgt -e"
} elseif ("$p" -eq "gnu") {
	$prolog='GNU Prolog'
	$logtalk="gplgt --query-goal"
} elseif ("$p" -eq "ji") {
	$prolog='JIProlog'
	$logtalk="jiplgt -n -g"
} elseif ("$p" -eq "lvm") {
	$prolog='LVM'
	$logtalk="lvmlgt -g"
} elseif ("$p" -eq "scryer") {
	$prolog='Scryer Prolog'
	$logtalk="scryerlgt -g"
} elseif ("$p" -eq "sicstus") {
	prolog='SICStus Prolog'
	logtalk="sicstuslgt --goal"
} elseif ("$p" -eq "swi") {
	$prolog='SWI-Prolog'
	$logtalk="swilgt -g"
} elseif ("$p" -eq "swipack") {
	$prolog='SWI-Prolog'
	$logtalk="swipl -g"
} elseif ("$p" -eq "tau") {
	$prolog='Tau Prolog'
	$logtalk="taulgt -g"
} elseif ("$p" -eq "trealla") {
	$prolog='Trealla Prolog'
	$logtalk="tplgt -g"
} elseif ("$p" -eq "xsb") {
	$prolog='XSB'
	$logtalk="xsblgt -e"
} elseif ("$p" -eq "yap") {
	$prolog='YAP'
	$logtalk="yaplgt -g"
} else {
	Write-Output ("Error! Unsupported backend Prolog compiler: " + "$p")
	Write-Usage-Help
	Exit
}

if ($env:LOGTALKPACKS -ne "") {
	$logtalk_packs = '$env:LOGTALKPACKS\'
} else {
	$logtalk_packs = '$env:USERPROFILE\logtalk_packs\'
}

$cwd = $pwd

if ($i -eq $true) {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),library(packs_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd\..\docs\sources'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\', '$logtalk_packs'])]),halt."
} elseif ($env:LOGTALKPACKS -ne "") {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd\..\docs\sources'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\']),exclude_prefixes(['$env:USERPROFILE\logtalk_packs\','$env:LOGTALKPACKS\'])]),halt."
} else {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd\..\docs\sources'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\']),exclude_prefixes(['$env:USERPROFILE\logtalk_packs\'])]),halt."
}

($logtalk + " " + ("`"$goal`"" -replace '\\','\\')) | Invoke-Expression

Push-Location $cwd\..\docs\sources
Write-Output $pwd

lgt2rst -t "Logtalk APIs"
Move-Item -Path _conf.py -Destination conf.py
.\make.bat clean
.\make.bat html
.\make.bat info
#make linkcheck
Copy-Item -Path _build\html\* -Destination .. -Recurse -Force
Copy-Item -Path _build\texinfo\LogtalkAPIs-*.info -Destination ..
.\make.bat clean
Move-Item -Path conf.py -Destination _conf.py
Move-Item -Path browserconfig.xml -Destination browserconfig.xml.saved
try {
	Remove-Item .\*.xml -Recurse -Force
} catch {
	Write-Output "Error occurred at cleanup"
}
Move-Item -Path browserconfig.xml.saved -Destination browserconfig.xml
try {
	Remove-Item .\*.rst -Recurse -Force
} catch {
	Write-Output "Error occurred at cleanup"
}

Pop-Location
