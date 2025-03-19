#############################################################################
## 
##   Logtalk script for updating the HTML core, library, tools, ports,
##   contributions, and (optionally) packs documentation
## 
##   Last updated on March 19, 2025
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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
	Write-Output "$myName 0.26"
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script updates the HTML documentation of the core entities, library,"
	Write-Output "developer tools, ports, contributions, and optionally installed packs."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-p prolog] [-i]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -p backend Prolog compiler (default is $p)"
	Write-Output "     (valid values are b, ciao, cx, eclipse, gnu, ji, xvm, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	Write-Output "  -i include all installed packs"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}


$scriptpath = $MyInvocation.MyCommand.Path
$dir = Split-Path $scriptpath
Push-Location $dir


if ($v -eq $true) {
	Write-Script-Version
	Exit
}

if ($h -eq $true) {
	Write-Usage-Help
	Exit
}

if ("$p" -eq "b") {
	$logtalk="bplgt -g"
} elseif ("$p" -eq "ciao") {
	$logtalk="ciaolgt -e"
} elseif ("$p" -eq "cx") {
	$logtalk="cxlgt --goal"
} elseif ("$p" -eq "eclipse") {
	$logtalk="eclipselgt -e"
} elseif ("$p" -eq "gnu") {
	$logtalk="gplgt --query-goal"
} elseif ("$p" -eq "ji") {
	$logtalk="jiplgt -n -g"
} elseif ("$p" -eq "sicstus") {
	$logtalk="sicstuslgt --goal"
} elseif ("$p" -eq "swi") {
	$logtalk="swilgt -g"
} elseif ("$p" -eq "swipack") {
	$logtalk="swipl -g"
} elseif ("$p" -eq "tau") {
	$logtalk="taulgt -g"
} elseif ("$p" -eq "trealla") {
	$logtalk="tplgt -g"
} elseif ("$p" -eq "xsb") {
	$logtalk="xsblgt -e"
} elseif ("$p" -eq "xvm") {
	$logtalk="xvmlgt -g"
} elseif ("$p" -eq "yap") {
	$logtalk="yaplgt -g"
} else {
	Write-Output "Error! Unsupported backend Prolog compiler: $p"
	Write-Usage-Help
	Exit
}

if ($env:LOGTALKPACKS -ne "") {
	$logtalk_packs = '$env:LOGTALKPACKS/'
} else {
	$logtalk_packs = '$env:USERPROFILE/logtalk_packs/'
}

$cwd = $pwd

if ($i -eq $true) {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),library(packs_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$env:LOGTALKUSER/','$env:LOGTALKHOME/', '$logtalk_packs'])]),halt."
} elseif ($env:LOGTALKPACKS -ne "") {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$env:LOGTALKUSER/','$env:LOGTALKHOME/']),exclude_prefixes(['$env:USERPROFILE/logtalk_packs/','$env:LOGTALKPACKS/'])]),halt."
} else {
	$goal = "set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$env:LOGTALKUSER/','$env:LOGTALKHOME/']),exclude_prefixes(['$env:USERPROFILE/logtalk_packs/'])]),halt."
}

($logtalk + " " + ("`"$goal`"" -replace '\\','/')) | Invoke-Expression

Push-Location "$pwd/../docs/sources"

lgt2rst -t "Logtalk APIs"
if ($i -eq $true) {
	Copy-Item -Path _templates/layout_packs.html -Destination _templates/layout.html
} else {
	Copy-Item -Path _templates/layout_no_packs.html -Destination _templates/layout.html
}
Move-Item -Path _conf.py -Destination conf.py

./make.bat clean
./make.bat html
./make.bat info
./make.bat latexpdf
./make.bat epub
./make.bat singlehtml
#./make linkcheck

$version = Get-Content $env:LOGTALKUSER/VERSION.txt
$version_base = $version.Split("-")[0]
pandoc _build/singlehtml/index.html -t gfm-raw_html -o _build/singlehtml/LogtalkAPIs-$version_base.md
# Remove heading link references from the Markdown file
(Get-Content _build/singlehtml/LogtalkAPIs-$version_base.md) -replace '\[.\]\(#[-a-z0-9]+ "Link to this heading"\)', '' | Set-Content _build/singlehtml/LogtalkAPIs-$version_base.md

Copy-Item -Path ./_build/html/* -Destination .. -Recurse -Force
Copy-Item -Path ./_build/texinfo/LogtalkAPIs-*.info -Destination ..
Copy-Item -Path ./_build/latex/LogtalkAPIs-*.pdf -Destination ..
Copy-Item -Path ./_build/epub/LogtalkAPIs-*.epub -Destination ..
Copy-Item -Path ./_build/singlehtml/LogtalkAPIs-*.md -Destination ..

./make.bat clean
Remove-Item _templates/layout.html
Move-Item -Path conf.py -Destination _conf.py
Move-Item -Path browserconfig.xml -Destination browserconfig.xml.saved
try {
	Remove-Item ./*.xml
} catch {
	Write-Output "Error occurred at cleanup"
}
Move-Item -Path browserconfig.xml.saved -Destination browserconfig.xml
try {
	Remove-Item ./*.rst
} catch {
	Write-Output "Error occurred at cleanup"
}

Pop-Location

Pop-Location
