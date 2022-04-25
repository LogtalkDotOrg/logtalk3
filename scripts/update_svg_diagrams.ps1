#############################################################################
## 
##   Logtalk script for updating the HTML library and tools SVG diagrams
## 
##   Last updated on April 25, 2022
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
	# default to SWI-Prolog as the backend compiler
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
	Write-Output "This script updates the SVG diagrams of the core entities, the library,"
	Write-Output "the development tools, and the third-party contributions."
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


$prolog='SWI-Prolog'
$logtalk="swilgt$extension -g"

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

# documentation goals

$core_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), inheritance_diagram::library(core,[title('Logtalk core entities'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\'])]), halt."

$library_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(library(all_loader)), inheritance_diagram::rlibrary(library, [title('Logtalk library'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\'])]), halt."

if ($i -eq $true) {
	$packs_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(library(all_loader)), logtalk_load(library(packs_loader)), inheritance_diagram::rdirectory(packs, '$logtalk_packs', [title('Logtalk packs'),node_type_captions(true),zoom(true),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/', '$logtalk_packs','$HOME/'])]), halt."
}
	
if ($env:LOGTALKPACKS -ne "") {
	$tools_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report)]), inheritance_diagram::rlibrary(tools, [title('Logtalk development tools'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\']),exclude_directories(['$env:USERPROFILE\logtalk_packs\','$env:LOGTALKPACKS\'])]), halt."
} else {
	$tools_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report)]), inheritance_diagram::rlibrary(tools, [title('Logtalk development tools'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\']),exclude_directories(['$env:USERPROFILE\logtalk_packs\'])]), halt."
}
	
$ports_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(ports(loader)), inheritance_diagram::rlibrary(ports, [title('Logtalk ports of third-party software'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\'])]), halt."
	
$contributions_goal = "git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(contributions(loader)), inheritance_diagram::rlibrary(contributions, [title('Logtalk third-party contributions'),node_type_captions(true),zoom(true),path_url_prefixes('$env:LOGTALKUSER\',GitHub,'https://logtalk.org/library/'),path_url_prefixes('$env:LOGTALKHOME\',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$env:LOGTALKUSER\','$env:LOGTALKHOME\','$env:USERPROFILE\'])]), halt."


Push-Location ..\docs

($logtalk + " " + ("`"$core_goal`"" -replace '\\', '/')) | Invoke-Expression
($logtalk + " " + ("`"$library_goal`"" -replace '\\', '/')) | Invoke-Expression
if ($i -eq $true) {
	($logtalk + " " + ("`"$packs_goal`"" -replace '\\', '/')) | Invoke-Expression
}
($logtalk + " " + ("`"$tools_goal`"" -replace '\\', '/')) | Invoke-Expression
($logtalk + " " + ("`"$contributions_goal`"" -replace '\\', '/')) | Invoke-Expression
($logtalk + " " + ("`"$ports_goal`"" -replace '\\', '/')) | Invoke-Expression

lgt2svg

try {
	Remove-Item .\*.dot -Recurse -Force
} catch {
	Write-Output "Error occurred at cleanup"
}

Pop-Location
