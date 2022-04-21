#############################################################################
## 
##   Documentation automation script
##   Last updated on April 21, 2022
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
	[Parameter(Mandatory=$true)]
	# default to SWI-Prolog as the backend compiler
	[String]$p = "swi",
	[String]$d = "$pwd\logtalk_doclet_logs",
	[String]$s = $env:USERPROFILE,
	# disable timeouts to maintain backward compatibility
	[String]$t = 0,
	[String]$a = "",
	[Switch]$i,
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 2.3")
}

function Run-Doclets() {
param(
	[Parameter()]
	[String]$path
)
	$directory = Split-Path -Path $path
	$directory_short = $directory -replace $prefix, ""
	Set-Location $d
	Write-Output '*******************************************************************************'
	Write-Output "***** Documenting $directory_short"
	$name = $directory -match '/' -replace '__'
	$doclet_exit = Run-Doclet $name $documenting_goal
	if ($doclet_exit -eq 0) {
		return 0
	} elseif ($doclet_exit -eq 124) {
		Write-Output "*****         timeout"
		Add-Content -Path $d\$name.errors -Value "LOGTALK_TIMEOUT"
	} else {
		Write-Output "*****         crash"
		Add-Content -Path $d\$name.errors -Value "LOGTALK_CRASH"
	}
	Exit 0
}

function Run-Doclet() {
param(
	[Parameter()]
	[String]$name,
	[String]$goal
)
	$results_file = Join-Path $d ($name + ".results")
	$errors_file  = Join-Path $d ($name + ".errors")
	if ($t -ne 0) {
		& $timeout_command $t $logtalk_call $goal $a > $results_file 2> $errors_file
	} else {
		& $logtalk_call $goal $a > $results_file 2> $errors_file
	}
	return $?
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script automates running doclets found on the current directory and recursively"
	Write-Output "in its sub-directories by scanning for doclet.lgt and doclet.logtalk source files. In"
	Write-Output  "case of failed doclets or doclet errors, this script returns a non-zero exit code."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ("  " + $myName + " -p prolog [-d results] [-t timeout] [-s prefix] [-a arguments]")
	Write-Output ("  " + $myName + " -v")
	Write-Output ("  " + $myName + " -h")
	Write-Output ""
	Write-Output "Required arguments:"
	Write-Output "  -p backend Prolog compiler"
	Write-Output "     (possible values are b, ciao, cx, eclipse, gnu, ji, lvm, scryer, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -d directory to store the doclet logs (default is ./logtalk_doclet_logs)"
	Write-Output ("  -t timeout in seconds for running each doclet (default is " + $t + "; i.e. disabled)")
	Write-Output ("  -s suppress path prefix (default is " + $s + ")")
	Write-Output "  -a arguments wrapped as a string to be passed to the integration script used to run the doclets (no default)"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}


function Check-Parameters() {

	if ($v -eq $true) {
		Write-Script-Version
		Exit
	}

	if ($h -eq $true) {
		Write-Usage-Help
		Exit
	}

	if ($p -eq "b") {
		$prolog='B-Prolog'
		$logtalk="bplgt -g"
		$logtalk_call="$logtalk -g"
	} elseif ($p -eq "ciao") {
		$prolog='Ciao Prolog'
		$logtalk="ciaolgt -e"
		$logtalk_call="$logtalk -e"
	} elseif ($p -eq "cx") {
		$prolog='CxProlog'
		$logtalk="cxlgt --goal"
		$logtalk_call="$logtalk --goal"
	} elseif ($p -eq "eclipse") {
		$prolog='ECLiPSe'
		$logtalk="eclipselgt -e"
		$logtalk_call="$logtalk -e"
	} elseif ($p -eq "gnu") {
		$prolog='GNU Prolog'
		$logtalk="gplgt --query-goal"
		$logtalk_call="$logtalk --query-goal"
	} elseif ($p -eq "ji") {
		$prolog='JIProlog'
		$logtalk="jiplgt -n -g"
		$logtalk_call="$logtalk -n -g"
	} elseif ($p -eq "lvm") {
		$prolog='LVM'
		$logtalk="lvmlgt -g"
		$logtalk_call="$logtalk $i_arg -g"
		$dot="."
	} elseif ($p -eq "scryer") {
		$prolog='Scryer Prolog'
		$logtalk="scryerlgt -g"
		$logtalk_call="$logtalk $i -g"
	} elseif ($p -eq "sicstus") {
		$prolog='SICStus Prolog'
		$logtalk="sicstuslgt --goal"
		$logtalk_call="$logtalk --goal"
		$dot="."
	} elseif ($p -eq "swi") {
		$prolog='SWI-Prolog'
		$logtalk="swilgt -g"
		$logtalk_call="$logtalk -g"
	} elseif ($p -eq "swipack") {
		$prolog='SWI-Prolog'
		$logtalk="swipl -g"
		$logtalk_call="$logtalk -g"
	} elseif ($p -eq "tau") {
		$prolog='Tau Prolog'
		$logtalk="taulgt -g"
		$logtalk_call="$logtalk -g"
	} elseif ($p -eq "trealla") {
		$prolog='Trealla Prolog'
		$logtalk="tplgt -g"
		$logtalk_call="$logtalk $i -g"
	} elseif ($p -eq "xsb") {
		$prolog='XSB'
		$logtalk="xsblgt -e"
		$logtalk_call="$logtalk -e"
		$dot="."
	} elseif ($p -eq "yap") {
		$prolog='YAP'
		$logtalk="yaplgt -g"
		$logtalk_call="$logtalk -g"
	} else {
		Write-Output ("Error! Unsupported backend Prolog compiler: " + "$p")
		Write-Usage-Help
		Exit
	}

	if ($p -eq "swipack") {
		$versions_goal = ("use_module(library(logtalk))," + $versions_goal)
	}

}

###################### here it starts ############################ 

# documenting goals

$versions_goal = ("logtalk_load(library(tester_versions)),halt" + $dot)
$documenting_goal = ("logtalk_load([doclet(loader),doclet]),halt" + $dot)

# default argument values

$base = $pwd
$dot = ""
$prolog = 'SWI-Prolog'
$logtalk = "swilgt"
$logtalk_call = $logtalk + " -g"

Check-Parameters

New-Item -Path $d -ItemType directory -Force > $null

if (Test-Path $d\*.results) {
	Remove-Item -Path $d\*.results -Force
}
if (Test-Path $d\*.errors) {
	Remove-Item -Path $d\*.errors -Force
}
if (Test-Path $d\errors.all) {
	Remove-Item -Path $d\errors.all -Force
}
if (Test-Path $d\tester_versions.txt) {
	Remove-Item -Path $d\tester_versions.txt -Force
}

$start_date = Get-Date -Format "yyyy-MM-dd-HH:mm:ss"

Write-Output '*******************************************************************************'
Write-Output ("***** Batch documentation processing started @ " + $start_date)
$tester_versions_file = Join-Path $d tester_versions.txt
($logtalk_call + " `"" + $versions_goal + "`"") > $tester_versions_file 2> /dev/null | Invoke-Expression
Select-String -Path $d\tester_versions.txt -Pattern "Logtalk version:" -SimpleMatch -Raw
(Select-String -Path $d\tester_versions.txt -Pattern "Prolog version:"  -SimpleMatch -Raw) -replace "Prolog", $prolog

$doclets=0
Get-ChildItem -Path . -Include doclet.lgt doclet.logtalk -Recurse | 
Foreach-Object {
	$doclets++
	Run-Doclets $_.FullName
}

Push-Location $d

$timeouts = (Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern 'LOGTALK_TIMEOUT').count
$crashes =  (Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern 'LOGTALK_CRASH').count
$failures = (Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'failed').count

Write-Output "*******************************************************************************"
Write-Output "***** Compilation errors/warnings and failed doclets"
Write-Output "*******************************************************************************"
(Get-ChildItem -Path . -Filter *.results  | Select-String -Pattern 'syntax_error' -SimpleMatch -Raw) -replace '.results', '' | Tee-Object -FilePath errors.all -Append
(Get-ChildItem -Path . -Filter *.errors   | Select-String -Pattern 'syntax_error' -SimpleMatch -Raw) -replace '.errors', ''  | Tee-Object -FilePath errors.all -Append
(Get-ChildItem -Path . -Filter *.results  | Select-String -Pattern '!     ' -SimpleMatch -Raw) -replace '.results', '' | Tee-Object -FilePath errors.all -Append
(Get-ChildItem -Path . -Filter *.errors   | Select-String -Pattern '!     ' -SimpleMatch -Raw) -replace '.errors', ''  | Tee-Object -FilePath errors.all -Append
(Get-ChildItem -Path . -Filter *.results  | Select-String -Pattern '*     ' -SimpleMatch -Raw) -replace '.results', '' | Tee-Object -FilePath errors.all -Append
(Get-ChildItem -Path . -Filter *.errors   | Select-String -Pattern '*     ' -SimpleMatch -Raw) -replace '.errors', ''  | Tee-Object -FilePath errors.all -Append
Write-Output "*******************************************************************************"
Write-Output "***** Timeouts"
Write-Output "*******************************************************************************"
((Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern 'LOGTALK_TIMEOUT' -SimpleMatch -Raw) -replace '\\', '__') -replace $prefix, ''
Write-Output "*******************************************************************************"
Write-Output "***** Crashes"
Write-Output "*******************************************************************************"
((Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern 'LOGTALK_CRASH' -SimpleMatch -Raw) -replace '\\', '__') -replace $prefix, ''
Write-Output "*******************************************************************************"
Write-Output "***** Failures"
Write-Output "*******************************************************************************"
Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'failed' -SimpleMatch -Raw
Write-Output "*******************************************************************************"
Write-Output ("***** " + $doclets + " doclets: " + $timeouts + " timeouts, " + $crashes + " crashes, " + $failures + " failures")
Write-Output "*******************************************************************************"

$end_date = Get-Date -Format "yyyy-MM-dd-HH:mm:ss"

Write-Output ("***** Batch documentation processing ended @ " + $end_date)
Write-Output '*******************************************************************************'

Pop-Location

if ($crashes -gt 0) {
	Exit 7
} elseif ($timeouts -gt 0) {
	Exit 3
} elseif ($failures -gt 0) {
	Exit 1
} else {
	Exit 0
}
