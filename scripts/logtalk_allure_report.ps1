#############################################################################
## 
##   Allure report generator script
##   Last updated on April 18, 2022
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
	[String]$d = $pwd,
	[String]$i = "allure-results",
	[String]$o = "allure-report",
	[String]$t,
	[Switch]$p,
	[String]$e = "",
	[Switch]$v,
	[Switch]$h
)

function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 0.9")
}

function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script generates Allure reports."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ("  " + $myName + " [-d tests] [-i results] [-o report] [-t title] [-p] [-e pairs]")
	Write-Output ("  " + $myName + " -v")
	Write-Output ("  " + $myName + " -h")
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -d tests directory (default is the current directory)"
	Write-Output "  -i results directory (default is $results)"
	Write-Output "  -o report directory (default is $report)"
	Write-Output "  -t report title (default is \"Allure Report\")"
	Write-Output "  -p preprocess results but do not generate report"
	Write-Output "  -e environment pairs as a string ('key1=value1,key2=value2,...')"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}


if (Test-Path $o -PathType container) {
	if ((Test-Path $o\data) -and (Test-Path $o\export) -and (Test-Path $o\history) -and `
		(Test-Path $o\plugins) -and (Test-Path $o\widgets) -and (Test-Path $o\app.js) -and `
		(Test-Path $o\favicon.ico) -and (Test-Path $o\index.html) -and (Test-Path $o\styles.css)) {
		Write-Output "Warning: Overriding previous report..."
	} else {
		Write-Output "Error! Specified report directory is not empty and does not contain a previous"
		Write-Output "       report. Terminating the script execution to prevent any data loss."
		Exit
	}
}

# move all xunit_report.xml files found in the current directory
# and sub-directories to the $results directory; the files are
# renamed using an integer counter to avoid overwriting them

New-Item -Path $i -ItemType directory -Force > $null
try {
	Remove-Item -Path $i/xunit_report_*.xml -Recurse -Force
} catch {
	Write-Output "Error occurred at cleanup previous results"
}

$counter=0
Get-ChildItem -Path $d\xunit_report.xml -Recurse |
Foreach-Object {
	$counter++
	Move-Item -Path $_.FullName -Destination ($i + "\xunit_report_" + $counter + ".xml")
}

if ($e -ne "") {
	$e.Split(",") | Set-Content -Path $i/environment.properties
}

if ($p -eq $true) {
	Exit
}

# assume that the $i directory is kept between test
# runs and use a custom file to track the build order
if (Test-Path $i\history ) {
	$current_build = [int](Get-Content -Path $i\history\logtalk_build_number)
} else {
	$current_build = 1
}

# move the history from the previous report to the
# $results directory so that we can get trend graphs
if (Test-Path $o\history) {
	if (Test-Path $i\history) {
		try {
			Remove-Item -Path $i\history -Confirm -Recurse -Force
		} catch {
			Write-Output "Error occurred at cleanup results history"
		}
	}
	Move-Item -Path $o\history -Destination $i\history
	$next_build = $current_build + 1
	Set-Content -Path $i\history\logtalk_build_number -Value $next_build
} else {
	$next_build = $current_build
}

# add a minimal executor.json so that trend graphs
# show build labels
New-Item -Path $o -Name executor.json -ItemType "file" -Force > $null
Add-Content -Path $o\executor.json -Value ('"buildOrder": "' + $next_build + '"')
Add-Content -Path $o\executor.json -Value ('"buildName": "logtalk_allure_report#' + $next_build + '"')
Add-Content -Path $o\executor.json -Value '"name": "logtalk_tester"'
Add-Content -Path $o\executor.json -Value '"type": "logtalk_tester"'

Push-Location (Join-Path $i ..)

allure generate --clean --report-dir $o $i
if ($t -ne "") {
	(Get-Content ($o + "\widgets\summary.json")) -Replace 'Allure Report', $t | Set-content ($o + "\widgets\summary.json")
}

Pop-Location
