#############################################################################
##
##   Allure report generator script
##   Last updated on December 2, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
	[String]$d = $pwd,
	[String]$i = "allure-results",
	[String]$o = "allure-report",
	[String]$t,
	[Switch]$p,
	[Switch]$s,
	[String]$e = "",
	[Switch]$v,
	[Switch]$h
)

Function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output "$myName 0.15"
}

Function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve

	Write-Output ""
	Write-Output "This script generates Allure reports from test results."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output "  $myName [-d tests] [-i results] [-o report] [-t title] [-p] [-e pairs]"
	Write-Output "  $myName -v"
	Write-Output "  $myName -h"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output "  -d tests directory (default is the current directory)"
	Write-Output "  -i results directory (default is .\allure-results)"
	Write-Output "  -o report directory (default is .\allure-report)"
	Write-Output "  -t report title (default is `"Allure Report`")"
	Write-Output "  -p preprocess results but do not generate report"
	Write-Output "  -s single file report"
	Write-Output "  -e environment pairs as a string ('key1=value1,key2=value2,...')"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

Function Confirm-Parameters() {
	if ($v -eq $true) {
		Write-Script-Version
		Exit 0
	}
	if ($h -eq $true) {
		Write-Usage-Help
		Exit 0
	}
}

###################### here it starts ############################

$minimalAllureVersion="2.26.0"

if ($null -eq (Get-Command "allure" -ErrorAction SilentlyContinue))  {
	Write-Error "Error: Cannot find the allure command-line tool!"
	Write-Error "See https://allurereport.org/docs/ for installation instructions."
	Exit 1
} else {
	$allureVersion = allure --version
	if (-not [System.Version]::TryParse($allureVersion, [ref]$null) -or
		[System.Version]$allureVersion -lt [System.Version]$minimalAllureVersion) {
		Write-Output "Warning: allure $minimalAllureVersion or later version is recommended!"
	}
}

Confirm-Parameters

if (Test-Path $o -PathType container) {
	# consider the directory a previous Allure report when it contains the
	# usual Allure artifacts. Accept either "plugins" or "plugin" directory
	# names and do not require a favicon to be present (not all reports include
	# it). This makes the check more robust against small variations in
	# generated report layouts.
	if ((Test-Path "$o/data") -and (Test-Path "$o/export") -and (Test-Path "$o/history") -and `
		(Test-Path "$o/widgets") -and (Test-Path "$o/app.js") -and (Test-Path "$o/index.html") -and `
		(Test-Path "$o/styles.css")) {
		if ((Test-Path "$o/plugins") -or (Test-Path "$o/plugin")) {
			Write-Output "Updating report; preserving previous run history..."
		} else {
			Write-Error "Error! Specified report directory is not empty and does not contain a previous"
			Write-Error "       report. Terminating the script execution to prevent any data loss."
			Exit 1
		}
	} else {
		Write-Error "Error! Specified report directory is not empty and does not contain a previous"
		Write-Error "       report. Terminating the script execution to prevent any data loss."
		Exit 1
	}
}

# move all xunit_report.xml files found in the current directory
# and sub-directories to the $results directory; the files are
# renamed using an integer counter to avoid overwriting them

New-Item -Path $i -ItemType directory -Force > $null
try {
	Remove-Item -Path "$i/xunit_report_*.xml" -Recurse -Force
} catch {
	Write-Error "Error occurred at cleanup previous results"
}

$counter=0
Get-ChildItem -Path "$d/xunit_report.xml" -Recurse |
Foreach-Object {
	$counter++
	Move-Item -Path $_.FullName -Destination "$i/xunit_report_$counter.xml"
}

if ($e -ne "") {
	$e.Split(",") | Set-Content -Path "$i/environment.properties"
}

if ($p -eq $true) {
	Exit 0
}

# assume that the $i directory is kept between test
# runs and use a custom file to track the build order
if (Test-Path "$i/history") {
	$current_build = [int](Get-Content -Path "$i/history/logtalk_build_number")
} else {
	$current_build = 1
}

# move the history from the previous report to the
# $results directory so that we can get trend graphs
if (Test-Path "$o/history") {
	if (Test-Path "$i/history") {
		try {
			Remove-Item -Path "$i/history" -Force -Recurse
		} catch {
			Write-Output "Error occurred at cleanup results history"
		}
	}
	Move-Item -Path "$o/history" -Destination "$i/history"
	$next_build = $current_build + 1
	Set-Content -Path "$i/history/logtalk_build_number" -Value $next_build
} else {
	$next_build = $current_build
}

# add a minimal executor.json so that trend graphs show build labels
New-Item -Path $i -Name executor.json -ItemType "file" -Force > $null
Add-Content -Path "$i/executor.json" -Value '{'
Add-Content -Path "$i/executor.json" -Value "	`"buildOrder`": `"$next_build`""
Add-Content -Path "$i/executor.json" -Value "	`"buildName`": `"logtalk_allure_report#$next_build`""
Add-Content -Path "$i/executor.json" -Value '	"name": "logtalk_tester"'
Add-Content -Path "$i/executor.json" -Value '	"type": "logtalk_tester"'
Add-Content -Path "$i/executor.json" -Value '}'

# add minimal categories.json to classify failed tests
New-Item -Path $i -Name categories.json -ItemType "file" -Force > $null
Add-Content -Path "$i/categories.json" -Value '['
Add-Content -Path "$i/categories.json" -Value '	{'
Add-Content -Path "$i/categories.json" -Value '		"name": "Failed tests",'
Add-Content -Path "$i/categories.json" -Value '		"matchedStatuses": ["failed"]'
Add-Content -Path "$i/categories.json" -Value '	}'
Add-Content -Path "$i/categories.json" -Value ']'

Push-Location (Join-Path "$i" ..)

if ($s -eq $true) {
	if ("$t" -ne "") {
		allure generate --single-file --clean --name "$t" --report-dir "$o" "$i"
	} else {
		allure generate --single-file --clean --report-dir "$o" "$i"
	}
} else {
	if ("$t" -ne "") {
		allure generate --clean --name "$t" --report-dir "$o" "$i"
	} else {
		allure generate --clean --report-dir "$o" "$i"
	}
}

Pop-Location
