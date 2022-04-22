#############################################################################
## 
##   Unit testing automation script
##   Last updated on April 22, 2022
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
##   SPDX-License-Identi}er: Apache-2.0
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
##   See the License for the speci}c language governing permissions and
##   limitations under the License.
## 
#############################################################################

[CmdletBinding()]
param(
	[Parameter()]
	[String]$p,
	[String]$o = "verbose",
	[String]$m = "normal",
	[String]$f = "default",
	[String]$d,
	# disable timeouts to maintain backward compatibility
	[String]$t = 0,
	[String]$n = "tester",
	[String]$s,
	[String]$b,
	[String]$u,
	[String]$c = "none",
	[String]$l,
	[String]$i,
	[String]$g = "true",
	[String]$r,
	[Switch]$w,
	[String]$a,
	[Switch]$v,
	[Switch]$h
)

Function Write-Script-Version {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve
	Write-Output ($myName + " 10.2")
}

Function Run-TestSet() {
param(
	[Parameter(Position = 0)]
	[String]$path
)
	$start_time = Get-Date -UFormat %s
	$unit = (Split-Path -Path $path) -replace '\\', '/'
	$unit_short = $unit -replace $prefix, ""
	Set-Location "$unit"
	if ($w -eq $true) {
		Remove-Item -Path .\.lgt_tmp -Recurse -Force
		Remove-Item -Path .\lgt_tmp  -Recurse -Force
	}
	if ($o -eq "verbose") {
		Write-Output "%"
		Write-Output ("% " + $unit_short)
	}
#	if (Test-Path $n.sh) {
#		$source_exit = & $n.sh $a
#		if ($source_exit -gt 0) {
#			Write-Output "%         source $n.sh returned code $source_exit"
#			Exit 9
#		}
#	}
	# convert any forward slashes so that the derived file name is usable
	# also convert any colon if running on Windows systems so that the derived file name is usable
	$name = (($unit -replace '/', '__') -replace '\\', '__') -replace ':', '___'
	$report_goal = "logtalk_load(lgtunit(automation_report)),set_logtalk_flag(test_results_directory,'" + $d + "'),set_logtalk_flag(test_unit_name,'" + $name + "')"
	if ($s -ne "") {
		$flag_goal = ("set_logtalk_flag(suppress_path_prefix,'" + $s + "')")
	} else {
		$flag_goal = "true"
	}
	if ($issue_server-ne "") {
		$flag_goal = "logtalk_load(issue_creator(loader)),set_logtalk_flag(issue_server,'" + $issue_server + "'),set_logtalk_flag(issue_labels,'" + $issue_labels + "')," + $flag_goal
	}
	if ($u -ne "") {
		$flag_goal = "set_logtalk_flag(tests_base_url,'" + $url + "')," + $flag_goal
	}
	if ($format -ne "default" -and $coverage -ne "none") {
		$flag_goal = "set_logtalk_flag(tests_report_directory,'" + $unit + "/')," + $flag_goal
	}
	if ($m -eq "optimal" -or $m -eq "all") {
		$tests_exit = Run-Tests "$name" ($initialization_goal + "," + $report_goal + "," + $format_goal + "," + $coverage_goal + "," + $flag_goal+ "," + $seed_goal + "," + $tester_optimal_goal)
		$mode_prefix="% (opt)   "
	} elseif ($m -eq "normal" -or $m -eq "all") {
		$tests_exit = Run-Tests $name ($initialization_goal + "," + $report_goal + "," + $format_goal + "," + $coverage_goal + "," + $flag_goal+ "," + $seed_goal + "," + $tester_normal_goal)
		$mode_prefix="%         "
	} elseif ($m -eq "debug" -or $m -eq "all") {
		$tests_exit = Run-Tests "$name" ($initialization_goal + "," + $report_goal + "," + $format_goal + "," + $coverage_goal + "," + $flag_goal+ "," + $seed_goal + "," + $tester_debug_goal)
		$mode_prefix="% (debug) "
	}

	if ($tests_exit -eq 0 -and $o -eq "verbose" -and (Select-String -Path $results/name.results -Pattern "tests skipped" -SimpleMatch -Quiet)) {
		Write-Output "%         skipped"
	} elseif ($tests_exit -eq 0 -and $o -eq "verbose" -and (Select-String -Path $results/name.results -Pattern "(not applicable)" -SimpleMatch -Quiet)) {
		Write-Output "%         not applicable"
	} elseif ($tests_exit -eq 0 -and (Test-Path $results/$name.totals) -and $o -eq "verbose") {
		Get-ChildItem -Path . -Filter .\*.totals |
		Foreach-Object {
			if ($_ | Select-String -Pattern '^object' -CaseSensitive -Quiet) {
				Write-Host -NoNewline $mode_prefix
				Write-Host -NoNewline $_.split("\t")[2]
				Write-Host -NoNewline ' tests: '
				Write-Host -NoNewline $_.split("\t")[3]
				Write-Host -NoNewline ' skipped, '
				Write-Host -NoNewline $_.split("\t")[4]
				Write-Host -NoNewline ' passed, '
				Write-Host -NoNewline $_.split("\t")[5]
				Write-Host -NoNewline ' failed ('		
				Write-Host -NoNewline $_.split("\t")[6]
				Write-Output ' flaky)'
				$end_time = Get-Date -UFormat %s
				$duration = $end_time - $start_time
				Write-Host -NoNewline '%         completed tests from object '
				Write-Host -NoNewline $_.split("\t")[1]
				if ($duration -eq 1) {
					Write-Output (" in " + $duration + " second")
				} else {
					Write-Output (" in " + $duration + " seconds")
				}
			}
		}
		Write-Host -NoNewline '%         clause coverage '
		(Get-Content -Path $results/$name.totals | Select-String -Pattern '^coverage' -CaseSensitive -Raw).split("\t")[1]
	} elseif ($tests_exit -eq 5) {
		if ($o -eq "verbose") {
			Write-Output "%         broken"
		}
		Ensure-Format-Report $unit (Split-Path -Path $unit -Leaf -Resolve) "Broken"
	} elseif ($tests_exit -eq 137) {
		if ($o -eq "verbose") {
			Write-Output "%         timeout"
		}
		Add-Content -Path $results/$name.errors -Value "LOGTALK_TIMEOUT"
		Ensure-Format-Report $unit (Split-Path -Path $unit -Leaf -Resolve) "Timeout"
	} elseif ($tests_exit -ne 0) {
		if ($o -eq "verbose") {
			Write-Output "%         crash"
		}
		Add-Content -Path $results/$name.errors -Value "LOGTALK_CRASH"
		Ensure-Format-Report $unit (Split-Path -Path $unit -Leaf -Resolve) "Crash"
	}
	if ($c -eq "xml") {
		if (Test-Path $env:LOGTALKUSER) {
			Copy-Item -Path $env:LOGTALKUSER\tools\lgtunit\coverage_report.dtd" -Destination .
			Copy-Item -Path $env:LOGTALKUSER\tools\lgtunit\coverage_report.xsl" -Destination .
		} elseif (Test-Path $env:LOGTALKHOME) {
			Copy-Item -Path $env:LOGTALKHOME\tools\lgtunit\coverage_report.dtd" -Destination .
			Copy-Item -Path $env:LOGTALKHOME\tools\lgtunit\coverage_report.xsl" -Destination .
		}
	}
	return 0
}

Function Run-Tests() {
param(
	[Parameter(Position = 0)]
	[String]$name,
	[Parameter(Position = 1)]
	[String]$goal
)
	if ($a -eq "") {
		if ($t -ne 0) {
			& $timeout_command $timeout $logtalk $logtalk_option (" `"" + $goal + "`"") > "$results/$name.results" 2> "$results/$name.errors"
		} else {
			& $logtalk $logtalk_option (" `"" + $goal + "`"") > "$results/$name.results" 2> "$results/$name.errors"
		}
	} else {
		if ($t -ne 0) {
			& $timeout_command $timeout $logtalk $logtalk_option (" `"" + $goal + "`"") -- (-Split $a) > "$results/$name.results" 2> "$results/$name.errors"
		} else {
			& $logtalk $logtalk_option (" `"" + $goal + "`"") -- (-Split $a) > "$results/$name.results" 2> "$results/$name.errors"
		}
	}
	if ($LASTEXITCODE -eq 0 -and
		!(Select-String -Path $results/name.results -Pattern "(not applicable)" -SimpleMatch -Quiet) -and
		!(Select-String -Path $results/name.total -Pattern "^object" -Quiet) -and
		!(Select-String -Path $results/name.results -Pattern "tests skipped" -SimpleMatch -Quiet)) {
		Add-Content -Path $results/$name.errors -Value "LOGTALK_BROKEN"
		return 5
	}
	return $LASTEXITCODE
}

Function Ensure-Format-Report() {
param(
	[Parameter(Position = 0)]
	[String]$directory,
	[Parameter(Position = 1)]
	[String]$name,
	[Parameter(Position = 2)]
	[String]$error
)
	$short = $directory -replace $prefix, ""
	if ($format -eq "xunit") {
		$timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
		New-Item -Path . -Name $directory/xunit_report.xml -ItemType "file" -Force > $null
		Add-Content -Path $directory/xunit_report.xml -Value "<?xml version=`"1.0`" encoding=`"UTF-8`"?>"
		Add-Content -Path $directory/xunit_report.xml -Value "<testsuites>"
		Add-Content -Path $directory/xunit_report.xml -Value "<testsuite package=`"$short/`" name=`"$short/tests.lgt`" tests=`"0`" errors=`"1`" failures=`"0`" skipped=`"0`" time=`"0`" timestamp=`"$timestamp`" id=`"0`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<testcase classname=`"tests`" name=`"$name`" time=`"0`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<failure message=`"$error`" type=`"$error`">$error</failure>"
		Add-Content -Path $directory/xunit_report.xml -Value "</testcase>"
		Add-Content -Path $directory/xunit_report.xml -Value "</testsuite>"
		Add-Content -Path $directory/xunit_report.xml -Value "</testsuites>"
	} elseif ($format -eq "xunit_net_v2") {
		run_date=$(date +"%Y-%m-%d")
		run_time=$(date +"%H:%M:%S")
		New-Item -Path . -Name $directory/xunit_report.xml -ItemType "file" -Force > $null
		Add-Content -Path $directory/xunit_report.xml -Value "<?xml version=`"1.0`" encoding=`"UTF-8`"?>"
		Add-Content -Path $directory/xunit_report.xml -Value "<assemblies>"
		Add-Content -Path $directory/xunit_report.xml -Value "<assembly name=`"$short/tests.lgt::tests`" con}g-file=`"$short/tests.lgt`" test-framework=`"lgtunit`" run-date=`"$run_date`" run-time=`"$run_time`" time=`"0`" total=`"0`" errors=`"1`" failures=`"1`" skipped=`"0`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<errors>"
		Add-Content -Path $directory/xunit_report.xml -Value "<error type=`"$error`" name=`"$error`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<failure exception-type=`"$error`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<message><![CDATA[$error]]></message>"
		Add-Content -Path $directory/xunit_report.xml -Value "</failure>"
		Add-Content -Path $directory/xunit_report.xml -Value "</error>"
		Add-Content -Path $directory/xunit_report.xml -Value "</errors>"
		# hack for Allure ignoring the "errors" tag
		Add-Content -Path $directory/xunit_report.xml -Value "<collection name=`"$short/tests.lgt::tests`" time=`"0`" total=`"1`" passed=`"0`" failed=`"1`" skipped=`"0`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<test name=`"$name::tests`" type=`"$short/tests.lgt::tests`" method=`"$name`" time=`"0`" result=`"Fail`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<failure exception-type=`"$error`">"
		Add-Content -Path $directory/xunit_report.xml -Value "<message><![CDATA[$error]]></message>"
		Add-Content -Path $directory/xunit_report.xml -Value "</failure>"
		Add-Content -Path $directory/xunit_report.xml -Value "</test>"
		Add-Content -Path $directory/xunit_report.xml -Value "</collection>"
		Add-Content -Path $directory/xunit_report.xml -Value "</assembly>"
		Add-Content -Path $directory/xunit_report.xml -Value "</assemblies>"
	} elseif ($format -eq "tap") {
		New-Item -Path . -Name $directory/tap_report.xml -ItemType "file" -Force > $null
		Add-Content -Path $directory/tap_report.xml -Value "TAP version 13"
		Add-Content -Path $directory/tap_report.xml -Value "Bail out! $unit $error"
	}
}

Function Write-Usage-Help() {
	$myFullName = $MyInvocation.ScriptName
	$myName = Split-Path -Path $myFullName -leaf -Resolve 

	Write-Output ""
	Write-Output "This script automates running unit tests found in the current directory and"
	Write-Output "recursively in its sub-directories by scanning by default for `"tester.lgt`""
	Write-Output "and `"tester.logtalk`" source files. In case of failed unit tests or test set"
	Write-Output "errors, this script returns a non-zero exit code. When a `"tester.sh`" file"
	Write-Output "exists in the tests directory, the file is sourced before running the tests."
	Write-Output "The `"tester.sh`" file is sourced with all the parameters passed to the script."
	Write-Output ""
	Write-Output "Usage:"
	Write-Output ("  " + $myName + " -p prolog [-o output] [-m mode] [-f format] [-d results] [-t timeout] [-n driver] [-s prefix] [-b tracker] [-u url] [-c report] [-l level] [-i options] [-g goal] [-r seed] [-w] [-a arguments]")
	Write-Output ("  " + $myName + " -v")
	Write-Output ("  " + $myName + " -h")
	Write-Output ""
	Write-Output "Required arguments:"
	Write-Output "  -p backend Prolog compiler"
	Write-Output "     (valid values are b, ciao, cx, eclipse, gnu, ji, lvm, scryer, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	Write-Output ""
	Write-Output "Optional arguments:"
	Write-Output ("  -o output (valid values are verbose and minimal; default is " + $o + ")")
	Write-Output ("  -m compilation mode (default is " + $m + ")")
	Write-Output "     (valid values are optimal, normal, debug, and all)"
	Write-Output ("  -f format for writing the test results (default is " + $f + ")")
	Write-Output "     (valid values are default, tap, xunit, and xunit_net_v2)"
	Write-Output "  -d directory to store the test logs (default is ./logtalk_tester_logs)"
	Write-Output ("  -t timeout in seconds for running each test set (default is " + $t + "; i.e. disabled)")
	Write-Output ("  -n name of the test driver and sourced files (minus file name extensions; default is " + $n + ")")
	Write-Output ("  -s suppress path prefix (default is " + $s + ")")
	Write-Output "  -b bug report server (valid values are github and gitlab; no default)"
	Write-Output "  -u base URL to generate links to test files (no default)"
	Write-Output ("  -c code coverage report (default is " + $c + ")")
	Write-Output "     (valid values are none and xml)"
	Write-Output "  -l directory depth level to look for test sets (default is to recurse into all sub-directories)"
	Write-Output "     (level 1 means current directory only)"
	Write-Output "  -i integration script command-line options (no default)"
	Write-Output ("  -g initialization goal (default is " + $g + ")")
	Write-Output "  -r random generator starting seed (no default)"
	Write-Output "  -w wipe default scratch directories (./.lgt_tmp and ./lgt_tmp) before running a test set"
	Write-Output "     (this option should not be used when running parallel proccess that use the same test sets)"
	Write-Output "  -a arguments wrapped as a string to be passed to the tests (no default)"
	Write-Output "  -v print version"
	Write-Output "  -h help"
	Write-Output ""
}

Function Check-Parameters() {

	if ($v -eq $true) {
		Write-Script-Version
		Exit
	}

	if ($h -eq $true) {
		Write-Usage-Help
		Exit
	}

	if ($p -eq "") {
		Write-Output ("Error! Backend Prolog compiler not specified!")
		Write-Usage-Help
		Exit 1
	} elseif ($p -eq "b") {
		$script:backend = 'b'
		$script:prolog = 'B-Prolog'
		$script:logtalk = "bplgt"
		$script:logtalk_option = "-g"
	} elseif ($p -eq "ciao") {
		$script:backend = 'ciao'
		$script:prolog = 'Ciao Prolog'
		$script:logtalk = "ciaolgt"
		$script:logtalk_option = "-e"
	} elseif ($p -eq "cx") {
		$script:backend = 'cx'
		$script:prolog = 'CxProlog'
		$script:logtalk = "cxlgt"
		$script:logtalk_option = "--goal"
	} elseif ($p -eq "eclipse") {
		$script:backend = eclipse
		$script:prolog = 'ECLiPSe'
		$script:logtalk = "eclipselgt"
		$script:logtalk_option = "-e"
	} elseif ($p -eq "gnu") {
		$script:backend = 'gnu'
		$script:prolog = 'GNU Prolog'
		$script:logtalk = "gplgt"
		$script:logtalk_option = "--query-goal"
	} elseif ($p -eq "ji") {
		$script:backend = 'ji'
		$script:prolog = 'JIProlog'
		$script:logtalk = "jiplgt"
		$script:logtalk_option = "-n -g"
	} elseif ($p -eq "lvm") {
		$script:backend = 'lvm'
		$script:prolog = 'LVM'
		$script:logtalk = "lvmlgt"
		$script:logtalk_option = "-g"
		$script:dot = "."
	} elseif ($p -eq "scryer") {
		$script:backend = 'scryer'
		$script:prolog = 'Scryer Prolog'
		$script:logtalk = "scryerlgt"
		$script:logtalk_option = "-g"
	} elseif ($p -eq "sicstus") {
		$script:backend = 'sicstus'
		$script:prolog = 'SICStus Prolog'
		$script:logtalk = "sicstuslgt"
		$script:logtalk_option = "--goal"
		$script:dot = "."
	} elseif ($p -eq "swi") {
		$script:backend = 'swi'
		$script:prolog = 'SWI-Prolog'
		$script:logtalk = "swilgt"
		$script:logtalk_option = "-g"
	} elseif ($p -eq "swipack") {
		$script:backend = 'swipack'
		$script:prolog = 'SWI-Prolog'
		$script:logtalk = "swipl"
		$script:logtalk_option = "-g"
	} elseif ($p -eq "tau") {
		$script:backend = 'tau'
		$script:prolog = 'Tau Prolog'
		$script:logtalk = "taulgt"
		$script:logtalk_option = "-g"
		$script:dot = "."
	} elseif ($p -eq "trealla") {
		$script:backend = 'trealla'
		$script:prolog = 'Trealla Prolog'
		$script:logtalk = "tplgt"
		$script:logtalk_option = "-g"
	} elseif ($p -eq "xsb") {
		$script:backend = 'xsb'
		$script:prolog = 'XSB'
		$script:logtalk = "xsblgt"
		$script:logtalk_option = "-e"
		$script:dot = "."
	} elseif ($p -eq "yap") {
		$script:backend = 'yap'
		$script:prolog = 'YAP'
		$script:logtalk = "yaplgt"
		$script:logtalk_option = "-g"
	} else {
		Write-Output ("Error! Unsupported backend Prolog compiler: " + $p)
		Write-Usage-Help
		Exit
	}

	if ($o -ne "verbose" -and $o -ne "minimal") {
		Write-Output ("Error! Unknown output verbosity: " + $o)
		Write-Usage-Help
		Exit 1
	}

	if ($m -ne "optimal" -and $m -ne "normal" -and $m -ne "debug" -and $m -ne "all") {
		Write-Output ("Error! Unknown compilation mode: " + $m)
		Write-Usage-Help
		Exit 1
	}

	if ($f -eq "default") {
		$script:format_goal = $format_default_goal
	} elseif ($f -eq "tap") {
		$script:format_goal = $format_tap_goal
	} elseif ($f -eq "xunit") {
		$script:format_goal = $format_xunit_goal
	} elseif ($f -eq "xunit_net_v2") {
		$script:format_goal = $format_xunit_net_v2_goal
	} else {
		Write-Output ("Error! Unknown format: " + $f)
		Write-Usage-Help
		Exit 1
	}

	if ($c -eq "none") {
		$script:coverage_goal = $coverage_default_goal
	} elseif ($c -eq "xml") {
		$script:coverage_goal = $coverage_xml_goal
	} else {
		Write-Output ("Error! Unknown coverage report: " + $c)
		Write-Usage-Help
		Exit 1
	}

	if ($b -ne "") {
		if ($b -contains ":") {
			$script:issue_array = $b.Split(":")
			$script:issue_server = $issue_array[0]
			$script:issue_labels = $issue_array[1]
		} else {
			$script:issue_server = $b
		}
		if ($script:issue_server -ne "github" -and $script:issue_server -ne "gitlab") {
			Write-Output ("Error! Issue tracker server must be either github or gitlab: " + $b)
			Write-Usage-Help
			Exit 1
		}		
	}

	if ($l -ne "") {
		if ($l -is [int] -and $l -ge 1) {
			$script:level = $l - 1
		} else {
			Write-Output ("Error! Level must be an integer equal or greater than 1: " + $l)
			Write-Usage-Help
			Exit 1
		}
	} else {
		$script:level = 999
	}

	if ($g -ne "") {
		$script:initialization_goal = $g
	}

	if ($r -ne "") {
		$script:seed_goal = ("logtalk_load(arbitrary(loader)),type::set_seed(" + $r + ")")
	} else {
		$script:seed_goal = "true"
	}

	if ($d -ne "") {
		$script:results = $d -replace '\\', '/'
	}

	if ($s -ne "") {
		$script:prefix = $s -replace '\\', '/'
	}

}

###################### here it starts ############################ 

# default argument values

$backend = "swi"
$prolog = "SWI-Prolog"
$logtalk = "swilgt"
$logtalk_option = "-g"
$dot = ""
$base = $pwd

$level = 999

$flag_goal = "true"
$initialization_goal = "true"

$issue_server = ""
$issue_labels = "bug"

$format_default_goal = "true"
$format_tap_goal = "logtalk_load(lgtunit(tap_report))"
$format_xunit_goal = "logtalk_load(lgtunit(xunit_report))"
$format_xunit_net_v2_goal = "logtalk_load(lgtunit(xunit_net_v2_report))"
$format_goal = $format_default_goal

$coverage_default_goal = "true"
$coverage_xml_goal = "logtalk_load(lgtunit(coverage_report))"
$coverage_goal = $coverage_default_goal

$results = (Join-Path $pwd "logtalk_tester_logs") -replace '\\', '/'
$prefix = $env:USERPROFILE -replace '\\', '/'

Check-Parameters

if ($p -eq "swipack") {
	$initialization_goal = ("use_module(library(logtalk))," + $initialization_goal)
}

$versions_goal = ("logtalk_load(library(tester_versions)),halt" + $dot)

$tester_optimal_goal = ("set_logtalk_flag(optimize,on),logtalk_load(" + $n + "),halt" + $dot)
$tester_normal_goal = ("logtalk_load(" + $n + "),halt" + $dot)
$tester_debug_goal = ("set_logtalk_flag(debug,on),logtalk_load(" + $n + "),halt" + $dot)

New-Item -Path $results -ItemType directory -Force > $null

if (Test-Path $results/*.results) {
	Remove-Item -Path $results/*.results -Force
}
if (Test-Path $results/*.errors) {
	Remove-Item -Path $results/*.errors -Force
}
if (Test-Path $results/*.totals) {
	Remove-Item -Path $results/*.totals -Force
}
if (Test-Path $results/errors.all) {
	Remove-Item -Path $results/errors.all -Force
}
if (Test-Path $results/tester_versions.txt) {
	Remove-Item -Path $results/tester_versions.txt -Force
}

if ($o -eq "verbose") {
	$start_date = Get-Date -Format "yyyy-MM-dd-HH:mm:ss"
	Write-Output "% Batch testing started @ $start_date"
	& $logtalk $logtalk_option (" `"" + $versions_goal + "`"") > $results/tester_versions.txt 2> $null
	Select-String -Path $results/tester_versions.txt -Pattern "Logtalk version:" -Raw -SimpleMatch
	(Select-String -Path $results/tester_versions.txt -Pattern "Prolog version:" -Raw -SimpleMatch) -replace "Prolog", $prolog
}

$testsets = 0

if ($l -eq "") {
	if ($o -eq "verbose") {
		Get-ChildItem -Path $base\* -Include ($n + ".lgt") ($n + ".logtalk") -Recurse |
		Foreach-Object {
			$testsets++
			Run-TestSet $_.FullName
		}
	} else {
		$counter = 1
		Get-ChildItem -Path $base\* -Include ($n + ".lgt") ($n + ".logtalk") -Recurse |
		Foreach-Object {
			$testsets++
			Write-Host -NoNewline "% running $testsets test sets: "
			Write-Host -NoNewline $counter
			Run-TestSet $_.FullName
			$counter++
			Write-Output "%"
		}
	}
} else {
	if ($o -eq "verbose") {
		Get-ChildItem -Path $base\* -Include ($n + ".lgt") ($n + ".logtalk") -Depth $level |
		Foreach-Object {
			$testsets++
			Run-TestSet $_.FullName
		}
	} else {
		$counter = 1
		Get-ChildItem -Path $base\* -Include ($n + ".lgt") ($n + ".logtalk") -Depth $level |
		Foreach-Object {
			$testsets++
			Write-Host -NoNewline "% running $testsets test sets: "
			Write-Host -NoNewline $counter
			Run-TestSet $_.FullName
			$counter++
			Write-Output "%"
		}
	}
}

if ($testsets -eq 0) {
	Write-Output "%"
	Write-Output "% 0 test sets: 0 completed, 0 skipped, 0 broken, 0 timedout, 0 crashed"
	Write-Output "% 0 tests: 0 skipped, 0 passed, 0 failed"
	Exit 0
}

Push-Location $d

$testsetskipped = (Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'tests skipped').count + (Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '(not applicable)').count

$timeouts = (Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern 'LOGTALK_TIMEOUT').count
$crashed =  (Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern 'LOGTALK_CRASH').count
$broken =   (Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'LOGTALK_BROKEN').count

$testsetruns = $testsets - $testsetskipped - $timeouts - $crashed - $broken


$skipped = 0
$passed = 0
$failed = 0
$flaky = 0

Get-ChildItem -Path . -Filter .\*.totals |
Foreach-Object {
	if ($_ | Select-String -Pattern '^object' -CaseSensitive -Quiet) {
		$skipped = $skipped + [int]($_.split("\t")[3])
		$passed =  $passed  + [int]($_.split("\t")[4])
		$failed =  $failed  + [int]($_.split("\t")[5])
		$flaky =   $flaky   + [int]($_.split("\t")[6])
	}
}

$total = $skipped + $passed + $failed

if ((Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern '^!' -CaseSensitive -Quiet) -or
	(Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '^!' -CaseSensitive -Quiet) -or
	(Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern '^\*' -CaseSensitive -Quiet) -or
	(Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '^\*' -CaseSensitive -Quiet)) {
	Write-Output "%"
	Write-Output "% Compilation errors/warnings and failed unit tests"
	Write-Output "% (compilation errors/warnings might be expected depending on the test)"
	Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern '^!'  | Tee-Object -FilePath errors.all -Append
	Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '^!'  | Tee-Object -FilePath errors.all -Append
	Get-ChildItem -Path . -Filter *.errors  | Select-String -Pattern '^\*' | Tee-Object -FilePath errors.all -Append
	Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '^\*' | Tee-Object -FilePath errors.all -Append
}
if ((Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'tests skipped' -CaseSensitive -SimpleMatch -Quiet) -or
	(Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '(not applicable)' -CaseSensitive -SimpleMatch -Quiet)) {
	Write-Output "%"
	Write-Output "% Skipped test sets"
	(((Get-ChildItem -Path . -Filter *.results | Select-String -Pattern 'tests skipped' -Raw -SimpleMatch) -replace '% tests skipped', '') -replace '__', '/') -replace $prefix, ''
	(((Get-ChildItem -Path . -Filter *.results | Select-String -Pattern '(not applicable)' -Raw -SimpleMatch) -replace '(not applicable)', '') -replace '__', '/') -replace $prefix, ''
}
if (Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern 'LOGTALK_BROKEN' -CaseSensitive -SimpleMatch -Quiet) {
	Write-Output "%"
	Write-Output "% Broken"
	Get-ChildItem -Path . -Filter *.errors |
	Foreach-Object {
		if (Select-String -Path $_.FullName -Pattern 'LOGTALK_BROKEN' -CaseSensitive -SimpleMatch -Quiet) {
			(($_.BaseName -replace '___', ':') -replace '__', '\') -replace $prefix, ''
		}
	}
}
if (Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern 'LOGTALK_TIMEOUT' -SimpleMatch -CaseSensitive -Quiet) {
	Write-Output "%"
	Write-Output "% Timedout"
	Get-ChildItem -Path . -Filter *.errors |
	Foreach-Object {
		if (Select-String -Path $_.FullName -Pattern 'LOGTALK_TIMEOUT' -CaseSensitive -SimpleMatch -Quiet) {
			(($_.BaseName -replace '___', ':') -replace '__', '\') -replace $prefix, ''
		}
	}
}
if (Get-ChildItem -Path . -Filter *.errors | Select-String -Pattern 'LOGTALK_CRASH' -CaseSensitive -SimpleMatch -Quiet) {
	Write-Output "%"
	Write-Output "% Crashed"
	Get-ChildItem -Path . -Filter *.errors |
	Foreach-Object {
		if (Select-String -Path $_.FullName -Pattern 'LOGTALK_CRASH' -CaseSensitive -SimpleMatch -Quiet) {
			(($_.BaseName -replace '___', ':') -replace '__', '\') -replace $prefix, ''
		}
	}
}
if (Get-ChildItem -Path . -Filter *.totals | Select-String -Pattern '^skipped' -CaseSensitive -Quiet) {
	Write-Output "%"
	Write-Output "% Skipped tests"
	Get-ChildItem -Path . -Filter *.totals |
	Foreach-Object {
		Get-Content $_ | ForEach-Object {
			if (Select-String -Pattern '^skipped' -CaseSensitive) {
				(($_.split("\t")[2] + $_.split("\t")[3]) -replace $prefix, '') -replace "\t", " - "
			}
		}
	}
}
if (Get-ChildItem -Path . -Filter *.totals | Select-String -Pattern '^failed' -CaseSensitive -Quiet) {
	Write-Output "%"
	Write-Output "% Failed tests"
	Get-ChildItem -Path . -Filter *.totals |
	Foreach-Object {
		Get-Content $_ | ForEach-Object {
			if (Select-String -Pattern '^failed' -CaseSensitive) {
				(($_.split("\t")[1] + $_.split("\t")[2]) -replace $prefix, '') -replace "\t", " - "
			}
		}
	}
}
Write-Output "%"
Write-Output ("% " + $testsets + " test sets: " + $testsetruns + " completed, " + $testsetskipped + " skipped, " + $broken + " broken, " + $timeouts + " timedout, " + $crashed + " crashed")
Write-Output ("% " + $total + " tests: " + $skipped + " skipped, " + $passed + " passed, " + $failed + " failed (" + $flaky + " flaky)")

if ($o -eq "verbose") {
	$end_date = Get-Date -Format "yyyy-MM-dd-HH:mm:ss"
	Write-Output "%"
	Write-Output ("% Batch testing ended @ " + $end_date)
}

Pop-Location

if ($crashed -gt 0) {
	Exit 7
} elseif ($broken -gt 0) {
	Exit 5
} elseif ($timeouts -gt 0) {
	Exit 3
} elseif (($failed - $flaky) -gt 0) {
	Exit 1
} else {
	Exit 0
}
