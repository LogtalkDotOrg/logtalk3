#!/usr/bin/env bash

#############################################################################
##
##   Unit testing automation script
##   Last updated on March 23, 2025
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

# loosely based on a unit test automation script contributed by Parker Jones

set -o pipefail

function cleanup {
	pkill -9 -P $$
}
trap cleanup EXIT

print_version() {
	echo "$(basename "$0") 20.1"
	exit 0
}

operating_system=$(uname -s)

if [ "${operating_system:0:10}" == "MINGW32_NT" ] || [ "${operating_system:0:10}" == "MINGW64_NT" ] ; then
	# assume that we're running on Windows using the Git for Windows bash shell
	extension='.sh'
elif [ "$LOGTALKHOME" != "" ] && [ "$LOGTALKUSER" != "" ] && [ "$LOGTALKHOME" == "$LOGTALKUSER" ] ; then
	# assume that we're running Logtalk without using the installer scripts
	extension='.sh'
else
	extension=''
fi

# first, make sure that we don't peak Windows own timeout command, which is not usable for this purpose
if [[ "$(command -v timeout)" == *"System32"* ]] || [[ "$(command -v timeout)" == *"system32"* ]] ; then
	timeout_command=""
# second, look for GNU coreutils package timeout command
elif [ -x "$(command -v timeout)" ] && [[ "$(timeout --version)" == *"GNU coreutils"* ]] ; then
	timeout_command="timeout --foreground -s 9 -k 1.0s"
elif [ -x "$(command -v gtimeout)" ] && [[ "$(gtimeout --version)" == *"GNU coreutils"* ]] ; then
	timeout_command="gtimeout --foreground -s 9 -k 1.0s"
else
	timeout_command=""
fi

# default argument values

declare allargs
allargs="$@"
driver='tester'
dot=""
output='verbose'
if [ "${operating_system:0:10}" == "MINGW32_NT" ] || [ "${operating_system:0:10}" == "MINGW64_NT" ] ; then
	# assume that we're running on Windows using the Git for Windows bash shell and get the
	# current directory path using forward slashes for better Prolog backend compatibility
	base="$(pwd -W)"
else
	base="$PWD"
fi
level=""
exclude=""
results="$base/logtalk_tester_logs"
mode='normal'
format='default'
coverage='none'
flag_goal="true"
initialization_goal="true"
wipe='false'
# disable timeouts to maintain backward compatibility
timeout=0
prefix="$HOME/"
issue_server=""
issue_labels="bug"
url=""

# use GNU sed if available instead of BSD sed
if gsed --version >/dev/null 2>&1 ; then
	sed="gsed"
else
	sed="sed"
fi

format_default_goal="true"
format_tap_goal="logtalk_load(lgtunit(tap_report))"
format_xunit_goal="logtalk_load(lgtunit(xunit_report))"
format_xunit_net_v2_goal="logtalk_load(lgtunit(xunit_net_v2_report))"
format_goal=$format_default_goal

coverage_default_goal="true"
coverage_xml_goal="logtalk_load(lgtunit(coverage_report))"
coverage_goal=$coverage_default_goal

run_testset() {
	start_time="$(date +%s)"
	unit=$(dirname "$1")
	unit_short=${unit#$prefix}
	cd "$unit" || exit 1
	if [ "$wipe" == 'true' ] ; then
		rm -rf ./.lgt_tmp
		rm -rf ./lgt_tmp
	fi
	if [ "$output" == 'verbose' ] ; then
		echo "%"
		echo "% $unit_short"
	fi
	if [ -f "$driver.sh" ] ; then
		source "./$driver.sh" $allargs
		source_exit=$?
		if [ "$source_exit" -gt 0 ] ; then
			echo "%         source $driver.sh returned code $source_exit"
			exit 9
		fi
	fi
	# convert any forward slashes so that the derived file name is usable
	name=${unit////__}
	# also convert any colon if running on Windows systems so that the derived file name is usable
	name=${name/:/__}
	report_goal="logtalk_load(lgtunit(automation_report)),set_logtalk_flag(test_results_directory,'$results'),set_logtalk_flag(test_unit_name,'$name')"
	if [ "$prefix" != "" ] ; then
		flag_goal="set_logtalk_flag(suppress_path_prefix,'$prefix')"
	else
		flag_goal="true"
	fi
	if [ "$issue_server" != "" ] ; then
		flag_goal="logtalk_load(issue_creator(loader)),set_logtalk_flag(issue_server,'$issue_server'),set_logtalk_flag(issue_labels,'$issue_labels'),$flag_goal"
	fi
	if [ "$url" != "" ] ; then
		flag_goal="set_logtalk_flag(tests_base_url,'$url'),$flag_goal"
	fi
	if [[ "$format" != "default" || "$coverage" != "none" ]] ; then
		flag_goal="set_logtalk_flag(tests_report_directory,'$unit/'),$flag_goal"
	fi
	if [ $mode == 'optimal' ] || [ $mode == 'all' ] ; then
		run_tests "$name" "$initialization_goal,$report_goal,$format_goal,$coverage_goal,$flag_goal,$seed_goal,$tester_optimal_goal"
		tests_exit=$?
		mode_prefix="% (opt)   "
	elif [ $mode == 'normal' ] || [ $mode == 'all' ] ; then
		run_tests "$name" "$initialization_goal,$report_goal,$format_goal,$coverage_goal,$flag_goal,$seed_goal,$tester_normal_goal"
		tests_exit=$?
		mode_prefix="%         "
	elif [ $mode == 'debug' ] || [ $mode == 'all' ] ; then
		run_tests "$name" "$initialization_goal,$report_goal,$format_goal,$coverage_goal,$flag_goal,$seed_goal,$tester_debug_goal"
		tests_exit=$?
		mode_prefix="% (debug) "
	fi

	if [ "$tests_exit" -eq 0 ] && [ "$output" == 'verbose' ] && grep -q "tests skipped" "$results/$name.results" ; then
		echo "%         skipped"
	elif [ "$tests_exit" -eq 0 ] && [ "$output" == 'verbose' ] && grep -q "(not applicable)" "$results/$name.results" ; then
		echo "%         not applicable"
	elif [ "$tests_exit" -eq 0 ] && [ -f "$results/$name.totals" ] && [ "$output" == 'verbose' ] ; then
		while read -r line ; do
			echo -n "$mode_prefix"
			echo -n "$(cut -f 3 <<< "$line")"
			echo -n ' tests: '
			echo -n "$(cut -f 4 <<< "$line")"
			echo -n ' skipped, '
			echo -n "$(cut -f 5 <<< "$line")"
			echo -n ' passed, '
			echo -n "$(cut -f 6 <<< "$line")"
			echo -n ' failed ('
			echo -n "$(cut -f 7 <<< "$line")"
			echo ' flaky)'
			duration=$(( $(date +%s) - start_time ))
			echo -n '%         completed tests from object '
			echo -n "$(cut -f 2 <<< "$line")"
			if [ $duration -eq 1 ] ; then
				echo " in $duration second"
			else
				echo " in $duration seconds"
			fi
		done < <(grep '^object' "$results/$name.totals")
		echo -n '%         clause coverage '
		echo "$(grep "^coverage" "$results/$name.totals" | cut -f 2)"
	elif [ "$tests_exit" -eq 5 ] ; then
		if [ "$output" == 'verbose' ] ; then
			echo "%         broken"
		fi
		ensure_format_report "$unit" "$(basename "$unit")" "Broken"
	elif [ "$tests_exit" -eq 137 ] || [ "$tests_exit" -eq 124 ] ; then
		if [ "$output" == 'verbose' ] ; then
			echo "%         timeout"
		fi
		echo "LOGTALK_TIMEOUT" >> "$results/$name.errors"
		ensure_format_report "$unit" "$(basename "$unit")" "Timeout"
	elif [ "$tests_exit" -ne 0 ] ; then
		if [ "$output" == 'verbose' ] ; then
			echo "%         crash"
		fi
		echo "LOGTALK_CRASH" >> "$results/$name.errors"
		ensure_format_report "$unit" "$(basename "$unit")" "Crash"
	fi
	if [ $coverage == 'xml' ] ; then
		if [ -d "$LOGTALKUSER" ] ; then
			cp "$LOGTALKUSER/tools/lgtunit/coverage_report.dtd" . || true
			cp "$LOGTALKUSER/tools/lgtunit/coverage_report.xsl" . || true
		elif [ -d "$LOGTALKHOME" ] ; then
			cp "$LOGTALKHOME/tools/lgtunit/coverage_report.dtd" . || true
			cp "$LOGTALKHOME/tools/lgtunit/coverage_report.xsl" . || true
		fi
	fi
	return 0
}

run_tests() {
	name="$1"
	goal="$2"
	if [ ${#args[@]} -eq 0 ] ; then
		if [ "$timeout_command" != "" ] && [ $timeout -ne 0 ] ; then
			$timeout_command $timeout $logtalk_call "$goal" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
		else
			$logtalk_call "$goal" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
		fi
	else
		if [ "$timeout_command" != "" ] && [ $timeout -ne 0 ] ; then
			$timeout_command $timeout $logtalk_call "$goal" -- "${args[@]}" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
		else
			$logtalk_call "$goal" -- "${args[@]}" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
		fi
	fi
	exit=$?
	if grep -q "Likely bug in the backend Prolog compiler. Please file a bug report." "$results/$name.errors"; then
		echo "LOGTALK_BROKEN" >> "$results/$name.errors"
		return 5
	elif [ $exit -eq 0 ] && ! grep -q "(not applicable)" "$results/$name.results" && ! grep -q -s "^object" "$results/$name.totals" && ! grep -q "tests skipped" "$results/$name.results"; then
		echo "LOGTALK_BROKEN" >> "$results/$name.errors"
		return 5
	fi
	return $exit
}

ensure_format_report() {
	directory="$1"
	name="$2"
	error="$3"
	short=$(echo "$directory" | $sed "s|^$prefix||")
	if [ "$format" == "xunit" ] ; then
		timestamp=$(date +"%Y-%m-%dT%H:%M:%S")
		echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" > "$directory/xunit_report.xml"
		echo "<testsuites>" >> "$directory/xunit_report.xml"
		echo "<testsuite package=\"$short/\" name=\"$short/tests.lgt\" tests=\"0\" errors=\"1\" failures=\"0\" skipped=\"0\" time=\"0\" timestamp=\"$timestamp\" id=\"0\">" >> "$directory/xunit_report.xml"
		echo "<testcase classname=\"tests\" name=\"$name\" time=\"0\">" >> "$directory/xunit_report.xml"
		echo "<failure message=\"$error\" type=\"$error\">$error</failure>" >> "$directory/xunit_report.xml"
		echo "</testcase>" >> "$directory/xunit_report.xml"
		echo "</testsuite>" >> "$directory/xunit_report.xml"
		echo "</testsuites>" >> "$directory/xunit_report.xml"
	elif [ "$format" == "xunit_net_v2" ] ; then
		run_date=$(date +"%Y-%m-%d")
		run_time=$(date +"%H:%M:%S")
		echo "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" > "$directory/xunit_report.xml"
		echo "<assemblies>" >> "$directory/xunit_report.xml"
		echo "<assembly name=\"$short/tests.lgt::tests\" config-file=\"$short/tests.lgt\" test-framework=\"lgtunit\" run-date=\"$run_date\" run-time=\"$run_time\" time=\"0\" total=\"0\" errors=\"1\" failures=\"1\" skipped=\"0\">" >> "$directory/xunit_report.xml"
		echo "<errors>" >> "$directory/xunit_report.xml"
		echo "<error type=\"$error\" name=\"$error\">" >> "$directory/xunit_report.xml"
		echo "<failure exception-type=\"$error\">" >> "$directory/xunit_report.xml"
		echo "<message><![CDATA[$error]]></message>" >> "$directory/xunit_report.xml"
		echo "</failure>" >> "$directory/xunit_report.xml"
		echo "</error>" >> "$directory/xunit_report.xml"
		echo "</errors>" >> "$directory/xunit_report.xml"
		# hack for Allure ignoring the "errors" tag
		echo "<collection name=\"$short/tests.lgt::tests\" time=\"0\" total=\"1\" passed=\"0\" failed=\"1\" skipped=\"0\">" >> "$directory/xunit_report.xml"
		echo "<test name=\"$name::tests\" type=\"$short/tests.lgt::tests\" method=\"$name\" time=\"0\" result=\"Fail\">" >> "$directory/xunit_report.xml"
		echo "<failure exception-type=\"$error\">" >> "$directory/xunit_report.xml"
		echo "<message><![CDATA[$error]]></message>" >> "$directory/xunit_report.xml"
		echo "</failure>" >> "$directory/xunit_report.xml"
		echo "</test>" >> "$directory/xunit_report.xml"
		echo "</collection>" >> "$directory/xunit_report.xml"
		echo "</assembly>" >> "$directory/xunit_report.xml"
		echo "</assemblies>" >> "$directory/xunit_report.xml"
	elif [ "$format" == "tap" ] ; then
		echo "TAP version 13" > "$directory/tap_report.xml"
		echo "Bail out! $unit $error" >> "$directory/tap_report.xml"
	fi
}

usage_help() {
	echo
	echo "This script automates running unit tests found in the current directory and"
	echo "recursively in its sub-directories by scanning by default for \"tester.lgt\""
	echo "and \"tester.logtalk\" source files. In case of failed unit tests or test set"
	echo "errors, this script returns a non-zero exit code. When a \"tester.sh\" file"
	echo "exists in the tests directory, the file is sourced before running the tests."
	echo "The \"tester.sh\" file is sourced with all the parameters passed to the script."
	echo "The timeout option requires availability of the GNU coreutils timeout command."
	echo
	echo "Usage:"
	echo "  $(basename "$0") -p prolog [-o output] [-m mode] [-f format] [-d results] [-t timeout] [-n driver] [-s prefix] [-b tracker] [-u url] [-c report] [-l level] [-e exclude] [-i options] [-g goal] [-r seed] [-w] [-- arguments]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Required arguments:"
	echo "  -p backend Prolog compiler"
	echo "     (valid values are b, ciao, cx, eclipse, gnu, gnunc, ji, xvm, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -o output (valid values are verbose and minimal; default is $output)"
	echo "  -m compilation mode (default is $mode)"
	echo "     (valid values are optimal, normal, debug, and all)"
	echo "  -f format for writing the test results (default is $format)"
	echo "     (valid values are default, tap, xunit, and xunit_net_v2)"
	echo "  -d directory to store the test logs (default is ./logtalk_tester_logs)"
	echo "  -t timeout in seconds for running each test set (default is $timeout; i.e. disabled)"
	echo "  -n name of the test driver and sourced files (minus file name extensions; default is $driver)"
	echo "  -s suppress path prefix (default is $prefix)"
	echo "  -b bug report server (valid values are github and gitlab; no default; requires -u option)"
	echo "  -u base URL to generate links to test files (no default)"
	echo "  -c code coverage report (default is $coverage)"
	echo "     (valid values are none and xml)"
	echo "  -l directory depth level to look for test sets (default is to recurse into all sub-directories)"
	echo "     (level 1 means current directory only)"
	echo "  -e exclude directories matching a POSIX-extended regular expression"
	echo "  -i integration script command-line options (no default)"
	echo "  -g initialization goal (default is $initialization_goal)"
	echo "  -r random generator starting seed (no default)"
	echo "  -w wipe default scratch directories (./.lgt_tmp and ./lgt_tmp) before running a test set"
	echo "     (this option should not be used when running parallel processes that use the same test sets)"
	echo "  -- arguments to be passed to the tests (no default)"
	echo "  -h help"
	echo
}

while getopts "vp:o:m:f:d:t:n:s:b:u:c:l:e:g:r:i:wh" option; do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		o) o_arg="$OPTARG";;
		m) m_arg="$OPTARG";;
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		n) n_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		b) b_arg="$OPTARG";;
		u) u_arg="$OPTARG";;
		c) c_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		e) e_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		g) g_arg="$OPTARG";;
		r) r_arg="$OPTARG";;
		w) wipe='true';;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

shift $((OPTIND - 1))
args=("$@")

if [ "$p_arg" == "" ] ; then
	echo "Error! Backend Prolog compiler not specified!" >&2
	usage_help
	exit 1
elif [ "$p_arg" == "b" ] ; then
	prolog='B-Prolog'
	logtalk=bplgt$extension
	logtalk_call="$logtalk $i_arg -g"
elif [ "$p_arg" == "ciao" ] ; then
	prolog='Ciao Prolog'
	logtalk=ciaolgt$extension
	logtalk_call="$logtalk $i_arg -e"
elif [ "$p_arg" == "cx" ] ; then
	prolog='CxProlog'
	logtalk=cxlgt$extension
	logtalk_call="$logtalk $i_arg --goal"
elif [ "$p_arg" == "eclipse" ] ; then
	prolog='ECLiPSe'
	logtalk=eclipselgt$extension
	logtalk_call="$logtalk $i_arg -e"
elif [ "$p_arg" == "gnu" ] ; then
	prolog='GNU Prolog'
	logtalk=gplgt$extension
	logtalk_call="$logtalk $i_arg --query-goal"
elif [ "$p_arg" == "gnunc" ] ; then
	prolog='GNU Prolog (native code)'
	logtalk=gplgtnc
	logtalk_call="$logtalk $i_arg --query-goal"
elif [ "$p_arg" == "ji" ] ; then
	prolog='JIProlog'
	logtalk=jiplgt$extension
	logtalk_call="$logtalk $i_arg -n -g"
elif [ "$p_arg" == "xvm" ] ; then
	prolog='XVM'
	logtalk=xvmlgt$extension
	logtalk_call="$logtalk $i_arg -g"
	case "$i_arg" in
		*"--standard-top-level"*) dot=".";;
		*"--custom-top-level"*) dot="?";;
		*) dot=".";;
	esac
elif [ "$p_arg" == "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk=sicstuslgt$extension
	logtalk_call="$logtalk $i_arg --goal"
	dot="."
elif [ "$p_arg" == "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk=swilgt$extension
	logtalk_call="$logtalk $i_arg -g"
elif [ "$p_arg" == "swipack" ] ; then
	prolog='SWI-Prolog (pack)'
	logtalk=swipl
	logtalk_call="$logtalk $i_arg -g"
elif [ "$p_arg" == "tau" ] ; then
	prolog='Tau Prolog'
	logtalk=taulgt$extension
	logtalk_call="$logtalk $i_arg -g"
	dot="."
elif [ "$p_arg" == "trealla" ] ; then
	prolog='Trealla Prolog'
	logtalk=tplgt$extension
	logtalk_call="$logtalk $i_arg -g"
elif [ "$p_arg" == "xsb" ] ; then
	prolog='XSB'
	logtalk=xsblgt$extension
	logtalk_call="$logtalk $i_arg -e"
	dot="."
elif [ "$p_arg" == "yap" ] ; then
	prolog='YAP'
	logtalk=yaplgt$extension
	logtalk_call="$logtalk $i_arg -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported backend Prolog compiler: $p_arg" >&2
	usage_help
	exit 1
elif [ ! "$(command -v $logtalk)" ] ; then
	echo "Error! $logtalk integration script for $prolog not found." >&2
	echo "       Check that its directory is in your execution path." >&2
	exit 1
fi

if [ "$o_arg" == "verbose" ] ; then
	output='verbose'
elif [ "$o_arg" == "minimal" ] ; then
	output='minimal'
elif [ "$o_arg" != "" ] ; then
	echo "Error! Unknown output verbosity: $o_arg" >&2
	usage_help
	exit 1
fi

if [ "$m_arg" == "optimal" ] ; then
	mode='optimal'
elif [ "$m_arg" == "normal" ] ; then
	mode='normal'
elif [ "$m_arg" == "debug" ] ; then
	mode='debug'
elif [ "$m_arg" == "all" ] ; then
	mode='all'
elif [ "$m_arg" != "" ] ; then
	echo "Error! Unknown compilation mode: $m_arg" >&2
	usage_help
	exit 1
fi

if [ "$f_arg" == "default" ] ; then
	format='default'
	format_goal=$format_default_goal
elif [ "$f_arg" == "tap" ] ; then
	format='tap'
	format_goal=$format_tap_goal
elif [ "$f_arg" == "xunit" ] ; then
	format='xunit'
	format_goal=$format_xunit_goal
elif [ "$f_arg" == "xunit_net_v2" ] ; then
	format='xunit_net_v2'
	format_goal=$format_xunit_net_v2_goal
elif [ "$f_arg" != "" ] ; then
	echo "Error! Unknown format: $f_arg" >&2
	usage_help
	exit 1
fi

if [ "$c_arg" == "none" ] ; then
	coverage='none'
	coverage_goal=$coverage_default_goal
elif [ "$c_arg" == "xml" ] ; then
	coverage='xml'
	coverage_goal=$coverage_xml_goal
elif [ "$c_arg" != "" ] ; then
	echo "Error! Unknown coverage report: $c_arg" >&2
	usage_help
	exit 1
fi

if [ "$d_arg" != "" ] ; then
	results=$(realpath "$d_arg" 2>/dev/null || echo "$d_arg")
fi

if [ "$t_arg" != "" ] ; then
	timeout="$t_arg"
fi

if [ "$s_arg" != "" ] ; then
	prefix="$s_arg"
fi

if [ "$b_arg" != "" ] ; then
	if [ "$u_arg" == "" ] ; then
		echo "Error! Issue tracker option (-b) requires the base URL (-u) option" >&2
		usage_help
		exit 1
	fi
	issue_server="$(echo "$b_arg" | cut -d':' -f1)"
	if [ "$issue_server" != "github" ] && [ "$issue_server" != "gitlab" ] ; then
		echo "Error! Issue tracker server must be either github or gitlab: $b_arg" >&2
		usage_help
		exit 1
	fi
	labels="$(echo "$b_arg" | cut -d':' -f2)"
	if [ "$labels" != "" ] && [ "$labels" != "$issue_server" ] ; then
		issue_labels="$labels"
	fi
fi

if [ "$u_arg" != "" ] ; then
    if [[ ! "$u_arg" =~ ^https?:// ]] ; then
        echo "Error! Base URL must start with http:// or https://" >&2
        usage_help
        exit 1
    fi
    url="$u_arg"
fi

if [ "$n_arg" != "" ] ; then
	driver="$n_arg"
fi

if [ "$l_arg" != "" ] ; then
	if [[ "$l_arg" =~ ^[1-9][0-9]*$ ]] ; then
		level="-maxdepth $l_arg"
	else
		echo "Error! Level must be an integer equal or greater than 1: $l_arg" >&2
		usage_help
		exit 1
	fi
fi

if [ "$e_arg" != "" ] ; then
	exclude="$e_arg"
fi

if [ "$g_arg" != "" ] ; then
	initialization_goal="$g_arg"
fi

if [ "$r_arg" != "" ] ; then
	seed_goal="logtalk_load(arbitrary(loader)),type::set_seed($r_arg)"
else
	seed_goal="true"
fi

if [ "$p_arg" == "swipack" ] ; then
	initialization_goal="use_module(library(logtalk)),$initialization_goal"
fi

if [ "$timeout_command" == "" ] ; then
	echo "Warning! Timeout support not available. The timeout option will be ignored." >&2
fi

versions_goal="logtalk_load(library(tester_versions)),halt$dot"

tester_optimal_goal="set_logtalk_flag(optimize,on),logtalk_load($driver),halt$dot"
tester_normal_goal="logtalk_load($driver),halt$dot"
tester_debug_goal="set_logtalk_flag(debug,on),logtalk_load($driver),halt$dot"

mkdir -p "$results"
rm -f "$results"/*.results
rm -f "$results"/*.errors
rm -f "$results"/*.totals
rm -f "$results"/errors.all
rm -f "$results"/tester_versions.txt

if [ "$output" == 'verbose' ] ; then
	start_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")
	echo "% Batch testing started @ $start_date"
	$logtalk_call $versions_goal > "$results"/tester_versions.txt 2> /dev/null
	grep -a "Logtalk version:" "$results"/tester_versions.txt
	grep -a "Prolog version:" "$results"/tester_versions.txt | $sed "s/Prolog/$prolog/"
fi

declare drivers
declare testsets
if [ "$exclude" == "" ] ; then
	drivers="$(find -L "$base" $level -type f -name "$driver.lgt" -or -name "$driver.logtalk" | LC_ALL=C sort)"
	testsets=$(find -L "$base" $level -type f -name "$driver.lgt" -or -name "$driver.logtalk" | wc -l | tr -d ' ')
	find_exit=$?
elif [ "$(uname)" == "Darwin" ] ; then
	drivers="$(find -L -E "$base" $level -type f -not -regex "$exclude" -name "$driver.lgt" -or -name "$driver.logtalk" | LC_ALL=C sort)"
	testsets=$(find -L -E "$base" $level -type f -not -regex "$exclude" -name "$driver.lgt" -or -name "$driver.logtalk" | wc -l | tr -d ' ')
	find_exit=$?
else
	drivers="$(find -L "$base" $level -type f -regextype posix-extended -not -regex "$exclude" -name "$driver.lgt" -or -name "$driver.logtalk" | LC_ALL=C sort)"
	testsets=$(find -L "$base" $level -type f -regextype posix-extended -not -regex "$exclude" -name "$driver.lgt" -or -name "$driver.logtalk" | wc -l | tr -d ' ')
	find_exit=$?
fi

if [ "$find_exit" -gt 0 ] ; then
	echo "%         find command returned code $find_exit"
	exit 11
fi

if  [ "$testsets" -eq 0 ] ; then
	echo "%"
	echo "% 0 test sets: 0 completed, 0 skipped, 0 broken, 0 timedout, 0 crashed"
	echo "% 0 tests: 0 skipped, 0 passed, 0 failed"
	exit 0
fi

if [ "$output" == 'verbose' ] ; then
	while read -r file && [ "$file" != "" ]; do
		run_testset "$file"
	done <<< "$drivers"
else
	counter=1
	while read -r file && [ "$file" != "" ]; do
		echo -ne "% running $testsets test sets: "
		echo -ne "$counter"'\r'
		run_testset "$file"
		((counter++))
	done <<< "$drivers"
	echo
fi

cd "$results" || exit 1
testsetskipped=$(cat -- *.results | grep -c 'tests skipped')
testsetskipped=$((testsetskipped+$(cat -- *.results | grep -c '(not applicable)')))
timeouts=$(cat -- *.errors | grep -c 'LOGTALK_TIMEOUT')
crashed=$(cat -- *.errors | grep -c 'LOGTALK_CRASH')
broken=$(cat -- *.errors | grep -c 'LOGTALK_BROKEN')
testsetruns=$((testsets-testsetskipped-timeouts-crashed-broken))

skipped=0
passed=0
failed=0
flaky=0
temp_file=$(mktemp)
grep -s '^object' ./*.totals > "$temp_file"
while read -r line ; do
	skipped=$((skipped+$(cut -f 4 <<< "$line")))
	passed=$((passed+$(cut -f 5 <<< "$line")))
	failed=$((failed+$(cut -f 6 <<< "$line")))
	flaky=$((flaky+$(cut -f 7 <<< "$line")))
done < "$temp_file"
rm -f "$temp_file"
total=$((skipped+passed+failed))

if grep -q -s -a -h '^!' -- *.errors || grep -q -s -a -h '^!' -- *.results || grep -q -s -a -h '^\*' -- *.errors || grep -q -s -a -h '^\*' -- *.results ; then
	echo "%"
	echo "% Compilation errors/warnings and failed unit tests"
	echo "% (compilation errors/warnings might be expected depending on the test)"
	grep -s -a -h '^!' -- *.errors | $sed 's/.errors//' | tee -a errors.all
	grep -s -a -h '^!' -- *.results | $sed 's/.results//' | tee -a errors.all
	grep -s -a -h '^\*' -- *.errors | $sed 's/.errors//' | tee -a errors.all
	grep -s -a -h '^\*' -- *.results | $sed 's/.results//' | tee -a errors.all
fi
if grep -q -s -a 'tests skipped' -- *.results || grep -q -s -a '(not applicable)' -- *.results ; then
	echo "%"
	echo "% Skipped test sets"
	grep -s -a 'tests skipped' -- *.results | $sed 's/% tests skipped//' | $sed 's/.results://' | $sed 's|__|/|g' | $sed "s|^$prefix||"
	grep -s -a '(not applicable)' -- *.results | $sed 's/(not applicable)//' | $sed 's/.results://' | $sed 's|__|/|g' | $sed "s|^$prefix||"
fi
if grep -q -s -a 'LOGTALK_BROKEN' -- *.errors; then
	echo "%"
	echo "% Broken"
	grep -s -a 'LOGTALK_BROKEN' -- *.errors | $sed 's/LOGTALK_BROKEN//' | $sed 's/.errors://' | $sed 's|__|/|g' | $sed "s|^$prefix||"
fi
if grep -q -s -a 'LOGTALK_TIMEOUT' -- *.errors; then
	echo "%"
	echo "% Timedout"
	grep -s -a 'LOGTALK_TIMEOUT' -- *.errors | $sed 's/LOGTALK_TIMEOUT//' | $sed 's/.errors://' | $sed 's|__|/|g' | $sed "s|^$prefix||"
fi
if grep -q -s -a 'LOGTALK_CRASH' -- *.errors; then
	echo "%"
	echo "% Crashed"
	grep -s -a 'LOGTALK_CRASH' -- *.errors | $sed 's/LOGTALK_CRASH//' | $sed 's/.errors://' | $sed 's|__|/|g' | $sed "s|^$prefix||"
fi
if grep -s -q '^skipped' -- *.totals; then
	echo "%"
	echo "% Skipped tests"
	for file in *.totals; do
		if grep -s -q '^skipped' "$file"; then
			grep '^skipped' "$file" | cut -f 2,3 | $sed "s|^$prefix||" | $sed "s|\t| - |"
		fi
	done
fi
if grep -s -q '^failed' -- *.totals; then
	echo "%"
	echo "% Failed tests"
	for file in *.totals; do
		if grep -s -q '^failed' "$file"; then
			grep '^failed' "$file" | cut -f 2,3 | $sed "s|^$prefix||" | $sed "s|\t| - |"
		fi
	done
fi
echo "%"
echo "% $testsets test sets: $testsetruns completed, $testsetskipped skipped, $broken broken, $timeouts timedout, $crashed crashed"
echo "% $total tests: $skipped skipped, $passed passed, $failed failed ($flaky flaky)"

if [ "$output" == 'verbose' ] ; then
	end_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")
	echo "%"
	echo "% Batch testing ended @ $end_date"
fi

if [ "$crashed" -gt 0 ] ; then
	exit 7
elif [ "$broken" -gt 0 ] ; then
	exit 5
elif [ "$timeouts" -gt 0 ] ; then
	exit 3
elif [ "$((failed - flaky))" -gt 0 ] ; then
	exit 1
else
	exit 0
fi
