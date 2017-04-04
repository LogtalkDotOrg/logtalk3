#!/usr/bin/env bash

#############################################################################
## 
##   Unit testing automation script
##   Last updated on April 4, 2017
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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

export LC_ALL=C

print_version() {
	echo "$(basename "$0") 0.24"
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
	timeout_command="timeout -k 1"
elif [ -x "$(command -v gtimeout)" ] && [[ "$(gtimeout --version)" == *"GNU coreutils"* ]] ; then
	timeout_command="gtimeout -k 1"
else
	timeout_command=""
fi

# testing goals

versions_goal="logtalk_load(library(tester_versions)),halt"
versions_goal_dot="logtalk_load(library(tester_versions)),halt."

tester_optimal_goal="set_logtalk_flag(optimize,on),logtalk_load(tester),halt"
tester_optimal_goal_dot="set_logtalk_flag(optimize,on),logtalk_load(tester),halt."

tester_normal_goal="logtalk_load(tester),halt"
tester_normal_goal_dot="logtalk_load(tester),halt."

tester_debug_goal="set_logtalk_flag(debug,on),logtalk_load(tester),halt"
tester_debug_goal_dot="set_logtalk_flag(debug,on),logtalk_load(tester),halt."

format_default_goal="true"
format_tap_goal="logtalk_load(lgtunit(tap_report))"
format_xunit_goal="logtalk_load(lgtunit(xunit_report))"

coverage_default_goal="true"
coverage_xml_goal="logtalk_load(lgtunit(coverage_report))"

# default argument values

base="$PWD"
results="$base/logtalk_tester_logs"
backend=swipl
prolog='SWI-Prolog'
logtalk=swilgt$extension
logtalk_call="$logtalk -g"
mode='normal'
format='default'
coverage='none'
format_goal=$format_default_goal
coverage_goal=$coverage_default_goal
# disable timeouts to maintain backward compatibility
timeout=0
prefix="$HOME/"

run_tests() {
	unit=$(dirname "$1")
	unit_short=${unit#$prefix}
	cd "$unit" || exit 1
	echo '*******************************************************************************'
	echo "***** Testing $unit_short"
	name=${unit////__}
	if [ $mode == 'optimal' ] || [ $mode == 'all' ] ; then
		run_test "$name" "$format_goal,$coverage_goal,$tester_optimal_goal"
		tests_exit=$?
		grep -a 'tests:' "$results/$name.results" | sed 's/%/***** (opt)  /'
	elif [ $mode == 'normal' ] || [ $mode == 'all' ] ; then
		run_test "$name" "$format_goal,$coverage_goal,$tester_normal_goal"
		tests_exit=$?
		grep -a 'tests:' "$results/$name.results" | sed 's/%/*****        /'
	elif [ $mode == 'debug' ] || [ $mode == 'all' ] ; then
		run_test "$name" "$format_goal,$coverage_goal,$tester_debug_goal"
		tests_exit=$?
		grep -a 'tests:' "$results/$name.results" | sed 's/%/***** (debug)/'
	fi
	if [ $tests_exit -eq 0 ] ; then
		grep -a 'out of' "$results/$name.results" | grep -a 'clause coverage' | sed 's/%/*****        /'
		grep -a 'no code coverage information collected' "$results/$name.results" | sed 's/%/*****        /'
		grep -a '(not applicable)' "$results/$name.results" | sed 's/(/*****         (/'
	elif [ $tests_exit -eq 124 ] ; then
		echo "*****         timeout"
		echo "LOGTALK_TIMEOUT" >> "$results/$name.errors"
	else
		echo "*****         crash"
		echo "LOGTALK_CRASH" >> "$results/$name.errors"
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

run_test() {
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
	return $?
}

usage_help()
{
	echo 
	echo "This script automates running unit tests found on the current directory and recursively"
	echo "in its sub-directories by scanning for tester.lgt and tester.logtalk source files. In"
	echo  "case of failed unit tests, this script returns an exit code of 1."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prolog] [-m mode] [-f format] [-d results] [-t timeout] [-s prefix] [-c report] [-- arguments]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -p back-end Prolog compiler (default is $backend)"
	echo "     (possible values are b, cx, eclipse, gnu, ji, lean, qp, sicstus, swi, swipack, xsb, xsbmt, and yap)"
	echo "  -m compilation mode (default is $mode)"
	echo "     (possible values are optimal, normal, debug, and all)"
	echo "  -f format for writing the test results (default is $format)"
	echo "     (possible values are default, tap, and xunit)"
	echo "  -d directory to store the test logs (default is ./logtalk_tester_logs)"
	echo "  -t timeout in seconds for running each test set (default is $timeout; i.e. disabled)"
	echo "  -s supress path prefix (default is $prefix)"
	echo "  -c code coverage report (default is $coverage)"
	echo "     (possible values are none and xml)"
	echo "  -- arguments to be passed to the integration script used to run the tests (no default)"
	echo "  -h help"
	echo
	exit 0
}

while getopts "vp:m:f:d:t:s:c:h" option
do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		m) m_arg="$OPTARG";;
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		c) c_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

shift $((OPTIND - 1))
args=("$@")

if [ "$p_arg" == "b" ] ; then
	prolog='B-Prolog'
	logtalk=bplgt$extension
	logtalk_call="$logtalk -g"
elif [ "$p_arg" == "cx" ] ; then
	prolog='CxProlog'
	logtalk=cxlgt$extension
	logtalk_call="$logtalk --goal"
elif [ "$p_arg" == "eclipse" ] ; then
	prolog='ECLiPSe'
	logtalk=eclipselgt$extension
	logtalk_call="$logtalk -e"
elif [ "$p_arg" == "gnu" ] ; then
	prolog='GNU Prolog'
	logtalk=gplgt$extension
	logtalk_call="$logtalk --query-goal"
elif [ "$p_arg" == "ji" ] ; then
	prolog='JIProlog'
	logtalk=jiplgt$extension
	logtalk_call="$logtalk -n -g"
elif [ "$p_arg" == "lean" ] ; then
	prolog='Lean Prolog'
	logtalk=lplgt$extension
	logtalk_call="$logtalk"
elif [ "$p_arg" == "qp" ] ; then
	prolog='Qu-Prolog'
	logtalk=qplgt$extension
	logtalk_call="$logtalk -g"
	versions_goal=$versions_goal_dot
	tester_optimal_goal=$tester_optimal_goal_dot
	tester_normal_goal=$tester_normal_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" == "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk=sicstuslgt$extension
	logtalk_call="$logtalk --goal"
	versions_goal=$versions_goal_dot
	tester_optimal_goal=$tester_optimal_goal_dot
	tester_normal_goal=$tester_normal_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" == "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk=swilgt$extension
	logtalk_call="$logtalk -g"
elif [ "$p_arg" == "swipack" ] ; then
	prolog='SWI-Prolog'
	logtalk=swipl
	logtalk_call="$logtalk -g"
elif [ "$p_arg" == "xsb" ] ; then
	prolog='XSB'
	logtalk=xsblgt$extension
	logtalk_call="$logtalk -e"
	versions_goal=$versions_goal_dot
	tester_optimal_goal=$tester_optimal_goal_dot
	tester_normal_goal=$tester_normal_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" == "xsbmt" ] ; then
	prolog='XSB-MT'
	logtalk=xsbmtlgt$extension
	logtalk_call="$logtalk -e"
	versions_goal=$versions_goal_dot
	tester_optimal_goal=$tester_optimal_goal_dot
	tester_normal_goal=$tester_normal_goal_dot
	tester_debug_goal=$tester_debug_goal_dot
elif [ "$p_arg" == "yap" ] ; then
	prolog='YAP'
	logtalk=yaplgt$extension
	logtalk_call="$logtalk -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported back-end Prolog compiler: $p_arg"
	usage_help
	exit 1
elif [ ! "$(command -v $backend)" ] ; then
    echo "Error! Default back-end Prolog compiler not found: $prolog"
	usage_help
    exit 1
elif [ ! "$(command -v $logtalk)" ] ; then
    echo "Error! $logtalk integration script for $prolog not found."
	echo "       Check that its directory is in your execution path."
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
	echo "Error! Unknow compilation mode: $m_arg"
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
elif [ "$f_arg" != "" ] ; then
	echo "Error! Unknow format: $f_arg"
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
	echo "Error! Unknow coverage report: $c_arg"
	usage_help
	exit 1
fi

if [ "$p_arg" == "swipack" ] ; then
	versions_goal="use_module(library(logtalk)),$versions_goal"
	format_goal="use_module(library(logtalk)),$format_goal"
fi

if [ "$d_arg" != "" ] ; then
	results="$d_arg"
fi

if [ "$t_arg" != "" ] ; then
	timeout="$t_arg"
fi

if [ "$s_arg" != "" ] ; then
	prefix="$s_arg"
fi

if [ "$timeout_command" == "" ] ; then
	echo "Warning! Timeout support not available. The timeout option will be ignored."
fi

mkdir -p "$results"
rm -f "$results"/*.results
rm -f "$results"/*.errors
rm -f "$results"/errors.all
rm -f "$results"/tester_versions.txt

start_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")

echo '*******************************************************************************'
echo "***** Batch testing started @ $start_date"
$logtalk_call $versions_goal > "$results"/tester_versions.txt 2> /dev/null
grep -a "Logtalk version:" "$results"/tester_versions.txt
grep -a "Prolog version:" "$results"/tester_versions.txt | sed "s/Prolog/$prolog/"

testsets=0
output="$(find "$base" -name "tester.lgt" -or -name "tester.logtalk")"
while read -r file && [ "$file" != "" ]; do
	((testsets++))
	run_tests "$file"
done <<< "$output"

cd "$results" || exit 1
timeouts=$(cat -- *.errors | grep -c 'LOGTALK_TIMEOUT')
crashes=$(cat -- *.errors | grep -c 'LOGTALK_CRASH')
testsetruns=$((testsets-timeouts-crashes))
skipped=$(cat -- *.results | grep -c ': skipped')
passed=$(cat -- *.results | grep -c ': success')
failed=$(cat -- *.results | grep -c ': failure')
total=$((skipped+passed+failed))

echo "*******************************************************************************"
echo "***** Compilation errors/warnings and failed unit tests"
echo "***** (compilation errors/warnings might be expected depending on the test)"
echo "*******************************************************************************"
grep -s -a -A2 'syntax_error' -- *.results | sed 's/.results//' | tee errors.all
grep -s -a -A2 'syntax_error' -- *.errors | sed 's/.errors//' | tee -a errors.all
grep -s -a -h '!     ' -- *.errors | sed 's/.errors//' | tee -a errors.all
grep -s -a -h '!     ' -- *.results | sed 's/.results//' | tee -a errors.all
grep -s -a -h -F '*     ' -- *.errors | sed 's/.errors//' | tee -a errors.all
grep -s -a -h -F '*     ' -- *.results | sed 's/.results//' | tee -a errors.all
echo "*******************************************************************************"
echo "***** Timeouts"
echo "*******************************************************************************"
grep -s -a 'LOGTALK_TIMEOUT' -- *.errors | sed 's/LOGTALK_TIMEOUT//' | sed 's/.errors://' | sed 's|__|/|g' | sed "s|^$prefix||"
echo "*******************************************************************************"
echo "***** Crashes"
echo "*******************************************************************************"
grep -s -a 'LOGTALK_CRASH' -- *.errors | sed 's/LOGTALK_CRASH//' | sed 's/.errors://' | sed 's|__|/|g' | sed "s|^$prefix||"
echo "*******************************************************************************"
echo "***** Skipped tests"
echo "*******************************************************************************"
grep -s -a ': skipped' -- *.results | sed 's/: skipped//' | sed 's/.results:% / - /' | sed 's|__|/|g' | sed "s|^$prefix||" | sed "s|^% ||"
echo "*******************************************************************************"
echo "***** Failed tests"
echo "*******************************************************************************"
grep -s -a ': failure' -- *.results | sed 's/: failure//' | sed 's/.results:!     / - /' | sed 's|__|/|g' | sed "s|^$prefix||" | sed "s|^!     ||"
echo "*******************************************************************************"
echo "***** $testsets test sets: $testsetruns completed, $timeouts timeouts, $crashes crashes"
echo "***** $total tests: $skipped skipped, $passed passed, $failed failed"
echo "*******************************************************************************"

end_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")

echo "***** Batch testing ended @ $end_date"
echo '*******************************************************************************'

if [ "$failed" -eq 0 ] && [ "$timeouts" -eq 0 ] && [ "$crashes" -eq 0 ] ; then
	exit 0
else
	exit 1
fi
