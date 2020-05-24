#!/usr/bin/env bash

#############################################################################
## 
##   Documentation automation script
##   Last updated on May 24, 2020
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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

export LC_ALL=C

print_version() {
	echo "$(basename "$0") 0.5"
	exit 0
}

operating_system=$(uname -s)

if [ "${operating_system:0:10}" == "MINGW32_NT" ] ; then
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

# documenting goals

versions_goal="logtalk_load(library(tester_versions)),halt"
versions_goal_dot="$versions_goal."

documenting_goal="logtalk_load([doclet(loader),doclet]),halt"
documenting_goal_dot="$documenting_goal."

# default argument values

base="$PWD"
results="$base/logtalk_doclet_logs"
backend=swipl
prolog='SWI-Prolog'
logtalk=swilgt$extension
logtalk_call="$logtalk -g"
# disable timeouts to maintain backward compatibility
timeout=0
prefix="$HOME/"

run_doclets() {
	directory=$(dirname "$1")
	directory_short=${directory#$prefix}
	cd "$directory" || exit 1
	echo '*******************************************************************************'
	echo "***** Documenting $directory_short"
	name=${directory////__}
	run_doclet "$name" "$documenting_goal"
	doclet_exit=$?
	if [ $doclet_exit -eq 0 ] ; then
		return 0
	elif [ $doclet_exit -eq 124 ] ; then
		echo "*****         timeout"
		echo "LOGTALK_TIMEOUT" >> "$results/$name.errors"
	else
		echo "*****         crash"
		echo "LOGTALK_CRASH" >> "$results/$name.errors"
	fi
	return 0
}

run_doclet() {
	name="$1"
	goal="$2"
	if [ "$timeout_command" != "" ] && [ $timeout -ne 0 ] ; then
		$timeout_command $timeout $logtalk_call "$goal" -- "$@" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
	else
		$logtalk_call "$goal" -- "$@" < /dev/null > "$results/$name.results" 2> "$results/$name.errors"
	fi
	return $?
}

usage_help()
{
	echo 
	echo "This script automates running doclets found on the current directory and recursively"
	echo "in its sub-directories by scanning for doclet.lgt and doclet.logtalk source files. In"
	echo  "case of failed doclets or doclet errors, this script returns a non-zero exit code."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prolog] [-d results] [-t timeout] [-- arguments]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -p backend Prolog compiler (default is $backend)"
	echo "     (possible values are b, ciao, cx, eclipse, gnu, ji, lean, qp, sicstus, swi, swipack, xsb, xsbmt, and yap)"
	echo "  -d directory to store the doclet logs (default is ./logtalk_doclet_logs)"
	echo "  -t timeout in seconds for running each doclet (default is $timeout; i.e. disabled)"
	echo "  -s supress path prefix (default is $prefix)"
	echo "  -- arguments to be passed to the integration script used to run the doclets (no default)"
	echo "  -h help"
	echo
}

while getopts "vp:m:f:d:t:s:h" option
do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

shift $((OPTIND - 1))

if [ "$p_arg" == "b" ] ; then
	prolog='B-Prolog'
	logtalk=bplgt$extension
	logtalk_call="$logtalk -g"
elif [ "$p_arg" == "ciao" ] ; then
	prolog='Ciao Prolog'
	logtalk=ciaolgt$extension
	logtalk_call="$logtalk -e"
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
	documenting_goal=$documenting_goal_dot
elif [ "$p_arg" == "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk=sicstuslgt$extension
	logtalk_call="$logtalk --goal"
	versions_goal=$versions_goal_dot
	documenting_goal=$documenting_goal_dot
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
	documenting_goal=$documenting_goal_dot
elif [ "$p_arg" == "xsbmt" ] ; then
	prolog='XSB-MT'
	logtalk=xsbmtlgt$extension
	logtalk_call="$logtalk -e"
	versions_goal=$versions_goal_dot
	documenting_goal=$documenting_goal_dot
elif [ "$p_arg" == "yap" ] ; then
	prolog='YAP'
	logtalk=yaplgt$extension
	logtalk_call="$logtalk -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported backend Prolog compiler: $p_arg" >&2
	usage_help
	exit 1
elif [ ! "$(command -v $backend)" ] ; then
	echo "Error! Default backend Prolog compiler not found: $prolog" >&2
	usage_help
	exit 1
elif [ ! "$(command -v $logtalk)" ] ; then
	echo "Error! $logtalk integration script for $prolog not found." >&2
	echo "       Check that its directory is in your execution path." >&2
	exit 1
fi

if [ "$p_arg" == "swipack" ] ; then
	versions_goal="use_module(library(logtalk)),$versions_goal"
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
	echo "Warning! Timeout support not available. The timeout option will be ignored." >&2
fi

mkdir -p "$results"
rm -f "$results"/*.results
rm -f "$results"/*.errors
rm -f "$results"/errors.all
rm -f "$results"/tester_versions.txt

start_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")

echo '*******************************************************************************'
echo "***** Batch documentation processing started @ $start_date"
$logtalk_call $versions_goal > "$results"/tester_versions.txt 2> /dev/null
grep -a "Logtalk version:" "$results"/tester_versions.txt
grep -a "Prolog version:" "$results"/tester_versions.txt | sed "s/Prolog/$prolog/"

doclets=0
output="$(find "$base" -name "doclet.lgt" -or -name "doclet.logtalk")"
while read -r file && [ "$file" != "" ]; do
	((doclets++))
	run_doclets "$file"
done <<< "$output"

cd "$results" || exit 1
timeouts=$(cat -- *.errors | grep -c 'LOGTALK_TIMEOUT')
crashes=$(cat -- *.errors | grep -c 'LOGTALK_CRASH')
failures=$(cat -- *.results | grep -c 'failed')

echo "*******************************************************************************"
echo "***** Compilation errors/warnings and failed doclets"
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
echo "***** Doclet failures"
echo "*******************************************************************************"
grep -s -a 'failed' -- *.results
echo "*******************************************************************************"
echo "***** $doclets doclets: $timeouts timeouts, $crashes crashes, $failures failures"
echo "*******************************************************************************"

end_date=$(eval date \"+%Y-%m-%d %H:%M:%S\")

echo "***** Batch documentation processing ended @ $end_date"
echo '*******************************************************************************'

if [ "$crashes" -gt 0 ] ; then
	exit 7
elif [ "$timeouts" -gt 0 ] ; then
	exit 3
elif [ "$failures" -gt 0 ] ; then
	exit 1
else
	exit 0
fi
