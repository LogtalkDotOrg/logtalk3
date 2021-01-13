#!/usr/bin/env bash

#############################################################################
## 
##   Allure report generator script
##   Last updated on January 13, 2021
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

print_version() {
	echo "$(basename "$0") 0.5"
	exit 0
}

# default argument values
tests=$(pwd)
results="./allure-results"
report="./allure-report"
preprocess_only="false"

usage_help()
{
	echo 
	echo "This script generates Allure reports."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-t logs] [-i results] [-o report] [-p]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -t tests directory (default is $tests)"
	echo "  -i results directory (default is $results)"
	echo "  -o report directory (default is $reports)"
	echo "  -p preprocess results but do not generate report"
	echo "  -h help"
	echo
}

while getopts "vt:i:o:ph" option
do
	case $option in
		v) print_version;;
		t) t_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		o) o_arg="$OPTARG";;
		p) p_arg="true";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

shift $((OPTIND - 1))

if [ "$t_arg" != "" ] ; then
	tests="$t_arg"
fi

if [ "$i_arg" != "" ] ; then
	mkdir -p "$i_arg"
	results="$i_arg"
fi

if [ "$o_arg" != "" ] ; then
	mkdir -p "$o_arg"
	report="$o_arg"
fi

if [ "$p_arg" != "" ] ; then
	preprocess_only="true"
fi

# move all xunit_report.xml files found in the current directory
# and sub-directories to the $results directory; the files are
# renamed using an integer counter to avoid overwriting them
counter=0
mkdir -p "$results"
rm -f "$results"/xunit_report_*.xml
output="$(find "$tests" -name xunit_report.xml)"
while read -r file && [ "$file" != "" ]; do
  ((counter++))
  mv -f "$file" "$results"/xunit_report_"$counter".xml
done <<< "$output"

if [ "$preprocess_only" == "true" ] ; then
	exit 0
fi

# assume that the $results directory is kept between test
# runs and use a custom file to track the build order
if [ -d "$results"/history ] ; then
	current_build=$(<"$results"/history/logtalk_build_number)
else
	current_build=1
fi

# move the history from the previous report to the
# $results directory so that we can get trend graphs
if [ -d "$report"/history ] ; then
    if [ -d "$results"/history ] ; then
        rm -rf "$results"/history
    fi
    mv "$report"/history "$results"/history
	next_build=$((current_build+1))
	echo "$next_build" > "$results"/history/logtalk_build_number
else
	next_build="$current_build"
fi

# add a minimal executor.json so that trend graphs
# show build labels
executor=$(cat <<EOF
{
    "buildOrder": "$next_build",
    "name": "logtalk_tester"
}
EOF
)
echo "$executor" > "$results"/executor.json

cd "$results/.." || exit 1
allure generate --clean --report-dir "$report" "$results"

exit 0
