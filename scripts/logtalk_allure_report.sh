#!/usr/bin/env bash

#############################################################################
##
##   Allure report generator script
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

print_version() {
	echo "$(basename "$0") 0.14"
	exit 0
}

# default argument values
minimal_version="2.26.0"
tests=$(pwd)
results="./allure-results"
report="./allure-report"
title=""
preprocess_only="false"
single_file="false"
environment_pairs=""

if ! [ -x "$(command -v allure)" ] ; then
	echo "Error: Cannot find the allure command-line tool!" >&2
	echo "See https://allurereport.org/docs/ for installation instructions." >&2
	exit 1
elif ! [ "$(printf '%s\n' "$minimal_version" "$(allure --version)" | sort -V | head -n1)" = "$minimal_version" ] ; then
	echo "Warning: allure $minimal_version or later version is recommended!"
fi

usage_help() {
	echo
	echo "This script generates Allure reports from test results."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-d tests] [-i results] [-o report] [-t title] [-p] [-s]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -d tests directory (default is the current directory)"
	echo "  -i results directory (default is $results)"
	echo "  -o report directory (default is $report)"
	echo "  -t report title (default is \"Allure Report\")"
	echo "  -p preprocess results but do not generate report"
	echo "  -s single file report"
	echo "  -- environment pairs (key1=value1 key2=value2 ...)"
	echo "  -h help"
	echo
}

while getopts "vd:i:o:t:psh" option; do
	case $option in
		v) print_version;;
		d) d_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		o) o_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		p) p_arg="true";;
		s) s_arg="true";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

shift $((OPTIND - 1))
environment_pairs=("$@")

if [ "$d_arg" != "" ] ; then
	tests="$d_arg"
fi

if [ "$i_arg" != "" ] ; then
	mkdir -p "$i_arg"
	results="$i_arg"
fi

if [ "$o_arg" != "" ] ; then
	mkdir -p "$o_arg"
	report="$o_arg"
fi

if [ "$t_arg" != "" ] ; then
	title="$t_arg"
fi

if [ "$p_arg" != "" ] ; then
	preprocess_only="true"
fi

if [ "$s_arg" != "" ] ; then
	single_file="true"
fi

if [ -d "$report" ] && [ -n "$(ls -A "$report")" ] ; then
	if  [ -d "$report/data" ] && [ -d "$report/export" ] && [ -d "$report/history" ] &&
		[ -d "$report/plugins" ] && [ -d "$report/widgets" ] && [ -e "$report/app.js" ] &&
		[ -e "$report/favicon.ico" ] && [ -e "$report/index.html" ] && [ -e "$report/styles.css" ] ; then
		echo "Warning: Overriding previous report..."
	else
		echo "Error! Specified report directory is not empty and does not contain a previous" >&2
		echo "       report. Terminating the script execution to prevent any data loss." >&2
		exit 1
	fi
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

if [ ${#environment_pairs[@]} -gt 0 ] ; then
	rm -f "$results"/environment.properties
	for pair in "${environment_pairs[@]}" ; do
		echo "$pair" >> "$results"/environment.properties
	done
fi

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
	"buildName": "logtalk_allure_report#$next_build",
	"name": "logtalk_tester",
	"type": "logtalk_tester"
}
EOF
)
echo "$executor" > "$results"/executor.json

# add minimal categories.json to classify failed tests
categories=$(cat <<EOF
[
	{
		"name": "Failed tests",
		"matchedStatuses": ["failed"]
	}
]
EOF
)
echo "$categories" > "$results"/categories.json

cd "$results/.." || exit 1
if [ "$single_file" == "true" ] ; then
	if [ "$title" != "" ] ; then
		allure generate --single-file --clean --name "$title" --report-dir "$report" "$results"
	else
		allure generate --single-file --clean --report-dir "$report" "$results"
	fi
elif [ "$title" != "" ] ; then
	allure generate --clean --name "$title" --report-dir "$report" "$results"
else
	allure generate --clean --report-dir "$report" "$results"
fi

exit 0
