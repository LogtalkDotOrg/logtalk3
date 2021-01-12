#!/usr/bin/env bash

#############################################################################
## 
##   Allure report generator script
##   Last updated on January 12, 2021
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

# loosely based on a unit test automation script contributed by Parker Jones

print_version() {
	echo "$(basename "$0") 0.3"
	exit 0
}

# default argument values

tests=$(pwd)
results="./allure-results"
report="./allure-report"
max=7

usage_help()
{
	echo 
	echo "This script generates Allure reports."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-t logs] [-i results] [-o report] [-m max]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -t tests directory (default is $tests)"
	echo "  -i results directory (default is $results)"
	echo "  -o report directory (default is $reports)"
	echo "  -m maximum number of reports in history (default is $max)"
	echo "  -h help"
	echo
}

while getopts "vt:i:o:m:h" option
do
	case $option in
		v) print_version;;
		t) t_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		o) o_arg="$OPTARG";;
		m) m_arg="$OPTARG";;
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
	reports="$o_arg"
fi

if [ "$m_arg" != "" ] ; then
	max="$m_arg"
fi

if [ ! -d "$results" ] ; then
	mkdir "$results"
else
	rm -rf "$results"/*.xml
fi

counter=0
output="$(find "$tests" -name xunit_report.xml)"
while read -r file && [ "$file" != "" ]; do
  ((counter++))
  mv -f "$file" "$results"/xunit_report_"$counter".xml
done <<< "$output"

if [[ -e "$report"/history ]]; then
    if [[ -e "$results"/history ]]; then
        rm -rf "$results"/history
    fi
    mv "$report"/history "$results"/history
fi

executor=$(cat <<EOF
{
    "name": "logtalk_tester"
}
EOF
)

echo "$executor" > "$results"/executor.json

cd "$results/.." || exit 1
allure generate --clean --report-dir "$report" "$results"

exit 0
