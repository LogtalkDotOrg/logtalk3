#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk script for updating the HTML library and tools documentation
##   Last updated on December 12, 2017
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


# default to SWI-Prolog as the backend compiler
backend=swipl
prolog='SWI-Prolog'
logtalk="swilgt$extension -g"


# documentation goals
core_goal="logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), inheritance_diagram::library(core,[title('Logtalk core entities'),node_type_captions(true),url_prefixes('https://github.com/LogtalkDotOrg/logtalk3/tree/master/','http://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/'])]), halt."

library_goal="logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(library(all_loader)), inheritance_diagram::library(library, [title('Logtalk library'),node_type_captions(true),url_prefixes('https://github.com/LogtalkDotOrg/logtalk3/tree/master/','http://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/'])]), halt."

tools_goal="logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),ports(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report)]), inheritance_diagram::rlibrary(tools, [title('Logtalk development tools'),node_type_captions(true),url_prefixes('https://github.com/LogtalkDotOrg/logtalk3/tree/master/','http://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/'])]), halt."


print_version() {
	echo "$(basename "$0") 0.7"
	exit 0
}


usage_help()
{
	echo 
	echo "This script updates the SVG diagrams of the core entities, the library, and the development tools."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prolog]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -p back-end Prolog compiler (default is $backend)"
	echo "     (possible values are b, cx, eclipse, gnu, ji, lean, qp, sicstus, swi, xsb, xsbmt, and yap)"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
	exit 0
}


while getopts "vp:m:d:h" option
do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done


if [ "$p_arg" == "b" ] ; then
	prolog='B-Prolog'
	logtalk="bplgt$extension -g"
elif [ "$p_arg" == "cx" ] ; then
	prolog='CxProlog'
	logtalk="cxlgt$extension --goal"
elif [ "$p_arg" == "eclipse" ] ; then
	prolog='ECLiPSe'
	logtalk="eclipselgt$extension -e"
elif [ "$p_arg" == "gnu" ] ; then
	prolog='GNU Prolog'
	logtalk="gplgt$extension --query-goal"
elif [ "$p_arg" == "ji" ] ; then
	prolog='JIProlog'
	logtalk="jiplgt$extension -n -g"
elif [ "$p_arg" == "lean" ] ; then
	prolog='Lean Prolog'
	logtalk="lplgt$extension"
elif [ "$p_arg" == "qp" ] ; then
	prolog='Qu-Prolog'
	logtalk="qplgt$extension -g"
elif [ "$p_arg" == "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk="sicstuslgt$extension --goal"
elif [ "$p_arg" == "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk="swilgt$extension -g"
elif [ "$p_arg" == "xsb" ] ; then
	prolog='XSB'
	logtalk="xsblgt$extension -e"
elif [ "$p_arg" == "xsbmt" ] ; then
	prolog='XSB-MT'
	logtalk="xsbmtlgt$extension -e"
elif [ "$p_arg" == "yap" ] ; then
	prolog='YAP'
	logtalk="yaplgt$extension -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported back-end Prolog compiler: $p_arg"
	usage_help
	exit 1
elif [ ! "$(command -v $backend)" ] ; then
    echo "Error! Default back-end Prolog compiler not found: $prolog"
	usage_help
    exit 1
fi


cd ../docs || exit 1

$logtalk "$core_goal"
$logtalk "$library_goal"
$logtalk "$tools_goal"

for f in *.dot; do
	flag=0
	counter=10
	while [ $flag -eq 0 ] && [ $counter -ge 0 ] ; do
		dot -Tsvg "$f" > "${f%.*}.svg"
		if [ $? == 0 ]; then
			flag=1
		fi
		(( --counter ))
	done
done

rm ./*.dot

exit 0
