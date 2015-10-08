#!/bin/bash

#############################################################################
## 
##   Logtalk script for updating the HTML library and tools documentation
##   Last updated on October 8, 2015
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


operating_system=`uname -s`

if [ "${operating_system:0:10}" == "MINGW32_NT" ] ; then
	# assume that we're running on Windows using the Git for Windows bash shell
	extension='.sh'
elif [ "$LOGTALKHOME" != "" ] && [ "$LOGTALKUSER" != "" ] && [ "$LOGTALKHOME" == "$LOGTALKUSER" ] ; then
	# assume that we're running Logtalk without using the installer scripts
	extension='.sh'
else
	extension=''
fi


# default to SWI-Prolog as some of the documentation should be
# generated using a multi-threaded back-end Prolog compiler
backend=swipl
prolog='SWI-Prolog'
logtalk="swilgt$extension -g"

cwd=`pwd`

# documentation goals
core_goal="logtalk_load(lgtdoc(loader)),lgtdoc::library(core,[xmldir('$cwd/../docs/tmp_core')]),halt."
library_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(library(all_loader)),lgtdoc::rlibrary(library,[xmldir('$cwd/../docs/tmp_library')]),halt."
assertions_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(assertions(loader)),lgtdoc::library(assertions,[xmldir('$cwd/../docs/tmp_assertions')]),halt."
debugger_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(debugger(loader)),lgtdoc::library(debugger,[xmldir('$cwd/../docs/tmp_debugger')]),halt."
diagrams_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(diagrams(loader)),lgtdoc::library(diagrams,[xmldir('$cwd/../docs/tmp_diagrams')]),halt."
help_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(help(loader)),lgtdoc::library(help,[xmldir('$cwd/../docs/tmp_help')]),halt."
lgtdoc_goal="set_logtalk_flag(source_data,on),logtalk_load(lgtdoc(loader)),lgtdoc::library(lgtdoc,[xmldir('$cwd/../docs/tmp_lgtdoc')]),halt."
lgtunit_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(lgtunit(loader)),lgtdoc::library(lgtunit,[xmldir('$cwd/../docs/tmp_lgtunit')]),halt."
ports_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(ports(loader)),lgtdoc::library(ports,[xmldir('$cwd/../docs/tmp_ports')]),halt."
profiler_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(profiler(loader)),lgtdoc::library(profiler,[xmldir('$cwd/../docs/tmp_profiler')]),halt."


print_version() {
	echo "`basename $0` 0.3"
	exit 0
}


usage_help()
{
	echo 
	echo "This script updates the HTML documentation of the library and the development tools."
	echo
	echo "Usage:"
	echo "  `basename $0` [-p prolog]"
	echo "  `basename $0` -v"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -p back-end Prolog compiler (default is $backend)"
	echo "     (possible values are b, cx, eclipse, gnu, lean, qp, sicstus, swi, xsb, xsbmt, and yap)"
	echo "  -v print version of `basename $0`"
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
elif [ ! `command -v $backend` ] ; then
    echo "Error! Default back-end Prolog compiler not found: $prolog"
	usage_help
    exit 1
fi

cd "$cwd/../docs"

$logtalk "$core_goal"
$logtalk "$library_goal"
$logtalk "$assertions_goal"
$logtalk "$debugger_goal"
$logtalk "$diagrams_goal"
$logtalk "$help_goal"
$logtalk "$lgtdoc_goal"
$logtalk "$lgtunit_goal"
$logtalk "$ports_goal"
$logtalk "$profiler_goal"

cd ./tmp_core && lgt2html -i core.html -t "Core entities documentation index" && mv *.html ..
cd ../tmp_library && lgt2html -i library.html -t "Library documentation index" && mv *.html ..
cd ../tmp_assertions && lgt2html -i assertions_tool.html -t "Assertions tool" && mv *.html ..
cd ../tmp_debugger && lgt2html -i debugger_tool.html -t "Debugger tool" && mv *.html ..
cd ../tmp_diagrams && lgt2html -i diagrams_tool.html -t "Diagrams tool" && mv *.html ..
cd ../tmp_help && lgt2html -i help_tool.html -t "Help tool" && mv *.html ..
cd ../tmp_lgtdoc && lgt2html -i lgtdoc_tool.html -t "Logtalk documenting tool" && mv *.html ..
cd ../tmp_lgtunit && lgt2html -i lgtunit_tool.html -t "Logtalk unit testing tool" && mv *.html ..
cd ../tmp_ports && lgt2html -i ports_tool.html -t "Port profiler tool" && mv *.html ..
cd ../tmp_profiler && lgt2html -i profiler_tool.html -t "Profiler tool" && mv *.html ..

rm -rf ../tmp_*

exit 0
