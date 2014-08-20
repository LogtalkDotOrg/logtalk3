#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk script for updating the HTML library and tools documentation
##   Last updated on August 20, 2014
## 
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##   
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##   
##   Additional licensing terms apply per Section 7 of the GNU General
##   Public License 3. Consult the `LICENSE.txt` file for details.
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


# documentation goals
core_goal="logtalk_load(lgtdoc(loader)),lgtdoc::library(core,[xmldir('$LOGTALKUSER/docs/tmp_core')]),halt."
library_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(library(all_loader)),lgtdoc::rlibrary(library,[xmldir('$LOGTALKUSER/docs/tmp_library')]),halt."
assertions_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(assertions(loader)),lgtdoc::library(assertions,[xmldir('$LOGTALKUSER/docs/tmp_assertions')]),halt."
debugger_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(debugger(loader)),lgtdoc::library(debugger,[xmldir('$LOGTALKUSER/docs/tmp_debugger')]),halt."
diagrams_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(diagrams(loader)),lgtdoc::library(diagrams,[xmldir('$LOGTALKUSER/docs/tmp_diagrams')]),halt."
help_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(help(loader)),lgtdoc::library(help,[xmldir('$LOGTALKUSER/docs/tmp_help')]),halt."
lgtdoc_goal="set_logtalk_flag(source_data,on),logtalk_load(lgtdoc(loader)),lgtdoc::library(lgtdoc,[xmldir('$LOGTALKUSER/docs/tmp_lgtdoc')]),halt."
lgtunit_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(lgtunit(loader)),lgtdoc::library(lgtunit,[xmldir('$LOGTALKUSER/docs/tmp_lgtunit')]),halt."
ports_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(ports(loader)),lgtdoc::library(ports,[xmldir('$LOGTALKUSER/docs/tmp_ports')]),halt."
profiler_goal="logtalk_load(lgtdoc(loader)),set_logtalk_flag(source_data,on),logtalk_load(profiler(loader)),lgtdoc::library(profiler,[xmldir('$LOGTALKUSER/docs/tmp_profiler')]),halt."


print_version() {
	echo "`basename $0` 0.2"
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


$logtalk $core_goal
$logtalk $library_goal
$logtalk $assertions_goal
$logtalk $debugger_goal
$logtalk $diagrams_goal
$logtalk $help_goal
$logtalk $lgtdoc_goal
$logtalk $lgtunit_goal
$logtalk $ports_goal
$logtalk $profiler_goal

cd $LOGTALKUSER/docs/tmp_core && lgt2html -i core.html -t "Core entities documentation index" && mv *.html ..
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
