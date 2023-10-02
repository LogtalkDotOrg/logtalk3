#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk script for updating the HTML core, library, tools, ports,
##   contributions, and (optionally) packs documentation
## 
##   Last updated on January 12, 2023
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


# allow using this script from any directory
cd "$(dirname "$0")" || exit 1


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


# default to SWI-Prolog as some of the documentation should be
# generated using a multi-threaded backend Prolog compiler
backend=swi
prolog='SWI-Prolog'
logtalk="swilgt$extension -g"
include_packs='false'
if [ "$LOGTALKPACKS" != "" ] ; then
	logtalk_packs='$LOGTALKPACKS/'
else
	logtalk_packs='$HOME/logtalk_packs/'
fi

cwd=$(pwd)

set_goal() {
# documentation goal
	if [ "$include_packs" == 'true' ] ; then
		goal="set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),library(packs_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/', '$logtalk_packs'])]),halt."
	elif [ "$LOGTALKPACKS" != "" ] ; then
		goal="set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/']),exclude_prefixes(['$HOME/logtalk_packs/','$LOGTALKPACKS/'])]),halt."
	else
		goal="set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report),ports(loader),contributions(loader)]),lgtdoc::all([xml_docs_directory('$cwd/../docs/sources'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/']),exclude_prefixes(['$HOME/logtalk_packs/'])]),halt."
	fi
}

print_version() {
	echo "$(basename "$0") 0.25"
	exit 0
}


usage_help()
{
	echo 
	echo "This script updates the HTML documentation of the core entities, library,"
	echo "developer tools, ports, contributions, and optionally installed packs."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prolog] [-i]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -p backend Prolog compiler (default is $backend)"
	echo "     (valid values are b, ciao, cx, eclipse, gnu, ji, lvm, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
	echo "  -i include all installed packs"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
}


while getopts "vip:m:d:h" option
do
	case $option in
		v) print_version;;
		p) p_arg="$OPTARG";;
		i) include_packs='true';;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done


if [ "$p_arg" == "b" ] ; then
	prolog='B-Prolog'
	logtalk="bplgt$extension -g"
elif [ "$p_arg" == "ciao" ] ; then
	prolog='Ciao Prolog'
	logtalk="ciaolgt$extension -e"
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
elif [ "$p_arg" == "lvm" ] ; then
	prolog='LVM'
	logtalk="lvmlgt$extension -g"
elif [ "$p_arg" == "sicstus" ] ; then
	prolog='SICStus Prolog'
	logtalk="sicstuslgt$extension --goal"
elif [ "$p_arg" == "swi" ] ; then
	prolog='SWI-Prolog'
	logtalk="swilgt$extension -g"
elif [ "$p_arg" == "swipack" ] ; then
	prolog='SWI-Prolog'
	logtalk="swipl -g"
elif [ "$p_arg" == "tau" ] ; then
	prolog='Tau Prolog'
	logtalk="taulgt$extension -g"
elif [ "$p_arg" == "trealla" ] ; then
	prolog='Trealla Prolog'
	logtalk="tplgt$extension -g"
elif [ "$p_arg" == "xsb" ] ; then
	prolog='XSB'
	logtalk="xsblgt$extension -e"
elif [ "$p_arg" == "yap" ] ; then
	prolog='YAP'
	logtalk="yaplgt$extension -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported backend Prolog compiler: $p_arg" >&2
	usage_help
	exit 1
fi

set_goal
$logtalk "$goal"
cd "$cwd/../docs/sources" || exit 1
lgt2rst -t "Logtalk APIs"
if [ "$include_packs" == 'true' ] ; then
	cp _templates/layout_packs.html _templates/layout.html
else
	cp _templates/layout_no_packs.html _templates/layout.html
fi
mv _conf.py conf.py
make clean
make html
make info
#make linkcheck
cp -R _build/html/* ../
cp _build/texinfo/LogtalkAPIs-*.info ../
make clean
rm _templates/layout.html
mv conf.py _conf.py
mv browserconfig.xml browserconfig.xml.saved
rm *.xml
mv browserconfig.xml.saved browserconfig.xml
rm *.rst

exit 0
