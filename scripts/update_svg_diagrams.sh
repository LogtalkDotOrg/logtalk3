#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk script for updating the HTML library and tools SVG diagrams
## 
##   Last updated on November 2, 2024
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


# default to SWI-Prolog as the backend compiler
backend=swi
logtalk="swilgt$extension -g"
include_packs='false'
if [ "$LOGTALKPACKS" != "" ] ; then
	logtalk_packs='$LOGTALKPACKS/'
else
	logtalk_packs='$HOME/logtalk_packs/'
fi

set_goals() {
# documentation goals
	core_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), inheritance_diagram::library(core,[title('Logtalk core entities'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/'])]), halt."

	library_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(library(all_loader)), inheritance_diagram::rlibrary(library, [title('Logtalk library'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/'])]), halt."
	
	if [ "$include_packs" == 'true' ] ; then
		packs_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(library(all_loader)), logtalk_load(library(packs_loader)), inheritance_diagram::rdirectory(packs, '$logtalk_packs', [title('Logtalk packs'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/', '$logtalk_packs','$HOME/'])]), halt."
	fi
	
	if [ "$LOGTALKPACKS" != "" ] ; then
		tools_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report)]), inheritance_diagram::rlibrary(tools, [title('Logtalk development tools'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/']),exclude_directories(['$HOME/logtalk_packs/','$LOGTALKPACKS/'])]), halt."
	else
		tools_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on),logtalk_load([library(all_loader),tools(loader),issue_creator(loader),ports_profiler(loader),tutor(loader),wrapper(loader),lgtunit(coverage_report),lgtunit(automation_report),lgtunit(minimal_output),lgtunit(tap_output),lgtunit(tap_report),lgtunit(xunit_output),lgtunit(xunit_report),lgtunit(xunit_net_v2_output),lgtunit(xunit_net_v2_report)]), inheritance_diagram::rlibrary(tools, [title('Logtalk development tools'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/']),exclude_directories(['$HOME/logtalk_packs/'])]), halt."
	fi
	
	ports_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(ports(loader)), inheritance_diagram::rlibrary(ports, [title('Logtalk ports of third-party software'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/'])]), halt."
	
	contributions_goal="git_hash(Hash,[]), atomic_list_concat(['https://github.com/LogtalkDotOrg/logtalk3/tree/',Hash,'/'],GitHub), logtalk_load(diagrams(loader)), set_logtalk_flag(source_data,on), logtalk_load(contributions(loader)), inheritance_diagram::rlibrary(contributions, [title('Logtalk third-party contributions'),node_type_captions(true),zoom(true),output_directory('./'),path_url_prefixes('$LOGTALKUSER/',GitHub,'https://logtalk.org/library/'),output_directory('./'),path_url_prefixes('$LOGTALKHOME/',GitHub,'https://logtalk.org/library/'),omit_path_prefixes(['$LOGTALKUSER/','$LOGTALKHOME/','$HOME/'])]), halt."
}

print_version() {
	echo "$(basename "$0") 0.26"
	exit 0
}


usage_help()
{
	echo 
	echo "This script updates the SVG diagrams of the core entities, library,"
	echo "developer tools, ports, contributions, and optionally installed packs."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prolog] [-i]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -p backend Prolog compiler (default is $backend)"
	echo "     (valid values are b, ciao, cx, eclipse, gnu, ji, xvm, sicstus, swi, swipack, tau, trealla, xsb, and yap)"
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
	logtalk="ciaolgt$extension -e"
elif [ "$p_arg" == "cx" ] ; then
	logtalk="cxlgt$extension --goal"
elif [ "$p_arg" == "eclipse" ] ; then
	logtalk="eclipselgt$extension -e"
elif [ "$p_arg" == "gnu" ] ; then
	logtalk="gplgt$extension --query-goal"
elif [ "$p_arg" == "ji" ] ; then
	logtalk="jiplgt$extension -n -g"
elif [ "$p_arg" == "xvm" ] ; then
	logtalk="xvmlgt$extension -g"
elif [ "$p_arg" == "sicstus" ] ; then
	logtalk="sicstuslgt$extension --goal"
elif [ "$p_arg" == "swi" ] ; then
	logtalk="swilgt$extension -g"
elif [ "$p_arg" == "swipack" ] ; then
	logtalk="swipl -g"
elif [ "$p_arg" == "tau" ] ; then
	logtalk="taulgt$extension -g"
elif [ "$p_arg" == "trealla" ] ; then
	logtalk="tplgt$extension -g"
elif [ "$p_arg" == "xsb" ] ; then
	logtalk="xsblgt$extension -e"
elif [ "$p_arg" == "yap" ] ; then
	logtalk="yaplgt$extension -g"
elif [ "$p_arg" != "" ] ; then
	echo "Error! Unsupported backend Prolog compiler: $p_arg" >&2
	usage_help
	exit 1
fi


cd ../docs || exit 1

set_goals
$logtalk "$core_goal"
$logtalk "$library_goal"
if [ "$include_packs" == 'true' ] ; then
	$logtalk "$packs_goal"
fi
$logtalk "$tools_goal"
$logtalk "$contributions_goal"
$logtalk "$ports_goal"

"$LOGTALKHOME/tools/diagrams/lgt2svg.sh"
lgt2svg_exit=$?

rm ./*.dot

exit $lgt2svg_exit
