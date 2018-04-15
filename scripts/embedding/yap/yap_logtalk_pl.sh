#!/usr/bin/env bash

#############################################################################
## 
##   This script creates a YAP logtalk.pl file with the Logtalk compiler
##   and runtime and optionally an application.pl file with a Logtalk
##   application
## 
##   Last updated on April 15, 2018
## 
##   This file is part of Logtalk <https://logtalk.org/>  
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


if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first, pointing"
	echo "to your Logtalk installation directory!"
	echo "Trying the default locations for the Logtalk installation..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "... using Logtalk installation found at /usr/local/share/logtalk"
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "... using Logtalk installation found at /usr/share/logtalk"
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "... using Logtalk installation found at /opt/local/share/logtalk"
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "... using Logtalk installation found at /opt/share/logtalk"
	elif [ -d "$HOME/share/logtalk" ]; then
		LOGTALKHOME="$HOME/share/logtalk"
		echo "... using Logtalk installation found at $HOME/share/logtalk"
	elif [ -f "$( cd "$( dirname "$0" )" && pwd )/../core/core.pl" ]; then
		LOGTALKHOME="$( cd "$( dirname "$0" )" && pwd )/.."
		echo "... using Logtalk installation found at $( cd "$( dirname "$0" )" && pwd )/.."
	else
		echo "... unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo
	export LOGTALKHOME=$LOGTALKHOME
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first, pointing"
	echo "to your Logtalk user directory!"
	echo "Trying the default location for the Logtalk user directory..."
	echo
	export LOGTALKUSER=$HOME/logtalk
fi

if [ -d "$LOGTALKUSER" ]; then
	if ! [ -f "$LOGTALKUSER/VERSION.txt" ]; then
		echo "Cannot find version information in the Logtalk user directory at $LOGTALKUSER!"
		echo "Creating an up-to-date Logtalk user directory..."
		logtalk_user_setup
	else
		system_version=$(cat "$LOGTALKHOME/VERSION.txt")
		user_version=$(cat "$LOGTALKUSER/VERSION.txt")
		if [ "$user_version" \< "$system_version" ]; then
			echo "Logtalk user directory at $LOGTALKUSER is outdated: "
			echo "    $user_version < $system_version"
			echo "Creating an up-to-date Logtalk user directory..."
			logtalk_user_setup
		fi
	fi
else
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"logtalk_user_setup\" shell script:"
	logtalk_user_setup
fi

# default values
directory="$HOME/collect"
paths="$LOGTALKHOME/paths/paths_core.pl"
hooks="$LOGTALKHOME/adapters/yaphooks.pl"
compile="false"

print_version() {
	echo "$(basename "$0") 0.9"
	exit 0
}

usage_help()
{
	echo 
	echo "This script creates a YAP logtalk.pl file with the Logtalk compiler and"
	echo "runtime and an optional application.qlf file from an application source"
	echo "code given its loader file."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-c] [-d directory] [-p paths] [-h hooks] [-s settings] [-l loader]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -c compile library alias paths in paths and settings files"
	echo "  -d directory to use for intermediate and final results (default is $directory)"
	echo "  -p library paths file (default is $paths)"
	echo "  -h YAP hooks file (default is $hooks)"
	echo "  -s optional settings file"
	echo "  -l optional loader file for the application"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
	exit 0
}

while getopts "cd:p:h:l:s:vh" option
do
	case $option in
		c) compile="true";;
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) h_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		v) print_version;;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$d_arg" != "" ] ; then
	directory="$d_arg"
fi

if [ "$p_arg" != "" ] ; then
	if [ -f "$p_arg" ] ; then
		paths="$p_arg"
	else
		echo "The $p_arg library paths file does not exist!"
		exit 1
	fi
fi

if [ "$h_arg" != "" ] ; then
	if [ -f "$h_arg" ] ; then
		hooks="$h_arg"
	else
		echo "The $h_arg hooks file does not exist!"
		exit 1
	fi
fi

if [ "$l_arg" != "" ] ; then
	if [ -f "$l_arg" ] ; then
		loader="$l_arg"
	else
		echo "The $l_arg loader file does not exist!"
		exit 1
	fi
else
	loader=""
fi

if [ "$s_arg" != "" ] ; then
	if [ -f "$s_arg" ] ; then
		settings="$s_arg"
	else
		echo "The $s_arg settings file does not exist!"
		exit 1
	fi
else
	settings=""
fi

mkdir -p "$directory"
cd "$directory"

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

cp "$LOGTALKHOME/adapters/yap.pl" .
cp "$LOGTALKHOME/core/core.pl" .

yaplgt$extension -g "logtalk_compile([core(expanding),core(monitoring),core(forwarding),core(user),core(logtalk),core(core_messages)],[optimize(on),scratch_directory('$directory')]),halt"

if [ "$compile" != "false" ] ; then
	yaplgt$extension -g "logtalk_load(library(expand_library_alias_paths_loader)),logtalk_compile('$paths',[hook(expand_library_alias_paths),scratch_directory('$directory')]),halt"
else
	cp "$paths" "$directory/paths_lgt.yap"
fi

if [ "$settings" != "" ] ; then
	if [ "$compile" != "false" ] ; then
		yaplgt$extension -g "logtalk_load(library(expand_library_alias_paths_loader)),logtalk_compile('$settings',[hook(expand_library_alias_paths),optimize(on),scratch_directory('$directory')]),halt"
	else
		yaplgt$extension -g "logtalk_compile('$settings',[optimize(on),scratch_directory('$directory')]),halt"
	fi
	cat \
		yap.pl \
		paths*.yap \
		expanding*_lgt.yap \
		monitoring*_lgt.yap \
		forwarding*_lgt.yap \
		user*_lgt.yap \
		logtalk*_lgt.yap \
		core_messages*_lgt.yap \
		settings*.yap \
		core.pl \
		"$hooks" \
		> logtalk.pl
else
	cat \
		yap.pl \
		paths*.yap \
		expanding*_lgt.yap \
		monitoring*_lgt.yap \
		forwarding*_lgt.yap \
		user*_lgt.yap \
		logtalk*_lgt.yap \
		core_messages*_lgt.yap \
		core.pl \
		"$hooks" \
		> logtalk.pl
fi

rm *.yap
rm yap.pl
rm core.pl
rm -f yaphooks.pl

if [ "$loader" != "" ] ; then
	mkdir -p "$directory/application"
	cd "$directory/application"
	yap -g "consult('../logtalk'),set_logtalk_flag(clean,off),set_logtalk_flag(scratch_directory,'$directory/application'),logtalk_load('$loader'),halt"
	cat $(ls -rt $directory/application/*.yap) > ../application.pl
	rm *.yap
fi
