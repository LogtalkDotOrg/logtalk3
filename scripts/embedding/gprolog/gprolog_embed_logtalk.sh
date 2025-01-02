#!/usr/bin/env bash

#############################################################################
## 
##   This script creates a new GNU Prolog top-level interpreter
##   that embeds Logtalk and optionally a Logtalk application
## 
##   Last updated on January 9, 2024
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
	echo "$(basename "$0") 0.18"
	exit 0
}

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
		echo "... unable to locate Logtalk installation directory!" >&2
		echo
		exit 1
	fi
	echo
	export LOGTALKHOME=$LOGTALKHOME
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!" >&2
	echo "Its current value is: $LOGTALKHOME" >&2
	echo "The variable must be set to your Logtalk installation directory!" >&2
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
directory="$(pwd -P)"
temporary=""
name="logtalk"
paths="$LOGTALKHOME/paths/paths.pl"
settings="$LOGTALKHOME/scripts/embedding/settings-embedding-sample.lgt"
compile="false"

usage_help()
{
	echo 
	echo "This script creates a new GNU Prolog top-level interpreter that embeds the"
	echo "Logtalk compiler and runtime and an optional application from an application"
	echo "source code given its loader file."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-c] [-d directory] [-t tmpdir] [-n name] [-p paths] [-s settings] [-l loader]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -c compile library alias paths in paths and settings files"
	echo "  -d directory for new top-level (absolute path; default is current directory)"
	echo "  -t temporary directory for intermediate files (absolute path; default is an auto-created directory)"
	echo "  -n name of the generated top-level (default is logtalk)"
	echo "  -p library paths file (absolute path; default is $paths)"
	echo "  -s settings file (absolute path or 'none'; default is $settings)"
	echo "  -l loader file for the application (absolute path)"
	echo "  -- arguments to be passed to gplc (no default)"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
}

while getopts "cd:t:n:p:l:s:vh" option
do
	case $option in
		c) compile="true";;
		d) d_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		n) n_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		v) print_version;;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

shift $((OPTIND - 1))
args=("$@")

if [ "$d_arg" != "" ] ; then
	directory="$d_arg"
fi

if [ "$t_arg" != "" ] ; then
	temporary="$t_arg"
fi

if [ "$n_arg" != "" ] ; then
	name="$n_arg"
fi

if [ "$p_arg" != "" ] ; then
	if [ -f "$p_arg" ] ; then
		paths="$p_arg"
	else
		echo "The $p_arg library paths file does not exist!" >&2
		exit 1
	fi
fi

if [ "$s_arg" != "" ] && [ "$s_arg" != "none" ] ; then
	if [ -f "$s_arg" ] ; then
		settings="$s_arg"
	else
		echo "The $s_arg settings file does not exist!" >&2
		exit 1
	fi
elif [ "$s_arg" == "none" ] ; then
	settings="none"
fi

if [ "$l_arg" != "" ] ; then
	if [ -f "$l_arg" ] ; then
		loader="$l_arg"
	else
		echo "The $l_arg loader file does not exist!" >&2
		exit 1
	fi
else
	loader=""
fi

mkdir -p "$directory"

if [ "$temporary" != "" ] ; then
	mkdir -p "$temporary"
else
	temporary=$(mktemp -d)
fi

if [[ ! "$temporary" || ! -d "$temporary" ]]; then
	echo "Could not create temporary directory!"
	exit 1
fi

cd "$temporary" || exit 1

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

# use GNU sed if available instead of BSD sed
if gsed --version >/dev/null 2>&1 ; then
	sed="gsed"
else
	sed="sed"
fi

cp "$LOGTALKHOME/adapters/gnu.pl" .
cp "$LOGTALKHOME/core/core.pl" .

gplgt$extension --query-goal "logtalk_compile([core(expanding),core(monitoring),core(forwarding),core(user),core(logtalk),core(core_messages)],[optimize(on),scratch_directory('$temporary')]),halt"

if [ "$compile" != "false" ] ; then
	gplgt$extension --query-goal "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$paths',[hook(expand_library_alias_paths),scratch_directory('$temporary')]),halt"
else
	cp "$paths" "$temporary/paths_lgt.pl"
fi

if [ "$settings" != "" ] && [ "$settings" != "none" ] ; then
	if [ "$compile" != "false" ] ; then
		gplgt$extension --query-goal "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$settings',[hook(expand_library_alias_paths),optimize(on),scratch_directory('$temporary')]),halt"
	else
		gplgt$extension --query-goal "logtalk_compile('$settings',[optimize(on),scratch_directory('$temporary')]),halt"
	fi
	$sed -i "s/settings_file, allow/settings_file, deny/" gnu.pl
else
	touch settings_lgt.pl
fi

if [ "$loader" != "" ] ; then
	mkdir -p "$temporary/application"
	cd "$temporary/application" || exit 1
	if [ "$settings" != "" ] && [ "$settings" != "none" ] ; then
		cp "$settings" ./settings.lgt
	fi
	gplgt$extension --query-goal "set_logtalk_flag(clean,off),set_logtalk_flag(scratch_directory,'$temporary/application'),logtalk_load('$loader'),halt"
	cd ..
else
	touch application.pl
fi

gplc "${args[@]}" -o "$directory"/"$name" gnu.pl expanding*_lgt.pl monitoring*_lgt.pl forwarding*_lgt.pl user*_lgt.pl logtalk*_lgt.pl core_messages*_lgt.pl $(ls -rt application/*.pl 2>/dev/null) settings*_lgt.pl core.pl paths_*.pl

function cleanup {
	rm -rf "$temporary"
}
# register cleanup function to be called on EXIT signal
trap cleanup EXIT
