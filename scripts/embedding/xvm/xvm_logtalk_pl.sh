#!/usr/bin/env bash

#############################################################################
##
##   This script creates a XVM logtalk.pl file with the Logtalk compiler and
##   runtime and optionally an application.pl file with a Logtalk application
##
##   Last updated on March 23, 2025
##
##   This file is part of Logtalk <https://logtalk.org/>
##   SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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
	echo "$(basename "$0") 0.13"
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
paths="$LOGTALKHOME/paths/paths.pl"
settings="$LOGTALKHOME/scripts/embedding/settings-embedding-sample.lgt"
compile="false"
foreign="false"
goal="true"
encrypt="false"

usage_help() {
	echo
	echo "This script creates a XVM logtalk.pl file with the Logtalk compiler/runtime"
	echo "and an optional application.pl file from an application source code given"
	echo "its loader file. When embedding an application, this script also creates a"
	echo "loader.pl file for loading all generated Prolog and foreign library files,"
	echo "optionally calling a startup goal."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-c] [-d directory] [-t tmpdir] [-p paths] [-s settings] [-l loader] [-g goal] [-f] [-x]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -c compile library alias paths in paths and settings files"
	echo "  -d directory for generated Prolog files (absolute path; default is current directory)"
	echo "  -t temporary directory for intermediate files (absolute path; default is an auto-created directory)"
	echo "  -p library paths file (absolute path; default is $paths)"
	echo "  -s settings file (absolute path or 'none'; default is $settings)"
	echo "  -l loader file for the application (absolute path)"
	echo "  -g startup goal for the application in canonical syntax (default is $goal)"
	echo "  -f copy foreign library files loaded by the application"
	echo "  -x encrypt the generated logtalk.pl and application.pl files"
	echo "  -v print version of $(basename "$0")"
	echo "  -h help"
	echo
}

while getopts "cd:t:p:s:l:g:fxvh" option; do
	case $option in
		c) compile="true";;
		d) d_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		s) s_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		g) g_arg="$OPTARG";;
		f) foreign="true";;
		x) encrypt="true";;
		v) print_version;;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

if [ "$d_arg" != "" ] ; then
	directory="$d_arg"
fi

if [ "$t_arg" != "" ] ; then
	temporary="$t_arg"
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

if [ "$g_arg" != "" ] ; then
	goal="$g_arg"
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

case $(sed --help 2>&1) in
	*GNU*) sed_i () { sed -i "$@"; };;
	*) sed_i () { sed -i '' "$@"; };;
esac

cp "$LOGTALKHOME/adapters/xvm.pl" .
cp "$LOGTALKHOME/core/core.pl" .

xvmlgt$extension --goal "logtalk_compile([core(expanding),core(monitoring),core(forwarding),core(user),core(logtalk),core(core_messages)],[optimize(on),scratch_directory('$temporary')]),halt."

if [ "$compile" = "true" ] ; then
	xvmlgt$extension --goal "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$paths',[hook(expand_library_alias_paths),scratch_directory('$temporary')]),halt."
else
	cp "$paths" "$temporary/paths_lgt.pl"
fi

if [ "$settings" != "" ] && [ "$settings" != "none" ] ; then
	if [ "$compile" = "true" ] ; then
		xvmlgt$extension --goal "logtalk_load(expand_library_alias_paths(loader)),logtalk_compile('$settings',[hook(expand_library_alias_paths),optimize(on),scratch_directory('$temporary')]),halt."
	else
		xvmlgt$extension --goal "logtalk_compile('$settings',[optimize(on),scratch_directory('$temporary')]),halt."
	fi
	sed_i "s/settings_file, allow/settings_file, deny/" xvm.pl
	cat \
		xvm.pl \
		paths*.pl \
		expanding*_lgt.pl \
		monitoring*_lgt.pl \
		forwarding*_lgt.pl \
		user*_lgt.pl \
		logtalk*_lgt.pl \
		core_messages*_lgt.pl \
		settings*.pl \
		core.pl \
		> "$directory"/logtalk.pl
else
	cat \
		xvm.pl \
		paths*.pl \
		expanding*_lgt.pl \
		monitoring*_lgt.pl \
		forwarding*_lgt.pl \
		user*_lgt.pl \
		logtalk*_lgt.pl \
		core_messages*_lgt.pl \
		core.pl \
		> "$directory"/logtalk.pl
fi

if [ "$loader" != "" ] ; then
	mkdir -p "$temporary/application"
	cd "$temporary/application" || exit 1
	if [ "$foreign" = "true" ] ; then
		xvmpl --goal "consult('$directory/logtalk'),set_logtalk_flag(clean,off),set_logtalk_flag(scratch_directory,'$temporary/application'),logtalk_load('$loader'),forall((current_plugin(PlugIn),plugin_property(PlugIn,file(File))),(decompose_file_name(File,_,Basename),atom_concat('$directory/',Basename,Copy),copy_file(File,Copy))),halt."
	else
		xvmpl --goal "consult('$directory/logtalk'),set_logtalk_flag(clean,off),set_logtalk_flag(scratch_directory,'$temporary/application'),logtalk_load('$loader'),halt."
	fi
	if test -n "$(find . -maxdepth 1 -name '*.pl' -print -quit)" ; then
		files="$(ls -rt ./*.pl)"
		for a in $files ; do cat "$a" >> "$directory"/application.pl ; done
	else
		echo
		echo "No application files found!"
		echo
		exit 1
	fi
	if [ "$encrypt" = "true" ] ; then
		xvmpl --goal "encrypt_program('$directory/logtalk.pl'),halt."
		xvmpl --goal "encrypt_program('$directory/application.pl'),halt."
		rm "$directory"/logtalk.pl
		rm "$directory"/application.pl
	fi
	if [ "$foreign" = "true" ] ; then
		xvmpl --goal "consult('$directory/logtalk'),set_logtalk_flag(report,off),logtalk_load('$loader'),open('$directory/loader.pl',append,Stream),forall((current_plugin(PlugIn),plugin_property(PlugIn,file(File))),(decompose_file_name(File,_,Basename,_),format(Stream,'\tload_foreign_library(~q),~n',[Basename]))),close(Stream),halt."
	fi
	echo ":- initialization((" > "$directory"/loader.pl
	echo "	consult(logtalk)," >> "$directory"/loader.pl
	if [ "$goal" = "true" ] ; then
		echo "	consult(application)" >> "$directory"/loader.pl
	else
		echo "	consult(application)," >> "$directory"/loader.pl
		echo "	$goal" >> "$directory"/loader.pl
	fi
	echo "))." >> "$directory"/loader.pl
	if [ "$encrypt" = "true" ] ; then
		xvmpl --goal "encrypt_program('$directory/loader.pl'),halt."
		rm "$directory"/loader.pl
	fi
fi

function cleanup {
	rm -rf "$temporary"
}
# register cleanup function to be called on EXIT signal
trap cleanup EXIT
