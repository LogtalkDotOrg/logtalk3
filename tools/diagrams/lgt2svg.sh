#!/usr/bin/env bash

#############################################################################
## 
##   DOT diagram files to SVG files conversion script 
##   Last updated on November 19, 2024
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
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!" >&2
	echo "Its current value is: $LOGTALKHOME" >&2
	echo "The variable must be set to your Logtalk installation directory!" >&2
	echo
	exit 1
fi
export LOGTALKHOME

if ! [ "$LOGTALKUSER" ]; then
	echo "The environment variable LOGTALKUSER should be defined first, pointing"
	echo "to your Logtalk user directory!"
	echo "Trying the default location for the Logtalk user directory..."
	export LOGTALKUSER=$HOME/logtalk
	if [ -d "$LOGTALKUSER" ]; then
		echo "... using Logtalk user directory found at $LOGTALKUSER"
	else
		echo "... Logtalk user directory not found at default location. Creating a new"
		echo "Logtalk user directory by running the \"logtalk_user_setup\" shell script:"
		logtalk_user_setup
	fi
elif ! [ -d "$LOGTALKUSER" ]; then
	echo "Cannot find \$LOGTALKUSER directory! Creating a new Logtalk user directory"
	echo "by running the \"logtalk_user_setup\" shell script:"
	logtalk_user_setup
fi
echo


print_version() {
	echo "$(basename "$0") 0.12"
	exit 0
}


# default argument values
command="dot"
layout="elk"


usage_help()
{
	echo 
	echo "This script converts .d2 and .dot files in the current directory to SVG files"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-c command] [-- arguments]"
	echo "  $(basename "$0") [-l layout] [-- arguments]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -c Graphviz command (dot, circo, fdp, or neato; default is $command)"
	echo "  -l d2 layout (dagre, elk, or tala; default is $layout)"
	echo "  -- additional arguments to be passed to the converter command (no default)"
	echo "  -v print version"
	echo "  -h print help"
	echo
}


while getopts "c:l:vh" Option
do
	case $Option in
		c) c_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		v) print_version;;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

shift $((OPTIND - 1))
args=("$@")


if [ "$c_arg" == "dot" ] ; then
	command="dot"
elif [ "$c_arg" == "circo" ] ; then
	command="circo"
elif [ "$c_arg" == "fdp" ] ; then
	command="fdp"
elif [ "$c_arg" == "neato" ] ; then
	command="neato"
elif [ "$c_arg" != "" ] ; then
	echo "Error! Unknown Graphviz command: $c_arg" >&2
	usage_help
	exit 1
fi

if [ "$l_arg" == "dagre" ] ; then
	layout="dagre"
elif [ "$l_arg" == "elk" ] ; then
	layout="elk"
elif [ "$l_arg" == "tala" ] ; then
	layout="tala"
elif [ "$l_arg" != "" ] ; then
	echo "Error! Unknown d2 layout: $l_arg" >&2
	usage_help
	exit 1
fi

d2_count=$(ls -1 ./*.d2 2>/dev/null | wc -l)
dot_count=$(ls -1 ./*.dot 2>/dev/null | wc -l)

d2_failed_flag=0
dot_failed_flag=0

if [ $d2_count -ne 0 ] ; then
	echo "Converting .d2 files to .svg files ..."
	cp "$LOGTALKUSER/tools/diagrams/diagrams.css" .
	for file in ./*.d2; do
		echo -n "  converting $(basename "$file")... "
		d2 --layout "$layout" "${args[@]}" "$file" "${file%.*}.svg"  2>/dev/null | cat
		if [ "${PIPESTATUS[0]}" == 0 ] ; then
			echo "done"
		else
			d2_failed_flag=1
			echo "failed"
		fi
	done
fi

if [ $dot_count -ne 0 ] ; then
	echo "Converting .dot files to .svg files ..."
	cp "$LOGTALKUSER/tools/diagrams/diagrams.css" .
	for file in ./*.dot; do
		echo -n "  converting $(basename "$file") "
		converted=1
		counter=24
		while [ $converted -eq 1 ] && [ $counter -gt 0 ] ; do
			$command -q -Tsvg -Gfontnames=svg -o "${file%.*}.svg" "${args[@]}" "$file" 2>/dev/null | cat
			if [ "${PIPESTATUS[0]}" == 0 ] ; then
				converted=0
			fi
			(( --counter ))
			echo -n "."
		done
		if [ $counter == 0 ] ; then
			dot_failed_flag=1
			echo " failed"
		else
			echo " done"
		fi
	done
fi

if [ $d2_count -eq 0 ] && [ $dot_count -eq 0 ] ; then
	echo "No .d2 or .dot files exist in the current directory!"
	echo
fi

if [ $d2_failed_flag -eq 0 ] && [ $dot_failed_flag -eq 0 ] ; then
	echo "Conversion done."
	echo
	exit 0
else
	echo "One or more files could not be converted!"
	echo
	exit 1
fi
