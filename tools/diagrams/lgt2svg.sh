#!/usr/bin/env bash

#############################################################################
## 
##   DOT diagram files to SVG files conversion script 
##   Last updated on November 25, 2021
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
	echo "$(basename "$0") 0.6"
	exit 0
}


# default argument values
command="dot"


usage_help()
{
	echo 
	echo "This script converts all Graphviz .dot files"
	echo "in the current directory to SVG files"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-c command] [-- arguments]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version"
	echo "  -c Graphviz command (valid values are dot, circo, fdp and neato; default is $command)"
	echo "  -- addtional arguments to be passed to the Graphviz command (no default)"
	echo "  -h print help"
	echo
}


while getopts "c:vh" Option
do
	case $Option in
		c) c_arg="$OPTARG";;
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


echo "Converting .dot files to .svg files ..."
count=$(ls -1 ./*.dot 2>/dev/null | wc -l)
if [ $count != 0 ] ; then
	cp "$LOGTALKUSER/tools/diagrams/zoom.png" .
	cp "$LOGTALKUSER/tools/diagrams/diagrams.css" .
	for file in ./*.dot; do
		echo -n "  converting $(basename "$file") "
		flag=1
		counter=16
		while [ $flag -eq 1 ] && [ $counter -gt 0 ] ; do
			$command -q -Tsvg -Gfontnames=svg -o"${file%.*}.svg" ${args[@]} "$file" 2>/dev/null | cat
			if [ "${PIPESTATUS[0]}" == 0 ] ; then
				flag=0
			fi
			(( --counter ))
			echo -n "."
		done
		if [ $counter == 0 ] ; then
			echo " failed"
		else
			echo " done"
		fi
	done
	echo "Conversion done"
	echo
else
	echo "No .dot files exist in the current directory!"
	echo
fi

exit 0
