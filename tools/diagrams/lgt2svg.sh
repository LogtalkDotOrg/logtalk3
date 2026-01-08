#!/usr/bin/env bash

#############################################################################
##
##   DOT and d2 diagram files to SVG files conversion script
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
	echo
	export LOGTALKUSER=$HOME/logtalk
fi

if [ -d "$LOGTALKUSER" ]; then
	if ! [ -f "$LOGTALKUSER/VERSION.txt" ]; then
		echo "Cannot find VERSION.txt in the Logtalk user directory at $LOGTALKUSER!"
		echo "Creating an up-to-date Logtalk user directory..."
		logtalk_user_setup
	else
		system_version=$(cat "$LOGTALKHOME/VERSION.txt")
		user_version=$(cat "$LOGTALKUSER/VERSION.txt")
		if [ "$user_version" \< "$system_version" ]; then
			echo "Logtalk user directory at $LOGTALKUSER is outdated: "
			echo "	$user_version < $system_version"
			echo "Creating an up-to-date Logtalk user directory..."
			logtalk_user_setup
		fi
	fi
else
	echo "Cannot find the Logtalk user directory at $LOGTALKUSER!"
	echo "Running the logtalk_user_setup shell script to create the directory:"
	logtalk_user_setup
fi
echo


print_version() {
	echo "$(basename "$0") 0.13"
	exit 0
}


# default argument values
command="dot"
layout="elk"


usage_help() {
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


while getopts "c:l:vh" option; do
	case $option in
		c) c_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		v) print_version;;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

shift $((OPTIND - 1))
args=("$@")

case "$c_arg" in
	"dot"|"circo"|"fdp"|"neato")
		command="$c_arg"
		;;
	"")
		;;
	*)
		echo "Error! Unknown Graphviz command: $c_arg" >&2
		usage_help
		exit 1
		;;
esac

case "$l_arg" in
	"dagre"|"elk"|"tala")
		layout="$l_arg"
		;;
	"")
		;;
	*)
		echo "Error! Unknown d2 layout: $l_arg" >&2
		usage_help
		exit 1
		;;
esac

d2_count=$(ls -1 ./*.d2 2>/dev/null | wc -l)
dot_count=$(ls -1 ./*.dot 2>/dev/null | wc -l)

if [ $d2_count -ne 0 ] && ! command -v d2 >/dev/null 2>&1; then
	echo "Error! Cannot find the d2 command-line tool!" >&2
	echo "See https://d2lang.com/ for installation instructions." >&2
	echo
	exit 1
fi

if [ $dot_count -ne 0 ] && ! command -v "$command" >/dev/null 2>&1; then
	echo "Error! Cannot find the $command command-line tool!" >&2
	echo "See https://graphviz.org/ for installation instructions." >&2
	echo
	exit 1
fi

d2_failed_flag=0
dot_failed_flag=0

if [ $d2_count -ne 0 ] || [ $dot_count -ne 0 ] ; then
	cp "$LOGTALKUSER/tools/diagrams/diagrams.css" .
fi

if [ $d2_count -ne 0 ] ; then
	echo "Converting .d2 files to .svg files ..."
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
	for file in ./*.dot; do
		echo -n "  converting $(basename "$file") "
		converted=0
		counter=24
		while [ $converted -eq 0 ] && [ $counter -gt 0 ] ; do
			$command -q -Tsvg -Gfontnames=svg -o "${file%.*}.svg" "${args[@]}" "$file" 2>/dev/null | cat
			if [ "${PIPESTATUS[0]}" == 0 ] ; then
				converted=1
			fi
			counter=$((counter - 1))
			echo -n "."
		done
		if [ $converted -eq 0 ] ; then
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
    exit 0
elif [ $d2_failed_flag -eq 0 ] && [ $dot_failed_flag -eq 0 ] ; then
    echo "Conversion done."
    echo
    exit 0
else
    echo "Error! One or more files could not be converted!" >&2
    echo
    exit 1
fi
