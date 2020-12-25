#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk version select script
##   Last updated on May 18, 2018
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
	echo "$(basename "$0") 0.10"
	exit 0
}


list_versions() {
	echo "Available versions:"
	if ls -d "$prefix"/logtalk-* 1> /dev/null 2>&1 ; then
		for path in "$prefix"/logtalk-* ; do
			[ -d "$path" ] || continue
			echo "  $(basename "$path")"
		done
		echo
	else
		echo "none"
	fi
	exit 0
}


show_selected() {
	echo "Selected version:"
	if [ -e "$LOGTALKHOME" ]; then
		echo -n "  "
		readlink "$LOGTALKHOME"
	else
		echo "  none"
	fi
	exit 0
}


usage_help() {
	echo 
	echo "This script allows switching between installed Logtalk versions"
	echo
	echo "Usage:"
	echo "  $(basename "$0") version"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -l"
	echo "  $(basename "$0") -s"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -l list available versions"
	echo "  -s show the currently selected version"
	echo "  -h help"
	echo
}


switch_version() {
	if [ -d "$prefix/$1" ]; then
		cd "$prefix" || exit 1
		rm -f logtalk
		if ln -sf "$1" logtalk ; then
			echo "Switched to version: $1"
			exit 0
		else
			echo "An error occurred when switching the Logtalk version!" >&2
			echo "Check that you are executing this script with the necessary permissions." >&2
			exit 1
		fi
	else
		echo "Invalid version: $1" >&2
		exit 1
	fi
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


prefix=$(dirname "$LOGTALKHOME")


while getopts "vlsh" Option
do
	case $Option in
		v) print_version;;
		l) list_versions;;
		s) show_selected;;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done


if [ "$1" == "" ]; then
	usage_help
	exit 0
else
	switch_version "$1"
	error=$?
	if [ 0 != $error ]; then
		echo "An error occurred when activating version \"$1\"!" >&2
		exit 1
	fi
	exit 0
fi
