#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk version select script
##   Last updated on November 17, 2013
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


print_version() {
	echo "`basename $0` 0.7"
	exit 0
}


list_versions() {
    echo "Available versions:"
	if [ `(ls -d "$prefix"/logtalk-* | wc -l) 2> /dev/null` -gt 0 ]; then
		for path in $(ls -d "$prefix"/logtalk-*); do
			file=`basename $path`
			echo "  $file"
		done
		echo
	else
		echo "none"
	fi
	exit 1
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
	echo "  `basename $0` version"
	echo "  `basename $0` -v"
	echo "  `basename $0` -l"
	echo "  `basename $0` -s"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of `basename $0`"
	echo "  -l list available versions"
	echo "  -s show the currently selected version"
	echo "  -h help"
	echo
	exit 0
}


valid_version() {
	if [ `(ls -d "$prefix"/logtalk-* | wc -l) 2> /dev/null` -gt 0 ]; then
	    for path in $(ls -d "$prefix"/logtalk-*); do
			version=`basename $path`
	        if [ $1 == $version ]; then
	            return 0
	        fi
	    done
	fi
	return 1
}


switch_version() {
	valid_version $1
	if [ 0 != ${?} ]; then
    	echo "Invalid version: $1"
    	exit 1
	else
		cd "$prefix"
		rm -f logtalk
		ln -sf $1 logtalk
    	echo "Switched to version: $1"
		exit 0
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


prefix=`dirname "$LOGTALKHOME"`


while getopts "vlsh" Option
do
	case $Option in
		v) print_version;;
		l) list_versions;;
		s) show_selected;;
		h) usage_help;;
		*) usage_help;;
	esac
done


if [ "$1" == "" ]; then
	usage_help
else
	switch_version $1
	error=$?
	if [ 0 != $error ]; then
		echo "An error occurred when activating version \"$version\"!"
		exit 1
	fi
	exit 0
fi
