#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk back-end Prolog compiler select script
##   Last updated on February 17, 2017
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
	echo "Current `basename $0` version:"
	echo "  0.6"
	exit 0
}


list_backends() {
    echo "Available back-end Prolog compilers:"
	if [ -e `command -v bplgt` ]  && [ "`command -v bp`" != "" ] ; then
		echo -n "  bplgt"
	fi
	if [ -e `command -v cxlgt` ]  && [ "`command -v cxprolog`" != "" ] ; then
		echo -n "  cxlgt"
	fi
	if [ -e `command -v eclipselgt` ]  && [ "`command -v eclipse`" != "" ] ; then
		echo -n "  eclipselgt"
	fi
	if [ -e `command -v gplgt` ]  && [ "`command -v gprolog`" != "" ] ; then
		echo -n "  gplgt"
	fi
	if [ -e `command -v jiplgt` ]  && [ "`command -v jipconsole.sh`" != "" ] ; then
		echo -n "  jiplgt"
	fi
	if [ -e `command -v lplgt` ]  && [ "`command -v lprolog`" != "" ] ; then
		echo -n "  lplgt"
	fi
	if [ -e `command -v qplgt` ]  && [ "`command -v qp`" != "" ] ; then
		echo -n "  qplgt"
	fi
	if [ -e `command -v sicstuslgt` ]  && [ "`command -v sicstus`" != "" ] ; then
		echo -n "  sicstuslgt"
	fi
	if [ -e `command -v swilgt` ]  && [ "`command -v swipl`" != "" ] ; then
		echo -n "  swilgt"
	fi
	if [ -e `command -v xsblgt` ]  && [ "`command -v xsb`" != "" ] ; then
		echo -n "  xsblgt"
	fi
	if [ -e `command -v xsbmtlgt` ]  && [ "`command -v xsb-mt`" != "" ] ; then
		echo -n "  xsbmtlgt"
	fi
	if [ -e `command -v yaplgt` ]  && [ "`command -v yap`" != "" ] ; then
		echo -n "  yaplgt"
	fi
	echo
	exit 0
}


show_selected() {
    echo "Current Prolog integration script:"
    if [ -e `command -v logtalk` ] && [ "`command -v logtalk`" != "" ] ; then
		echo -n "  "
		readlink `command -v logtalk`
    else
        echo "  none"
    fi
	exit 0
}


usage_help() {
	echo 
	echo "This script allows the definition of a default back-end Prolog compiler by"
	echo "creating a symbolic link, \"logtalk\", to the corresponding integration script."
	echo
	echo "Usage:"
	echo "  `basename $0` integration-script"
	echo "  `basename $0` -v"
	echo "  `basename $0` -l"
	echo "  `basename $0` -s"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -v print script version"
	echo "  -l list available Prolog integration scripts"
	echo "  -s show the currently selected Prolog integration script"
	echo "  -h help"
	echo
	exit 0
}


valid_backend() {
	if [ "$1" == "bplgt" ] && [ -e `command -v bplgt` ]  && [ "`command -v bp`" != "" ] ; then
		return 0
	elif [ "$1" == "cxlgt" ] && [ -e `command -v cxlgt` ]  && [ "`command -v cxprolog`" != "" ] ; then
		return 0
	elif [ "$1" == "eclipselgt" ] && [ -e `command -v eclipselgt` ]  && [ "`command -v eclipse`" != "" ] ; then
		return 0
	elif [ "$1" == "gplgt" ] && [ -e `command -v gplgt` ]  && [ "`command -v gprolog`" != "" ] ; then
		return 0
	elif [ "$1" == "jiplgt" ] && [ -e `command -v jiplgt` ]  && [ "`command -v jipconsole.sh`" != "" ] ; then
		return 0
	elif [ "$1" == "lplgt" ] && [ -e `command -v lplgt` ]  && [ "`command -v lprolog`" != "" ] ; then
		return 0
	elif [ "$1" == "qplgt" ] && [ -e `command -v qplgt` ]  && [ "`command -v qp`" != "" ] ; then
		return 0
	elif [ "$1" == "sicstuslgt" ] && [ -e `command -v sicstuslgt` ]  && [ "`command -v sicstus`" != "" ] ; then
		return 0
	elif [ "$1" == "swilgt" ] && [ -e `command -v swilgt` ]  && [ "`command -v swipl`" != "" ] ; then
		return 0
	elif [ "$1" == "xsblgt" ] && [ -e `command -v xsblgt` ]  && [ "`command -v xsb`" != "" ] ; then
		return 0
	elif [ "$1" == "xsbmtlgt" ] && [ -e `command -v xsbmtlgt` ]  && [ "`command -v xsb-mt`" != "" ] ; then
		return 0
	elif [ "$1" == "yaplgt" ] && [ -e `command -v yaplgt` ]  && [ "`command -v yap`" != "" ] ; then
		return 0
	else
		return 1
	fi
}


switch_backend() {
	valid_backend $1
	if [ 0 != ${?} ]; then
    	echo "Invalid Prolog integration script: $1"
    	exit 1
	else
		cd $(dirname `command -v $1`)
		rm -f logtalk
		ln -sf $1 logtalk
		error=$?
		if [ 0 != $error ]; then
			echo "An error occurred when switching the default Prolog integration script!"
			echo "Check that you are executing this script with the necessary permissions."
			exit 1
    	else
			echo "Switched to the Prolog integration script:"
			echo "  $1"
			exit 0
		fi
	fi
}


while getopts "vlsh" option
do
	case $option in
		v) print_version;;
		l) list_backends;;
		s) show_selected;;
		h) usage_help;;
		*) usage_help;;
	esac
done


if [ "$1" == "" ]; then
	usage_help
else
	switch_backend $1
	error=$?
	if [ 0 != $error ]; then
		echo "An error occurred when switching to the $1 Prolog integration script!"
		exit 1
	fi
	exit 0
fi
