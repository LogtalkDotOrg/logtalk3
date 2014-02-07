#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk back-end Prolog compiler select script
##   Last updated on Febuary 7, 2014
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
	echo "Current `basename $0` version:"
	echo "  0.5"
	exit 0
}


list_backends() {
    echo "Available back-end Prolog compilers:"
	if [ -e `command -v bplgt` ]  && [ "`command -v bp`" != "" ] ; then
		echo -n "  bplgt"
	fi
	if [ -e `command -v cxlgt1` ]  && [ "`command -v cxprolog1`" != "" ] ; then
		echo -n "  cxlgt"
	fi
	if [ -e `command -v eclipselgt` ]  && [ "`command -v eclipse`" != "" ] ; then
		echo -n "  eclipselgt"
	fi
	if [ -e `command -v gplgt` ]  && [ "`command -v gprolog`" != "" ] ; then
		echo -n "  gplgt"
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
	if [ -e `command -v xsb64lgt` ]  && [ "`command -v xsb-bits64`" != "" ] ; then
		echo -n "  xsb64lgt"
	fi
	if [ -e `command -v xsbmtlgt` ]  && [ "`command -v xsb-mt`" != "" ] ; then
		echo -n "  xsbmtlgt"
	fi
	if [ -e `command -v xsbmt64lgt` ]  && [ "`command -v xsb-bits64-mt`" != "" ] ; then
		echo -n "  xsbmt64lgt"
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
	elif [ "$1" == "xsb64lgt" ] && [ -e `command -v xsb64lgt` ]  && [ "`command -v xsb-bits64`" != "" ] ; then
		return 0
	elif [ "$1" == "xsbmtlgt" ] && [ -e `command -v xsbmtlgt` ]  && [ "`command -v xsb-mt`" != "" ] ; then
		return 0
	elif [ "$1" == "xsbmt64lgt" ] && [ -e `command -v xsbmt64lgt` ]  && [ "`command -v xsb-bits64-mt`" != "" ] ; then
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
