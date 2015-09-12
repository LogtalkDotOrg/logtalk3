#!/bin/bash

#############################################################################
## 
##   XML documenting files to plain text conversion script 
##   Last updated on November 3, 2014
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
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

xslt="$LOGTALKUSER/tools/lgtdoc/xml/lgttxt.xsl"

processor=xsltproc
# processor=xalan
# processor=sabcmd

directory="."

usage_help()
{
	echo 
	echo "This script converts all Logtalk XML documenting files in the"
	echo "current directory to text files"
	echo
	echo "Usage:"
	echo "  `basename $0` [-d directory] [-p processor]"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -d output directory for the text files (default is $directory)"
	echo "  -p XSLT processor (xsltproc, xalan, or sabcmd; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

while getopts "d:p:h" Option
do
	case $Option in
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! directory does not exists: $d_arg"
	usage_help
	exit 1
elif [ "$d_arg" != "" ] ; then
	directory=$d_arg
fi

if [ "$p_arg" != "" ] && [ "$p_arg" != "fop" ] && [ "$p_arg" != "xep" ] && [ "$p_arg" != "xinc" ] ; then
	echo "Error! Unsupported XSL-FO processor: $p_arg"
	usage_help
	exit 1
elif [ "$p_arg" != "" ] ; then
	processor=$p_arg
fi

if ! [ -e "./logtalk.dtd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk.dtd .
fi

if ! [ -e "./custom.ent" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/custom.ent .
fi

if ! [ -e "./logtalk.xsd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk.xsd .
fi

if [ `(grep -l "<logtalk" *.xml | wc -l) 2> /dev/null` -gt 0 ] ; then
	echo
	echo "converting XML files to text files..."
	for file in `grep -l "<logtalk" *.xml`; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory\"/\"$name.txt\" \"$xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory\"/\"$name.txt\" \"$file\" \"$xslt\";;
			sabcmd)		eval sabcmd \"$xslt\" \"$file\" \"$directory\"/\"$name.txt\";;
		esac
	done
	echo "conversion done"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

if [ "$PWD" != "$LOGTALKHOME"/xml ] ; then
	rm -f ./logtalk.dtd
	rm -f ./logtalk.xsd
	rm -f ./custom.ent
fi

exit 0
