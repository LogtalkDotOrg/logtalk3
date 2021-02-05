#!/usr/bin/env bash

#############################################################################
## 
##   XML documenting files to plain text conversion script 
##   Last updated on November 20, 2019
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


print_version() {
	echo "$(basename "$0") 1.0"
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

entity_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_md.xsl"
index_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_md.xsl"

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
	echo "  $(basename "$0") [-d directory] [-p processor]"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -v print version of $(basename "$0")"
	echo "  -d output directory for the text files (default is $directory)"
	echo "  -p XSLT processor (xsltproc, xalan, or sabcmd; default is $processor)"
	echo "  -h help"
	echo
}

while getopts "vd:p:h" Option
do
	case $Option in
		v) print_version;;
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! directory does not exists: $d_arg" >&2
	usage_help
	exit 1
elif [ "$d_arg" != "" ] ; then
	directory=$d_arg
fi

if [ "$p_arg" != "" ] && [ "$p_arg" != "fop" ] && [ "$p_arg" != "xep" ] && [ "$p_arg" != "xinc" ] ; then
	echo "Error! Unsupported XSL-FO processor: $p_arg" >&2
	usage_help
	exit 1
elif [ "$p_arg" != "" ] ; then
	processor=$p_arg
fi

if ! [ -e "./logtalk_entity.dtd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.dtd .
fi

if ! [ -e "./logtalk_index.dtd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_index.dtd .
fi

if ! [ -e "./custom.ent" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/custom.ent .
fi

if ! [ -e "./logtalk_entity.xsd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.xsd .
fi

if ! [ -e "./logtalk_index.xsd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_index.xsd .
fi

if grep -q "<logtalk" ./*.xml ; then
	echo
	echo "converting XML files to text files..."
	for file in $(grep -l "<logtalk_entity" ./*.xml); do
		echo "  converting $(basename "$file")"
		name="$(expr "$file" : '\(.*\)\.[^./]*$' \| "$file")"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory\"/\"$name.txt\" \"$entity_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory\"/\"$name.txt\" \"$file\" \"$entity_xslt\";;
			sabcmd)		eval sabcmd \"$entity_xslt\" \"$file\" \"$directory\"/\"$name.txt\";;
		esac
	done
	for file in $(grep -l "<logtalk_index" ./*.xml); do
		echo "  converting $(basename "$file")"
		name="$(expr "$file" : '\(.*\)\.[^./]*$' \| "$file")"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory\"/\"$name.txt\" \"$index_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory\"/\"$name.txt\" \"$file\" \"$index_xslt\";;
			sabcmd)		eval sabcmd \"$index_xslt\" \"$file\" \"$directory\"/\"$name.txt\";;
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
	rm -f ./logtalk_entity.dtd
	rm -f ./logtalk_entity.xsd
	rm -f ./logtalk_index.dtd
	rm -f ./logtalk_index.xsd
	rm -f ./custom.ent
fi

exit 0
