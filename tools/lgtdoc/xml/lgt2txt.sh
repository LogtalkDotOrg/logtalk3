#!/usr/bin/env bash

#############################################################################
##
##   XML documenting files to plain text conversion script
##   Last updated on March 21, 2025
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


print_version() {
	echo "$(basename "$0") 2.4"
	exit 0
}

source "$(dirname "$0")/logtalk_setup_env.sh"
setup_logtalk_env || exit 1

entity_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_txt.xsl"
index_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_txt.xsl"

processor=xsltproc
# processor=xalan
# processor=saxon

directory="."

usage_help() {
	echo
	echo "This script converts all Logtalk XML documenting files in the"
	echo "current directory to plain text files"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-d directory] [-p processor]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -d output directory for the text files (default is $directory)"
	echo "  -p XSLT processor (xsltproc, xalan, or saxon; default is $processor)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

while getopts "vd:p:h" option; do
	case $option in
		v) print_version;;
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! output directory does not exists: $d_arg" >&2
	usage_help
	exit 1
elif [ "$d_arg" != "" ] ; then
	directory=$d_arg
fi

if [ "$p_arg" != "" ] && [ "$p_arg" != "xsltproc" ] && [ "$p_arg" != "xalan" ] && [ "$p_arg" != "saxon" ] ; then
	echo "Error! Unsupported XSLT processor: $p_arg" >&2
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
	echo "Converting XML files to plain text files..."
	for file in $(grep -l "<logtalk_entity" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory/$name.txt\" \"$entity_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory/$name.txt\" \"$file\" \"$entity_xslt\";;
			saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.txt\" -s:\"$file\" -xsl:\"$entity_xslt\";;
		esac
	done
	for file in $(grep -l "<logtalk_index" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory/$name.txt\" \"$index_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory/$name.txt\" \"$file\" \"$index_xslt\";;
			saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.txt\" -s:\"$file\" -xsl:\"$index_xslt\";;
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
