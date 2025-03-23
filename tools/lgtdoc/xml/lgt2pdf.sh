#!/usr/bin/env bash

#############################################################################
##
##   XML documenting files to PDF conversion script
##   Last updated on March 23, 2025
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
a4_xsl="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_pdf_a4.xsl"
us_xsl="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_pdf_us.xsl"

format=a4
# format=us

processor=fop
# processor=fop2
# processor=xep
# processor=xinc

directory="."

usage_help() {
	echo
	echo "This script converts all Logtalk XML documenting files in the"
	echo "current directory to PDF files"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-f format] [-d directory] [-p processor]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -f paper format (either a4 or us; default is $format)"
	echo "  -d output directory for the PDF files (default is $directory)"
	echo "  -p XSL-FO processor (either fop, fop2, xep, or xinc; default is $processor)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

while getopts "vf:d:p:h" option; do
	case $option in
		v) print_version;;
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

if [ "$f_arg" != "" ] && [ "$f_arg" != "a4" ] && [ "$f_arg" != "us" ] ; then
	echo "Error! Unsupported output format: $f_arg" >&2
	usage_help
	exit 1
elif [ "$f_arg" != "" ]
then
	format=$f_arg
fi

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! output directory does not exists: $d_arg" >&2
	usage_help
	exit 1
elif [ "$d_arg" != "" ] ; then
	directory=$d_arg
fi

if [ "$p_arg" != "" ] && [ "$p_arg" != "fop" ] && [ "$p_arg" != "fop2" ] && [ "$p_arg" != "xep" ] && [ "$p_arg" != "xinc" ] ; then
	echo "Error! Unsupported XSL-FO processor: $p_arg" >&2
	usage_help
	exit 1
elif [ "$p_arg" != "" ] ; then
	processor=$p_arg
fi

if [ "$format" = "a4" ] ; then
	xsl=$a4_xsl
else
	xsl=$us_xsl
fi

if ! [ -e "./logtalk_entity.dtd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.dtd .
fi

if ! [ -e "./custom.ent" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/custom.ent .
fi

if ! [ -e "./logtalk_entity.xsd" ] ; then
	cp "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.xsd .
fi

if grep -q "<logtalk" ./*.xml ; then
	echo
	echo "Converting XML files to PDF files..."
	for file in $(grep -l "<logtalk_entity" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case $processor in
			xinc)	eval xinc -xml \"$file\" -xsl \"$xsl\" -pdf \"$directory/$name.pdf\" 2> /dev/null;;
			*)		eval $processor -q -xml \"$file\" -xsl \"$xsl\" -pdf \"$directory/$name.pdf\";;
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
	rm -f ./custom.ent
fi

exit 0
