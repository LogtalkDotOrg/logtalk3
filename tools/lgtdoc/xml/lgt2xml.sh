#!/usr/bin/env bash

#############################################################################
##
##   XML documenting files to XML conversion script
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
	echo "$(basename "$0") 1.4"
	exit 0
}

source "$(dirname "$0")/logtalk_setup_env.sh"
setup_logtalk_env || exit 1

format=xhtml
index_file=index.html
index_title="Documentation index"

usage_help() {
	echo
	echo "This script generates an index for all the Logtalk XML files"
	echo "documenting files in the current directory"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-f format] [-i index] [-t title]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -f format of the index file (either xhtml or html; default is $format)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used in the index file (default is $index_title)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

create_index_file() {
	echo "" > "$index_file"

	case "$format" in
		xhtml)
			echo "<?xml version=\"1.0\" encoding=\"utf-8\"?>" >> "$index_file"
			echo "<?xml-stylesheet href=\"logtalk.css\" type=\"text/css\"?>" >> "$index_file"
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" >> "$index_file"
			echo "<html lang=\"en\" xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">" >> "$index_file"
			;;
		html)
			echo "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" >> "$index_file"
			echo "<html>" >> "$index_file"
			;;
	esac

	echo "<head>" >> "$index_file"
	echo "    <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\"/>" >> "$index_file"
	echo "    <title>$index_title</title>" >> "$index_file"
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> "$index_file"
	echo "</head>" >> "$index_file"
	echo "<body>" >> "$index_file"
	echo "<h1>$index_title</h1>" >> "$index_file"
	echo "<ul>" >> "$index_file"

	if [ -e "./directory_index.xml" ] ; then
		echo "    <li><a href=\"library_index.xml\">Library index</a></li>" >> "$index_file"
		echo "    <li><a href=\"directory_index.xml\">Directory index</a></li>" >> "$index_file"
		echo "    <li><a href=\"entity_index.xml\">Entity index</a></li>" >> "$index_file"
		echo "    <li><a href=\"predicate_index.xml\">Predicate index</a></li>" >> "$index_file"
	else
		for file in $(grep -l "<logtalk_entity" ./*.xml); do
			base="${file##*/}"
			name="${base%.*}"
			entity="${name%_*}"
			pars="${name##*_}"
			echo "  indexing $base"
			if [ $pars -gt 0 ]
			then
				echo "    <li><a href=\"$base\">$entity/$pars</a></li>" >> "$index_file"
			else
				echo "    <li><a href=\"$base\">$entity</a></li>" >> "$index_file"
			fi
		done
	fi

	echo "</ul>" >> "$index_file"

	date="$(eval date)"

	echo "<p>Generated on $date</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}

while getopts "vf:i:t:h" option; do
	case $option in
		v) print_version;;
		f) f_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

if [ "$f_arg" != "" ] && [ "$f_arg" != "xhtml" ] && [ "$f_arg" != "html" ] ; then
	echo "Error! Unsupported output format: $f_arg" >&2
	usage_help
	exit 1
elif [ "$f_arg" != "" ] ; then
	format=$f_arg
fi

if [ "$i_arg" != "" ] ; then
	index_file=$i_arg
fi

if [ "$t_arg" != "" ] ; then
	index_title=$t_arg
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

if ! [ -e "./logtalk.css" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.css .
fi

if ! [ -e "./logtalk_entity_to_xml.xsl" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_entity_to_xml.xsl .
fi

if ! [ -e "./logtalk_index_to_xml.xsl" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_index_to_xml.xsl .
fi

if [ $( (ls ./*.xml | wc -l) 2> /dev/null) -gt 0 ] ; then
	echo "Indexing XML files..."
	echo
	echo "generating $(basename "$index_file") file..."
	create_index_file
	echo "$(basename "$index_file") file generated"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

exit 0
