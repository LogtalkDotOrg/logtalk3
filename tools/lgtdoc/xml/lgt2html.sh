#!/usr/bin/env bash

#############################################################################
##
##   XML documenting files to (X)HTML conversion script
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

html_entity_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_html.xsl"
xhtml_entity_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_xhtml.xsl"

html_index_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_html.xsl"
xhtml_index_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_xhtml.xsl"

format=xhtml

directory="."

index_file=index.html
index_title="Documentation index"

processor=xsltproc
# processor=xalan
# processor=saxon

usage_help() {
	echo
	echo "This script converts all Logtalk XML files documenting files in the"
	echo "current directory to XHTML or HTML files"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-f format] [-d directory] [-i index] [-t title] [-p processor]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -f output file format (either xhtml or html; default is $format)"
	echo "  -d output directory for the generated files (default is $directory)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used in the index file (default is $index_title)"
	echo "  -p XSLT processor (xsltproc, xalan, or saxon; default is $processor)"
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
		echo "    <li><a href=\"library_index.html\">Library index</a></li>" >> "$index_file"
		echo "    <li><a href=\"directory_index.html\">Directory index</a></li>" >> "$index_file"
		echo "    <li><a href=\"entity_index.html\">Entity index</a></li>" >> "$index_file"
		echo "    <li><a href=\"predicate_index.html\">Predicate index</a></li>" >> "$index_file"
	else
		for file in $(grep -l "<logtalk_entity" ./*.xml); do
			base="${file##*/}"
			name="${base%.*}"
			entity="${name%_*}"
			pars="${name##*_}"
			echo "  indexing $name.html"
			if [ $pars -gt 0 ]
			then
				echo "    <li><a href=\"$name.html\">$entity/$pars</a></li>" >> "$index_file"
			else
				echo "    <li><a href=\"$name.html\">$entity</a></li>" >> "$index_file"
			fi
		done
	fi

	echo "</ul>" >> "$index_file"

	date="$(eval date)"

	echo "<p>Generated on $date</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}

while getopts "vf:d:i:t:p:h" option; do
	case $option in
		v) print_version;;
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

if [ "$f_arg" != "" ] && [ "$f_arg" != "xhtml" ] && [ "$f_arg" != "html" ] ; then
	echo "Error! Unsupported output format: $f_arg" >&2
	usage_help
	exit 1
elif [ "$f_arg" != "" ] ; then
	format=$f_arg
fi

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! output directory does not exists: $d_arg" >&2
	usage_help
	exit 1
elif [ "$d_arg" != "" ] ; then
	directory=$d_arg
fi

if [ "$i_arg" != "" ] ; then
	index_file=$i_arg
fi

if [ "$t_arg" != "" ] ; then
	index_title=$t_arg
fi

if [ "$p_arg" != "" ] && [ "$p_arg" != "xsltproc" ] && [ "$p_arg" != "xalan" ] && [ "$p_arg" != "saxon" ] ; then
	echo "Error! Unsupported XSLT processor: $p_arg" >&2
	usage_help
	exit 1
elif [ "$p_arg" != "" ] ; then
	processor=$p_arg
fi

if [ "$format" = "xhtml" ] ; then
	entity_xslt=$xhtml_entity_xslt
	index_xslt=$xhtml_index_xslt
else
	entity_xslt=$html_entity_xslt
	index_xslt=$html_index_xslt
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

if ! [ -e "$directory/logtalk.css" ] ; then
	cp "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.css "$directory"
fi

if grep -q "<logtalk" ./*.xml ; then
	echo
	echo "Converting XML files to (X)HTML files..."
	for file in $(grep -l "<logtalk_entity" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory/$name.html\" \"$entity_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory/$name.html\" \"$file\" \"$entity_xslt\";;
			saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.html\" -s:\"$file\" -xsl:\"$entity_xslt\";;
		esac
	done
	for file in $(grep -l "<logtalk_index" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory/$name.html\" \"$index_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory/$name.html\" \"$file\" \"$index_xslt\";;
			saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.html\" -s:\"$file\" -xsl:\"$index_xslt\";;
		esac
	done
	echo "conversion done"
	echo
	index_file="$directory/$index_file"
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
