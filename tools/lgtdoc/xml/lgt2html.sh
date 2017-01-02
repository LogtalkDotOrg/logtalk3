#!/bin/bash

#############################################################################
## 
##   XML documenting files to (X)HTML conversion script 
##   Last updated on July 5, 2016
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
# processor=sabcmd

usage_help()
{
	echo 
	echo "This script converts all Logtalk XML files documenting files in the"
	echo "current directory to XHTML or HTML files"
	echo
	echo "Usage:"
	echo "  `basename $0` [-f format] [-d directory] [-i index] [-t title] [-p processor]"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -f output file format (either xhtml or html; default is $format)"
	echo "  -d output directory for the generated files (default is $directory)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used in the index file (default is $index_title)"
	echo "  -p XSLT processor (xsltproc, xalan, or sabcmd; default is $processor)"
	echo "  -h help"
	echo
	exit 1
}

create_index_file()
{
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
	echo "    <title>"$index_title"</title>" >> "$index_file"
	echo "    <link rel=\"stylesheet\" href=\"logtalk.css\" type=\"text/css\"/>" >> "$index_file"
	echo "</head>" >> "$index_file"
	echo "<body>" >> "$index_file"
	echo "<h1>"$index_title"</h1>" >> "$index_file"
	echo "<ul>" >> "$index_file"

	if [ -e "./directory_index.xml" ] ; then
		echo "    <li><a href=\"library_index.html\">Library index</a></li>" >> "$index_file"
		echo "    <li><a href=\"directory_index.html\">Directory index</a></li>" >> "$index_file"
		echo "    <li><a href=\"entity_index.html\">Entity index</a></li>" >> "$index_file"
		echo "    <li><a href=\"predicate_index.html\">Predicate index</a></li>" >> "$index_file"
	else
		for file in `grep -l "<logtalk_entity" *.xml`; do
			name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
			entity=${name%_*}
			pars=${name##*_}
			echo "  indexing $name.html"
			if [ $pars -gt 0 ]
			then
				echo "    <li><a href=\""$name".html\">"$entity"/"$pars"</a></li>" >> "$index_file"
			else
				echo "    <li><a href=\""$name".html\">"$entity"</a></li>" >> "$index_file"
			fi
		done
	fi

	echo "</ul>" >> "$index_file"

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}

while getopts "f:d:i:t:p:h" Option
do
	case $Option in
		f) f_arg="$OPTARG";;
		d) d_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$f_arg" != "" ] && [ "$f_arg" != "xhtml" ] && [ "$f_arg" != "html" ] ; then
	echo "Error! Unsupported output format: $f_arg"
	usage_help
	exit 1
elif [ "$f_arg" != "" ] ; then
	format=$f_arg
fi

if [ "$d_arg" != "" ] && [ ! -d "$d_arg" ] ; then
	echo "Error! directory does not exists: $d_arg"
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

if [ "$p_arg" != "" ] && [ "$p_arg" != "xsltproc" ] && [ "$p_arg" != "xalan" ] && [ "$p_arg" != "sabcmd" ] ; then
	echo "Error! Unsupported XSLT processor: $p_arg"
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

if [ `(grep -l "<logtalk" *.xml | wc -l) 2> /dev/null` -gt 0 ] ; then
	echo
	echo "converting XML files..."
	for file in `grep -l "<logtalk_entity" *.xml`; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory\"/\"$name.html\" \"$entity_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory\"/\"$name.html\" \"$file\" \"$entity_xslt\";;
			sabcmd)		eval sabcmd \"$entity_xslt\" \"$file\" \"$directory\"/\"$name.html\";;
		esac
	done
	for file in `grep -l "<logtalk_index" *.xml`; do
		echo "  converting $file"
		name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory\"/\"$name.html\" \"$index_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory\"/\"$name.html\" \"$file\" \"$index_xslt\";;
			sabcmd)		eval sabcmd \"$index_xslt\" \"$file\" \"$directory\"/\"$name.html\";;
		esac
	done
	echo "conversion done"
	echo
	index_file="$directory/$index_file"
	echo "generating $index_file file..."
	create_index_file
	echo "index $index_file generated"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

exit 0
