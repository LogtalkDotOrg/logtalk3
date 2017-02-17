#!/usr/bin/env bash

#############################################################################
## 
##   XML documenting files to XML conversion script 
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

format=xhtml
index_file=index.html
index_title="Documentation index"

usage_help()
{
	echo 
	echo "This script generates an index for all the Logtalk XML files"
	echo "documenting files in the current directory"
	echo
	echo "Usage:"
	echo "  `basename $0` [-f format] [-i index] [-t title]"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -f format of the index file (either xhtml or html; default is $format)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used in the index file (default is $index_title)"
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
		echo "    <li><a href=\"library_index.xml\">Library index</a></li>" >> "$index_file"
		echo "    <li><a href=\"directory_index.xml\">Directory index</a></li>" >> "$index_file"
		echo "    <li><a href=\"entity_index.xml\">Entity index</a></li>" >> "$index_file"
		echo "    <li><a href=\"predicate_index.xml\">Predicate index</a></li>" >> "$index_file"
	else
		for file in `grep -l "<logtalk_entity" *.xml`; do
			name="`expr "$file" : '\(.*\)\.[^./]*$' \| "$file"`"
			entity=${name%_*}
			pars=${name##*_}
			echo "  indexing $file"
			if [ $pars -gt 0 ]
			then
				echo "    <li><a href=\""$file"\">"$entity"/"$pars"</a></li>" >> "$index_file"
			else
				echo "    <li><a href=\""$file"\">"$entity"</a></li>" >> "$index_file"
			fi
		done
	fi

	echo "</ul>" >> "$index_file"

	date="`eval date`"

	echo "<p>Generated on "$date"</p>" >> "$index_file"
	echo "</body>" >> "$index_file"
	echo "</html>" >> "$index_file"
}

while getopts "f:i:t:h" Option
do
	case $Option in
		f) f_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
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

if [ `(ls *.xml | wc -l) 2> /dev/null` -gt 0 ] ; then
	echo
	echo "generating $index_file file..."
	create_index_file
	echo "$index_file file generated"
	echo
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

exit 0
