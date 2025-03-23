#!/usr/bin/env bash

#############################################################################
##
##   XML documenting files to reStructuredText files conversion script
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
	echo "$(basename "$0") 5.3"
	exit 0
}

source "$(dirname "$0")/logtalk_setup_env.sh"
setup_logtalk_env || exit 1

entity_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_entity_to_rst.xsl"
index_xslt="$LOGTALKUSER/tools/lgtdoc/xml/logtalk_index_to_rst.xsl"

processor=xsltproc
# processor=xalan
# processor=saxon

directory="."

sphinx=false
make_html=false
mapping=""

index_file=index.rst
index_title="Documentation index"

usage_help() {
	echo
	echo "This script converts all Logtalk XML documenting files in the"
	echo "current directory to reStructuredText files for use with Sphinx"
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-d directory] [-i index] [-t title] [-p processor] [-s] [-m] [-l mapping]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -d output directory for the text files (default is $directory)"
	echo "  -i name of the index file (default is $index_file)"
	echo "  -t title to be used in the index file (default is $index_title)"
	echo "  -p XSLT processor (xsltproc, xalan, or saxon; default is $processor)"
	echo "  -s run sphinx-quickstart script"
	echo "  -m run make html (requires -s option)"
	echo "  -l Intersphinx mapping for linking library APIs to library descriptions (requires -s option)"
	echo "  -- arguments to be passed to sphinx-quickstart script (no default)"
	echo "  -v print version"
	echo "  -h help"
	echo
}

create_index_file() {
	echo "" > "$index_file"

	echo "$index_title" >> "$index_file"
	num=${#index_title}
	eval $(echo printf '"=%0.s"' {1..$num}) >> "$index_file"
	echo "" >> "$index_file"
	echo "" >> "$index_file"

	if [ -e "./directory_index.xml" ] ; then

		echo ".. toctree::" >> "$index_file"
		echo "   :maxdepth: 3" >> "$index_file"
		echo "   :caption: Contents" >> "$index_file"
		echo "" >> "$index_file"
		echo "   library_index" >> "$index_file"
		echo "   directory_index" >> "$index_file"
		echo "   entity_index" >> "$index_file"
		echo "   predicate_index" >> "$index_file"
		echo "" >> "$index_file"
		echo "Indices and tables" >> "$index_file"
		echo "==================" >> "$index_file"
		echo "" >> "$index_file"
		echo "* :ref:\`genindex\`" >> "$index_file"
		echo "* :ref:\`search\`" >> "$index_file"
	else
		echo ".. toctree::" >> "$index_file"
		echo "   :maxdepth: 1" >> "$index_file"
		echo "" >> "$index_file"
		for file in $(grep -l "<logtalk_entity" ./*.xml); do
			base="${file##*/}"
			name="${base%.*}"
			entity="${name%_*}"
			pars="${name##*_}"
			echo "  indexing $name.rst"
			if [ $pars -gt 0 ]
			then
				echo "   $entity/$pars <$name>" >> "$index_file"
			else
				echo "   $entity <$name>" >> "$index_file"
			fi
		done
	fi

	date="$(eval date)"
	echo "" >> "$index_file"
	echo "Generated on $date" >> "$index_file"
}

while getopts "vd:i:t:p:l:smh" option; do
	case $option in
		v) print_version;;
		d) d_arg="$OPTARG";;
		i) i_arg="$OPTARG";;
		t) t_arg="$OPTARG";;
		p) p_arg="$OPTARG";;
		l) l_arg="$OPTARG";;
		s) sphinx=true;;
		m) make_html=true;;
		h) usage_help; exit 0;;
		*) usage_help; exit 1;;
	esac
done

shift $((OPTIND - 1))
args=("$@")

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

if [ "$l_arg" != "" ] ; then
	mapping=$l_arg
fi

if ! [ -x "$(command -v $processor)" ] ; then
	echo "Error: Cannot find the $processor command-line tool!" >&2
	exit 1
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
	echo "Converting XML files to reStructuredText files..."
	for file in $(grep -l "<logtalk_entity" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		case "$processor" in
			xsltproc)	eval xsltproc -o \"$directory/$name.rst\" \"$entity_xslt\" \"$file\";;
			xalan)		eval xalan -o \"$directory/$name.rst\" \"$file\" \"$entity_xslt\";;
			saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.rst\" -s:\"$file\" -xsl:\"$entity_xslt\";;
		esac
	done
	for file in $(grep -l "<logtalk_index" ./*.xml); do
		echo "  converting $(basename "$file")"
		base="${file##*/}"
		name="${base%.*}"
		if [ "$mapping" != "" ] ; then
			case "$processor" in
				xsltproc)	eval xsltproc --stringparam mapping "$mapping" -o \"$directory/$name.rst\" \"$index_xslt\" \"$file\";;
				xalan)		eval xalan -p mapping \"\'"$mapping"\'\" -o \"$directory/$name.rst\" \"$file\" \"$index_xslt\";;
				saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.rst\" -s:\"$file\" -xsl:\"$index_xslt\" mapping="$mapping";;
			esac
		else
			case "$processor" in
				xsltproc)	eval xsltproc -o \"$directory/$name.rst\" \"$index_xslt\" \"$file\";;
				xalan)		eval xalan -o \"$directory/$name.rst\" \"$file\" \"$index_xslt\";;
				saxon)		eval java net.sf.saxon.Transform -o:\"$directory/$name.rst\" -s:\"$file\" -xsl:\"$index_xslt\";;
			esac
		fi
	done
	echo "conversion done"
	echo
	index_file="$directory/$index_file"
	echo "generating $(basename "$index_file") file..."
	create_index_file
	echo "$(basename "$index_file") file generated"
	echo
	if [ "$sphinx" = true ] ; then
		mv index.rst index.rst.backup
		if [ "$mapping" != "" ] ; then
			sphinx-quickstart --ext-intersphinx --templatedir="$LOGTALKUSER/tools/lgtdoc/xml" "${args[@]}"
		else
			sphinx-quickstart --templatedir="$LOGTALKUSER/tools/lgtdoc/xml" "${args[@]}"
		fi
		mkdir -p _static/css
		cp "$LOGTALKUSER/tools/lgtdoc/xml/css/sphinx/custom.css" _static/css/custom.css
		mv index.rst.backup index.rst
	fi
	if [ "$sphinx" = true ] && [ "$make_html" = true ] ; then
		make html
	fi
else
	echo
	echo "No XML files exist in the current directory!"
	echo
fi

if [ "$PWD" != "$LOGTALKHOME"/xml ] ; then
	rm -f ./logtalk_entity.dtd
	rm -f ./logtalk_index.dtd
	rm -f ./logtalk_entity.xsd
	rm -f ./logtalk_index.xsd
	rm -f ./custom.ent
fi

exit 0
