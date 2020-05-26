#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk installation script
##   Last updated on May 26, 2020
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


if [ -f "/etc/debian_version" ]; then
	default_prefix=/usr
else
	case $( uname -s ) in
		Darwin ) default_prefix=/opt/local;;
		*      ) default_prefix=/usr/local;;
	esac
fi

# allow using this script from any directory
cd "$(dirname "$0")" || exit 1

version=$(cat ../VERSION.txt)
default_directory=logtalk-$version

print_version() {
	echo "Current $(basename "$0") version:"
	echo "  0.4"
	exit 0
}

usage_help()
{
	echo 
	echo "This script install Logtalk on a default or a user-specified directory."
	echo "It may require a user with administrative privileges or the use of sudo."
	echo
	echo "Usage:"
	echo "  $(basename "$0") [-p prefix] [-d directory]"
	echo "  $(basename "$0") -v"
	echo "  $(basename "$0") -h"
	echo
	echo "Optional arguments:"
	echo "  -p prefix directory for the installation (default is $default_prefix)"
	echo "  -d installation directory (default is $default_directory)"
	echo "  -v print script version"
	echo "  -h help"
	echo
}

while getopts "p:d:vh" option
do
	case $option in
		v) print_version;;
		p) prefix_argument="$OPTARG";;
		d) directory_argument="$OPTARG";;
		h) usage_help; exit;;
		*) usage_help; exit;;
	esac
done

if [ "$prefix_argument" != "" ] && [ ! -d "$prefix_argument" ] ; then
	echo "Error! Prefix directory does not exist: $prefix_argument"
	usage_help
	exit 1
elif [ "$prefix_argument" != "" ] ; then
	prefix="$prefix_argument"
else
	prefix="$default_prefix"
fi

if [ "$directory_argument" != "" ] ; then
	directory="$directory_argument"
else
	directory="$default_directory"
fi

if [ ! -w "$prefix" ] ; then
	echo "Error! No write permission for the prefix directory: $prefix" >&2
	usage_help
	exit 1
fi

echo
echo "Installing Logtalk $version on $prefix/share ..."
echo

mkdir -p "$prefix/share"

rm -rf "$prefix/share/$directory"
rm -f "$prefix/share/logtalk"

mkdir "$prefix/share/$directory"

cp -R ../* "$prefix/share/$directory"

cd "$prefix/share/$directory" || exit 1
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
ln -sf "$directory" logtalk

mkdir -p "$prefix/bin"
cd "$prefix/bin" || exit 1

ln -sf ../share/logtalk/scripts/logtalk_tester.sh logtalk_tester
ln -sf ../share/logtalk/scripts/logtalk_doclet.sh logtalk_doclet
ln -sf ../share/logtalk/scripts/logtalk_user_setup.sh logtalk_user_setup
cp -f ../share/logtalk/scripts/logtalk_version_select.sh logtalk_version_select
ln -sf ../share/logtalk/scripts/logtalk_backend_select.sh logtalk_backend_select
ln -sf ../share/logtalk/tools/diagrams/lgt2svg.sh lgt2svg
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2pdf.sh lgt2pdf
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2html.sh lgt2html
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2xml.sh lgt2xml
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2md.sh lgt2md
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2rst.sh lgt2rst
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2txt.sh lgt2txt

echo "Links to the \"logtalk_user_setup\", \"logtalk_backend_select\","
echo "\"logtalk_version_select\", \"logtalk_tester\", \"logtalk_doclet\","
echo "\"lgt2svg\", \"lgt2pdf\", \"lgt2html\", \"lgt2xml\", \"lgt2md\","
echo "\"lgt2rst\" and \"lgt2txt\" scripts have been created on \"$prefix/bin\";"
echo "ensure that this directory is in your execution path."
echo

ln -sf ../share/logtalk/integration/bplgt.sh bplgt
ln -sf ../share/logtalk/integration/ciaolgt.sh ciaolgt
ln -sf ../share/logtalk/integration/cxlgt.sh cxlgt
ln -sf ../share/logtalk/integration/eclipselgt.sh eclipselgt
ln -sf ../share/logtalk/integration/gplgt.sh gplgt
ln -sf ../share/logtalk/integration/jiplgt.sh jiplgt
ln -sf ../share/logtalk/integration/lplgt.sh lplgt
ln -sf ../share/logtalk/integration/qplgt.sh qplgt
ln -sf ../share/logtalk/integration/quintuslgt.sh quintuslgt
ln -sf ../share/logtalk/integration/sicstuslgt.sh sicstuslgt
ln -sf ../share/logtalk/integration/swilgt.sh swilgt
ln -sf ../share/logtalk/integration/taulgt.sh taulgt
ln -sf ../share/logtalk/integration/xsblgt.sh xsblgt
ln -sf ../share/logtalk/integration/xsbmtlgt.sh xsbmtlgt
ln -sf ../share/logtalk/integration/yaplgt.sh yaplgt

mkdir -p ../share/man/man1
cd ../share/man/man1 || exit 1
gzip --best ../../logtalk/man/man1/*.1
for file in ../../logtalk/man/man1/*.1.gz ; do
	ln -sf "$file" "$(basename "$file")"
done

echo "The following integration scripts are installed for running Logtalk"
echo "with selected backend Prolog compilers:"
echo
echo "* B-Prolog (version 7.8 or later):         bplgt"
echo "* Ciao Prolog (version 1.19.0 or later):   ciaolgt     (first run may require sudo)"
echo "* CxProlog (version 0.98.1 or later):      cxlgt"
echo "* ECLiPSe (version 6.1#143 or later):      eclipselgt"
echo "* GNU Prolog (version 1.4.5 or later):     gplgt"
echo "* JIProlog (version 4.1.6.1 or later):     jiplgt      (first run may require sudo)"
echo "* Lean Prolog (version 4.5.4 or later):    lplgt       (experimental)"
echo "* Qu-Prolog (version 9.7 or later):        qplgt"
echo "* Quintus Prolog (version 3.3 or later):   quintuslgt  (experimental)"
echo "* SICStus Prolog (version 4.1.0 or later): sicstuslgt"
echo "* SWI-Prolog (version 6.6.0 or later):     swilgt"
echo "* Tau Prolog (version 0.3.0 or later):     taulgt      (experimental)"
echo "* XSB (version 3.8.0 or later):            xsblgt      (first run may require sudo)"
echo "* XSB MT (version 3.8.0 or later):         xsbmtlgt    (first run may require sudo)"
echo "* YAP (version 6.3.4 or later):            yaplgt"
echo
echo "Links to the Prolog integration scripts can be found on \"$prefix/bin\"."
echo "Make sure that the Prolog compilers are properly installed and available"
echo "on your execution path."
echo
echo "Users should ensure that the environment variable LOGTALKHOME is set to"
echo "\"$prefix/share/logtalk\" and then run the \"logtalk_user_setup\" shell script"
echo "once before running the integration scripts."
echo
echo "If you get an unexpected failure when using one of the Prolog integration"
echo "scripts, consult the \"$prefix/share/logtalk/adapters/NOTES.md\" file"
echo "for compatibility notes or consult the integration script man page."
echo

if [ "$(command -v update-mime-database)" != "" ]; then
	mkdir -p "$prefix/share/mime/packages"
	rm -f "$prefix/share/mime/packages/logtalk.xml"
	cp "$prefix/share/$directory/scripts/freedesktop/logtalk.xml" "$prefix/share/mime/packages/logtalk.xml"
	update-mime-database "$prefix/share/mime"
	echo "Added the Logtalk mime-type to the Shared MIME-info Database."
	echo
fi

echo "Logtalk $version basic installation completed."
echo
