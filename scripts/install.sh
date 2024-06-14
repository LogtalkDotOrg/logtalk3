#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk installation script
##   Last updated on December 17, 2023
## 
##   This file is part of Logtalk <https://logtalk.org/>  
##   SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
version_base=$(cat ../VERSION.txt | cut -f1 -d"-")
default_directory=logtalk-$version

print_version() {
	echo "Current $(basename "$0") version:"
	echo "  0.12"
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
echo "Installing Logtalk $version on \"$prefix/share\" ..."
echo

mkdir -p "$prefix/share"

rm -rf "$prefix/share/$directory"
rm -f "$prefix/share/logtalk"

mkdir -p "$prefix/share/$directory"

cp -R ../* "$prefix/share/$directory"

cd "$prefix/share/$directory" || exit 1
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
ln -sf "$directory" logtalk

mkdir -p "$prefix/bin"
cd "$prefix/bin" || exit 1

ln -sf ../share/logtalk/scripts/logtalk_allure_report.sh logtalk_allure_report
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
ln -sf ../share/logtalk/tools/packs/lgtenv.sh lgtenv

echo "Links to the following scripts have been created on \"$prefix/bin\":"
echo
echo "- logtalk_user_setup, logtalk_backend_select, logtalk_version_select"
echo "- logtalk_tester, logtalk_allure_report, logtalk_doclet"
echo "- lgt2svg, lgt2pdf, lgt2html, lgt2xml, lgt2md, lgt2rst, lgt2txt"
echo "- lgtenv"
echo
echo "Ensure that the \"$prefix/bin\" directory is in your PATH."

ln -sf ../share/logtalk/integration/bplgt.sh bplgt
ln -sf ../share/logtalk/integration/ciaolgt.sh ciaolgt
ln -sf ../share/logtalk/integration/cxlgt.sh cxlgt
ln -sf ../share/logtalk/integration/eclipselgt.sh eclipselgt
ln -sf ../share/logtalk/integration/gplgt.sh gplgt
ln -sf ../share/logtalk/integration/jiplgt.sh jiplgt
ln -sf ../share/logtalk/integration/xvmlgt.sh xvmlgt
ln -sf ../share/logtalk/integration/quintuslgt.sh quintuslgt
ln -sf ../share/logtalk/integration/sicstuslgt.sh sicstuslgt
ln -sf ../share/logtalk/integration/swilgt.sh swilgt
ln -sf ../share/logtalk/integration/taulgt.sh taulgt
ln -sf ../share/logtalk/integration/tplgt.sh tplgt
ln -sf ../share/logtalk/integration/xsblgt.sh xsblgt
ln -sf ../share/logtalk/integration/yaplgt.sh yaplgt

mkdir -p ../share/man/man1
cd ../share/man/man1 || exit 1
gzip --best ../../logtalk/man/man1/*.1
for file in ../../logtalk/man/man1/*.1.gz ; do
	ln -sf "$file" "$(basename "$file")"
done

if [ "$(command -v install-info)" != "" ]; then
	mkdir -p ../../info
	cd ../../info || exit 1
	if [ -f ../logtalk/docs/LogtalkAPIs-$version_base.info ] ; then
		cp ../logtalk/docs/LogtalkAPIs-$version_base.info .
		if [ -f dir ] ; then
			install-info --silent --delete LogtalkAPIs-*.info
		fi
		install-info LogtalkAPIs-$version_base.info dir
	fi
	if [ -f ../logtalk/manuals/TheLogtalkHandbook-$version_base.info ] ; then
		cp ../logtalk/manuals/TheLogtalkHandbook-$version_base.info .
		if [ -f dir ] ; then
			install-info --silent --delete TheLogtalkHandbook-*.info
		fi
		install-info TheLogtalkHandbook-$version_base.info dir
	fi
	echo "Ensure that the \"$prefix/share/info\" directory is in your INFOPATH."
fi

echo
echo "The following integration scripts are installed for running Logtalk"
echo "with selected backend Prolog compilers:"
echo
echo "* B-Prolog (8.1 or later):           bplgt       (experimental)"
echo "* Ciao Prolog (1.22.0 or later):     ciaolgt     (experimental; first run may require sudo)"
echo "* CxProlog (0.98.1 or later):        cxlgt"
echo "* ECLiPSe (6.1#143 or later):        eclipselgt"
echo "* GNU Prolog (1.4.5 or later):       gplgt"
echo "* JIProlog (4.1.7.1 or later):       jiplgt      (first run may require sudo)"
echo "* XVM (6.3.0 or later):              xvmlgt"
echo "* Quintus Prolog (3.3 or later):     quintuslgt  (experimental)"
echo "* SICStus Prolog (4.1.0 or later):   sicstuslgt"
echo "* SWI-Prolog (6.6.0 or later):       swilgt"
echo "* Tau Prolog (0.3.2 or later):       taulgt"
echo "* Trealla Prolog (2.18.7 or later):  tplgt"
echo "* XSB (3.8.0 or later):              xsblgt      (first run may require sudo)"
echo "* YAP (6.3.4 or later):              yaplgt"
echo
echo "Links to the Prolog integration scripts can be found on \"$prefix/bin\"."
echo "Make sure that the Prolog compilers are properly installed and available"
echo "on your execution path."
echo
echo "Users should ensure that the environment variable LOGTALKHOME is set to"
echo "\"$prefix/share/logtalk\" and then run the \"logtalk_user_setup\" shell script"
echo "once before running the integration scripts. For more details on manual"
echo "installation setup, see the \"INSTALL.md\" file."
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
