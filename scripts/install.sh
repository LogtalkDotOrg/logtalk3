#!/bin/bash

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk installation script
##   Last updated on February 10, 2014
## 
##   This program is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##   
##   This program is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##   
##   You should have received a copy of the GNU General Public License
##   along with this program.  If not, see <http://www.gnu.org/licenses/>.
##   
##   Additional licensing terms apply per Section 7 of the GNU General
##   Public License 3. Consult the `LICENSE.txt` file for details.
## 
#############################################################################


if [ -f "/etc/debian_version" ]; then
	default_prefix=/usr
else
	case $( uname -s ) in
		Darwin	) default_prefix=/opt/local;;
		*		) default_prefix=/usr/local;;
	esac
fi

version=`cat ../VERSION.txt`
default_directory=logtalk-$version

print_version() {
	echo "Current `basename $0` version:"
	echo "  0.2"
	exit 0
}

usage_help()
{
	echo 
	echo "This script install Logtalk on a default or a user-specified directory."
	echo "It may require a user with administrative privileges or the use of sudo."
	echo
	echo "Usage:"
	echo "  `basename $0` [-p prefix] [-d directory]"
	echo "  `basename $0` -v"
	echo "  `basename $0` -h"
	echo
	echo "Optional arguments:"
	echo "  -p prefix directory for the installation (default is $default_prefix)"
	echo "  -d installation directory (default is $default_directory)"
	echo "  -v print script version"
	echo "  -h help"
	echo
	exit 1
}

while getopts "p:d:vh" option
do
	case $option in
		v) print_version;;
		p) prefix_argument="$OPTARG";;
		d) directory_argument="$OPTARG";;
		h) usage_help;;
		*) usage_help;;
	esac
done

if [ "$prefix_argument" != "" ] && [ ! -d "$prefix_argument" ] ; then
	echo "Error! Prefix directory does not exist: $prefix_argument"
	usage_help
	exit 1
elif [ "$prefix_argument" != "" ] ; then
	prefix=$prefix_argument
else
	prefix=$default_prefix
fi

if [ "$directory_argument" != "" ] ; then
	directory=$directory_argument
else
	directory=$default_directory
fi

if [ ! -w "$prefix" ] ; then
	echo "Error! No write permission for the prefix directory: $prefix"
	usage_help
	exit 1
fi

echo
echo "Installing Logtalk $version on $prefix/share ..."
echo

mkdir -p $prefix/share

rm -rf $prefix/share/$directory
rm -f $prefix/share/logtalk

mkdir $prefix/share/$directory

cd ..
cp -R * $prefix/share/$directory

cd $prefix/share/$directory
chmod a+x scripts/cleandist.sh
scripts/cleandist.sh

cd ..
ln -sf $directory logtalk

mkdir -p $prefix/bin
cd $prefix/bin

ln -sf ../share/logtalk/scripts/logtalk_tester.sh logtalk_tester
ln -sf ../share/logtalk/scripts/logtalk_user_setup.sh logtalk_user_setup
cp -f ../share/logtalk/scripts/logtalk_version_select.sh logtalk_version_select
ln -sf ../share/logtalk/scripts/logtalk_backend_select.sh logtalk_backend_select
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2pdf.sh lgt2pdf
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2html.sh lgt2html
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2xml.sh lgt2xml
ln -sf ../share/logtalk/tools/lgtdoc/xml/lgt2txt.sh lgt2txt

echo "Links to the \"logtalk_user_setup\", \"logtalk_backend_select\","
echo "\"logtalk_version_select\", \"logtalk_tester\", \"lgt2pdf\", \"lgt2html\","
echo "\"lgt2xml\", and \"lgt2txt\" scripts have been created on \"$prefix/bin\";"
echo "you may need to add this directory to your execution path."
echo

ln -sf ../share/logtalk/integration/bplgt.sh bplgt
ln -sf ../share/logtalk/integration/cxlgt.sh cxlgt
ln -sf ../share/logtalk/integration/eclipselgt.sh eclipselgt
ln -sf ../share/logtalk/integration/gplgt.sh gplgt
ln -sf ../share/logtalk/integration/lplgt.sh lplgt
ln -sf ../share/logtalk/integration/qplgt.sh qplgt
ln -sf ../share/logtalk/integration/quintuslgt.sh quintuslgt
ln -sf ../share/logtalk/integration/sicstuslgt.sh sicstuslgt
ln -sf ../share/logtalk/integration/swilgt.sh swilgt
ln -sf ../share/logtalk/integration/xsblgt.sh xsblgt
ln -sf ../share/logtalk/integration/xsb64lgt.sh xsb64lgt
ln -sf ../share/logtalk/integration/xsbmtlgt.sh xsbmtlgt
ln -sf ../share/logtalk/integration/xsbmt64lgt.sh xsbmt64lgt
ln -sf ../share/logtalk/integration/yaplgt.sh yaplgt

mkdir -p ../share/man/man1
cd ../share/man/man1
gzip --best ../../logtalk/man/man1/*.1
for file in ../../logtalk/man/man1/*.1.gz ; do
	ln -sf $file `basename $file`
done

echo "The following integration scripts are installed for running Logtalk"
echo "with selected back-end Prolog compilers:"
echo
echo "* B-Prolog (version 7.8 or later):         bplgt"
echo "* CxProlog (version 0.97.7 or later):      cxlgt"
echo "* ECLiPSe (version 6.1#143 or later):      eclipselgt"
echo "* GNU Prolog (version 1.4.2 or later):     gplgt"
echo "* Lean Prolog (version 3.8.8 or later):    lplgt       (experimental)"
echo "* Qu-Prolog (version 9.0 or later):        qplgt"
echo "* Quintus Prolog (version 3.3 or later):   quintuslgt  (experimental)"
echo "* SICStus Prolog (version 4.1.0 or later): sicstuslgt"
echo "* SWI-Prolog (version 6.0.0 or later):     swilgt"
echo "* XSB (version 3.4.1 or later):            xsblgt      (first run may require sudo)"
echo "* XSB 64 bits (version 3.4.1 or later):    xsb64lgt    (first run may require sudo)"
echo "* XSB MT (version 3.4.1 or later):         xsbmtlgt    (first run may require sudo)"
echo "* XSB MT 64 bits (version 3.4.1 or later): xsbmt64lgt  (first run may require sudo)"
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

if [ "`command -v update-mime-database`" != "" ]; then
	mkdir -p $prefix/share/mime/packages
	rm -f $prefix/share/mime/packages/logtalk.xml
	cp $prefix/share/$directory/scripts/freedesktop/logtalk.xml $prefix/share/mime/packages/logtalk.xml
	update-mime-database $prefix/share/mime
	echo "Added the Logtalk mime-type to the Shared MIME-info Database."
	echo
fi

echo "Logtalk $version basic installation completed."
echo
