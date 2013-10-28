#!/bin/sh

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk uninstall script
##   Last updated on October 28, 2013
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


if ! [ "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME should be defined first!"
	echo "Trying default Logtalk installation directories..."
	if [ -d "/usr/local/share/logtalk" ]; then
		LOGTALKHOME=/usr/local/share/logtalk
		echo "Using Logtalk installation at \"/usr/local/share/logtalk\""
	elif [ -d "/usr/share/logtalk" ]; then
		LOGTALKHOME=/usr/share/logtalk
		echo "Using Logtalk installation at \"/usr/share/logtalk\""
	elif [ -d "/opt/local/share/logtalk" ]; then
		LOGTALKHOME=/opt/local/share/logtalk
		echo "Using Logtalk installation at \"/opt/local/share/logtalk\""
	elif [ -d "/opt/share/logtalk" ]; then
		LOGTALKHOME=/opt/share/logtalk
		echo "Using Logtalk installation at \"/opt/share/logtalk\""
	else
		echo "Unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	elif ! [ -d "$LOGTALKHOME" ]; then
		echo "The environment variable LOGTALKHOME points to a non-existing directory!"
		echo "Its current value is: $LOGTALKHOME"
		echo "The variable must be set to your Logtalk installation directory!"
		echo
		exit 1
fi

version=`cat "$LOGTALKHOME/VERSION.txt"`
directory=logtalk-$version

echo
echo "Uninstalling Logtalk $version system-level files..."
echo

cd $LOGTALKHOME/..
rm -rf $directory
rm -f logtalk
cd ../bin
rm -f bplgt
rm -f cxlgt
rm -f eclipselgt
rm -f gplgt
rm -f lgt2html
rm -f lgt2pdf
rm -f lgt2xml
rm -f lgt2txt
rm -f logtalk_backend_select
rm -f logtalk_tester
rm -f logtalk_user_setup
rm -f logtalk_version_select
rm -f qplgt
rm -f sicstuslgt
rm -f swilgt
rm -f xsblgt
rm -f xsb64lgt
rm -f xsbmtlgt
rm -f xsbmt64lgt
rm -f yaplgt
cd ../share/man/man1
rm -f bplgt.1.gz
rm -f cxlgt.1.gz
rm -f eclipselgt.1.gz
rm -f lgt2html.1.gz
rm -f lgt2pdf.1.gz
rm -f lgt2txt.1.gz
rm -f lgt2xml.1.gz
rm -f logtalk_backend_select.1.gz
rm -f logtalk_tester.1.gz
rm -f logtalk_user_setup.1.gz
rm -f logtalk_version_select.1.gz
rm -f lplgt.1.gz
rm -f qplgt.1.gz
rm -f sicstuslgt.1.gz
rm -f swilgt.1.gz
rm -f xsb64lgt.1.gz
rm -f xsblgt.1.gz
rm -f xsbmt64lgt.1.gz
rm -f xsbmtlgt.1.gz
rm -f yaplgt.1.gz


echo "Logtalk $version system-level uninstall completed. For uninstalling user-level"
echo "Logtalk files simply delete the LOGTALKUSER directories."
echo
