#!/usr/bin/env bash

#############################################################################
## 
##   Logtalk uninstall script
##   Last updated on May 24, 2020
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
		echo "Unable to locate Logtalk installation directory!" >&2
		echo
		exit 1
	fi
	elif ! [ -d "$LOGTALKHOME" ]; then
		echo "The environment variable LOGTALKHOME points to a non-existing directory!" >&2
		echo "Its current value is: $LOGTALKHOME" >&2
		echo "The variable must be set to your Logtalk installation directory!" >&2
		echo
		exit 1
fi

version=$(cat "$LOGTALKHOME/VERSION.txt")
directory=logtalk-$version

echo
echo "Uninstalling Logtalk $version system-level files..."
echo

cd "$LOGTALKHOME"/.. || exit 1
rm -rf "$directory"
rm -f logtalk
cd ../bin || exit 1
rm -f bplgt
rm -f ciaolgt
rm -f cxlgt
rm -f eclipselgt
rm -f gplgt
rm -f jiplgt
rm -f lgt2svg
rm -f lgt2html
rm -f lgt2pdf
rm -f lgt2xml
rm -f lgt2md
rm -f lgt2rst
rm -f lgt2txt
rm -f logtalk_backend_select
rm -f logtalk_tester
rm -f logtalk_doclet
rm -f logtalk_user_setup
rm -f logtalk_version_select
rm -f lplgt
rm -f qplgt
rm -f quintuslgt
rm -f sicstuslgt
rm -f swilgt
rm -f taulgt
rm -f xsblgt
rm -f xsbmtlgt
rm -f yaplgt
cd ../share/man/man1 || exit 1
rm -f bplgt.1.gz
rm -f cxlgt.1.gz
rm -f eclipselgt.1.gz
rm -f gplgt.1.gz
rm -f jiplgt.1.gz
rm -f lgt2svg.1.gz
rm -f lgt2html.1.gz
rm -f lgt2pdf.1.gz
rm -f lgt2xml.1.gz
rm -f lgt2md.1.gz
rm -f lgt2rst.1.gz
rm -f lgt2txt.1.gz
rm -f logtalk_backend_select.1.gz
rm -f logtalk_tester.1.gz
rm -f logtalk_doclet.1.gz
rm -f logtalk_user_setup.1.gz
rm -f logtalk_version_select.1.gz
rm -f lplgt.1.gz
rm -f qplgt.1.gz
rm -f quintuslgt.1.gz
rm -f sicstuslgt.1.gz
rm -f swilgt.1.gz
rm -f taulgt.1.gz
rm -f xsblgt.1.gz
rm -f xsbmtlgt.1.gz
rm -f yaplgt.1.gz


echo "Logtalk $version system-level uninstall completed. For uninstalling user-level"
echo "Logtalk files simply delete the LOGTALKUSER directories."
echo
