#!/bin/sh

#############################################################################
## 
##   Logtalk user folder setup script
##   Last updated on April 9, 2019
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

echo
echo "This script copies all the Logtalk per-user files and directories to the"
echo "user home directory. The location can be set by the environment variable"
echo "\$LOGTALKUSER (defaults to \"~/logtalk\" when the variable is not defined)."
echo

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
	elif [ -d "$HOME/share/logtalk" ]; then
		LOGTALKHOME="$HOME/share/logtalk"
		echo "... using Logtalk installation found at $HOME/share/logtalk"
	elif [ -f "$( cd "$( dirname "$0" )" && pwd )/../core/core.pl" ]; then
		LOGTALKHOME="$( cd "$( dirname "$0" )" && pwd )/.."
		echo "... using Logtalk installation found at $( cd "$( dirname "$0" )" && pwd )/.."
	else
		echo "Unable to locate Logtalk installation directory!" >&2
		echo
		exit 1
	fi
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKHOME pointing to \"$LOGTALKHOME\"."
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!" >&2
	echo "Its current value is: $LOGTALKHOME" >&2
	echo "The variable must be set to your Logtalk installation directory!" >&2
	echo
	exit 1
fi

if ! [ "$LOGTALKUSER" ]
then
	LOGTALKUSER="$HOME/logtalk"
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKUSER pointing to \"$LOGTALKUSER\"."
	echo
fi

if [ -d "$LOGTALKUSER" ]
then
	date=$(eval date \"+%Y-%m-%d-%H%M%S\")
	mv "$LOGTALKUSER" "$LOGTALKUSER-backup-$date"
	echo "Created a backup of the existing \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER-backup-$date"
	echo
	echo "Creating a new \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER"
	echo
	mkdir "$LOGTALKUSER"
	if [ -f "$LOGTALKUSER-backup-$date"/settings.lgt ]
	then
		cp "$LOGTALKUSER-backup-$date"/settings.lgt "$LOGTALKUSER"/
		echo "Copied your old \"settings.lgt\" file to the new \"\$LOGTALKUSER\" directory."
	fi
	if [ -f "$LOGTALKUSER-backup-$date"/settings.logtalk ]
	then
		cp "$LOGTALKUSER-backup-$date"/settings.logtalk "$LOGTALKUSER"/
		echo "Copied your old \"settings.logtalk\" file to the new \"\$LOGTALKUSER\" directory."
	fi
	echo "If you are upgrading from a previous Logtalk version, check the file"
	echo "\"\$LOGTALKUSER/settings-sample.lgt\" for updated settings suggestions."
	echo
else
	echo "Creating a new \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER"
	echo
	mkdir "$LOGTALKUSER"
fi

echo "Copying Logtalk files and directories..."
mkdir -p "$LOGTALKUSER"/contributions
mkdir -p "$LOGTALKUSER"/docs
mkdir -p "$LOGTALKUSER"/examples
mkdir -p "$LOGTALKUSER"/library
mkdir -p "$LOGTALKUSER"/ports
mkdir -p "$LOGTALKUSER"/scratch
mkdir -p "$LOGTALKUSER"/tests
mkdir -p "$LOGTALKUSER"/tools
cp -RL "$LOGTALKHOME"/contributions "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/docs "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/examples "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/library "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/ports "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/scratch "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/tests "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/tools "$LOGTALKUSER"/
cp "$LOGTALKHOME"/loader-sample.lgt "$LOGTALKUSER"/loader-sample.lgt
cp "$LOGTALKHOME"/settings-sample.lgt "$LOGTALKUSER"/settings-sample.lgt
cp "$LOGTALKHOME"/tester-sample.lgt "$LOGTALKUSER"/tester-sample.lgt
cp "$LOGTALKHOME"/tests-sample.lgt "$LOGTALKUSER"/tests-sample.lgt
cp "$LOGTALKHOME"/VERSION.txt "$LOGTALKUSER"/
chmod -R u+w "$LOGTALKUSER"
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2*
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.dtd
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.xsd
ln -sf "$LOGTALKHOME"/ACKNOWLEDGMENTS.md "$LOGTALKUSER"/ACKNOWLEDGMENTS.md
ln -sf "$LOGTALKHOME"/BIBLIOGRAPHY.bib "$LOGTALKUSER"/BIBLIOGRAPHY.bib
ln -sf "$LOGTALKHOME"/CONTRIBUTING.md "$LOGTALKUSER"/CONTRIBUTING.md
ln -sf "$LOGTALKHOME"/CUSTOMIZE.md "$LOGTALKUSER"/CUSTOMIZE.md
ln -sf "$LOGTALKHOME"/INSTALL.md "$LOGTALKUSER"/INSTALL.md
ln -sf "$LOGTALKHOME"/LICENSE.txt "$LOGTALKUSER"/LICENSE.txt
ln -sf "$LOGTALKHOME"/QUICK_START.md "$LOGTALKUSER"/QUICK_START.md
ln -sf "$LOGTALKHOME"/README.md "$LOGTALKUSER"/README.md
ln -sf "$LOGTALKHOME"/RELEASE_NOTES.md "$LOGTALKUSER"/RELEASE_NOTES.md
ln -sf "$LOGTALKHOME"/UPGRADING.md "$LOGTALKUSER"/UPGRADING.md
ln -sf "$LOGTALKHOME"/adapters "$LOGTALKUSER"/adapters
ln -sf "$LOGTALKHOME"/coding "$LOGTALKUSER"/coding
ln -sf "$LOGTALKHOME"/integration "$LOGTALKUSER"/integration
ln -sf "$LOGTALKHOME"/manuals "$LOGTALKUSER"/manuals
ln -sf "$LOGTALKHOME"/paths "$LOGTALKUSER"/paths
ln -sf "$LOGTALKHOME"/scripts "$LOGTALKUSER"/scripts
ln -sf "$LOGTALKHOME"/tools/diagrams/lgt2svg.sh "$LOGTALKUSER"/tools/diagrams/lgt2svg
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2html.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2html
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2pdf.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2pdf
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2xml.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2xml
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2md.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2md
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2txt.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2txt
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.dtd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_entity.dtd
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.rng "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_entity.rng
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_entity.xsd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_entity.xsd
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_index.dtd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_index.dtd
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_index.rng "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_index.rng
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk_index.xsd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk_index.xsd
echo "Finished copying Logtalk files and directories."
echo
echo "You may want to customize the default Logtalk compiler flags by renaming"
echo "and editing the \"settings-sample.lgt\" source file found in the directory"
echo "\"\$LOGTALKUSER\". For more information on how to customize Logtalk and the"
echo "working environment, consult the \"\$LOGTALKUSER/CUSTOMIZE.md\" file."
echo
