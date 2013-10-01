#!/bin/sh

#############################################################################
## 
##   This file is part of Logtalk <http://logtalk.org/>  
##   Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>
## 
##   Logtalk user folder setup script
##   Last updated on October 1, 2013
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

echo
echo "This script copies the Logtalk per-user files and directories to the"
echo "user home directory. The location can be set by the environment"
echo "variable \$LOGTALKUSER (defaults to \"~/logtalk\" when the variable is not"
echo "defined)"
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
		echo "Unable to locate Logtalk installation directory!"
		echo
		exit 1
	fi
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKHOME pointing to \"$LOGTALKHOME\"."
	echo
elif ! [ -d "$LOGTALKHOME" ]; then
	echo "The environment variable LOGTALKHOME points to a non-existing directory!"
	echo "Its current value is: $LOGTALKHOME"
	echo "The variable must be set to your Logtalk installation directory!"
	echo
	exit 1
fi

if ! [ "$LOGTALKUSER" ]
then
	LOGTALKUSER=$HOME/logtalk
	echo "After the script completion, you must set the environment variable"
	echo "LOGTALKUSER pointing to \"$LOGTALKUSER\"."
	echo
fi

if [ -d "$LOGTALKUSER" ]
then
	date=`eval date \"+%Y-%m-%d-%H%M%S\"`
	mv $LOGTALKUSER "$LOGTALKUSER-backup-$date"
	echo "Created a backup of the existing \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER-backup-$date"
	echo
	echo "Creating a new \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER"
	echo
	mkdir $LOGTALKUSER
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
	echo "The file \"settings-sample.lgt\" file contains an up-to-date copy of the"
	echo "\"settings-sample.lgt\" file distributed with the currently installed Logtalk"
	echo "version. Review this file for possible settings files update information."
	echo
else
	echo "Creating a new \"\$LOGTALKUSER\" directory:"
	echo
	echo "  $LOGTALKUSER"
	echo
	mkdir $LOGTALKUSER
fi

echo "Copying Logtalk files and directories..."
mkdir -p "$LOGTALKUSER"/contributions
mkdir -p "$LOGTALKUSER"/docs
mkdir -p "$LOGTALKUSER"/examples
mkdir -p "$LOGTALKUSER"/library
mkdir -p "$LOGTALKUSER"/scratch
mkdir -p "$LOGTALKUSER"/tests
mkdir -p "$LOGTALKUSER"/tools
cp -RL "$LOGTALKHOME"/contributions "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/docs "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/examples "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/library "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/scratch "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/tests "$LOGTALKUSER"/
cp -RL "$LOGTALKHOME"/tools "$LOGTALKUSER"/
cp "$LOGTALKHOME"/settings-sample.lgt "$LOGTALKUSER"/settings-sample.lgt
cp "$LOGTALKHOME"/VERSION.txt "$LOGTALKUSER"/
chmod -R u+w "$LOGTALKUSER"
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2*
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.dtd
rm -f "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.xsd
ln -sf "$LOGTALKHOME"/BIBLIOGRAPHY.bib "$LOGTALKUSER"/BIBLIOGRAPHY.bib
ln -sf "$LOGTALKHOME"/CUSTOMIZE.md "$LOGTALKUSER"/CUSTOMIZE.md
ln -sf "$LOGTALKHOME"/INSTALL.md "$LOGTALKUSER"/INSTALL.md
ln -sf "$LOGTALKHOME"/LICENSE.txt "$LOGTALKUSER"/LICENSE.txt
ln -sf "$LOGTALKHOME"/QUICK_START.md "$LOGTALKUSER"/QUICK_START.md
ln -sf "$LOGTALKHOME"/README.md "$LOGTALKUSER"/README.md
ln -sf "$LOGTALKHOME"/RELEASE_NOTES.md "$LOGTALKUSER"/RELEASE_NOTES.md
ln -sf "$LOGTALKHOME"/UPGRADING.md "$LOGTALKUSER"/UPGRADING.md
ln -sf "$LOGTALKHOME"/adapters "$LOGTALKUSER"/adapters
ln -sf "$LOGTALKHOME"/manuals "$LOGTALKUSER"/manuals
ln -sf "$LOGTALKHOME"/paths "$LOGTALKUSER"/paths
ln -sf "$LOGTALKHOME"/coding "$LOGTALKUSER"/coding
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2html.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2html
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2pdf.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2pdf
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2xml.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2xml
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/lgt2txt.sh "$LOGTALKUSER"/tools/lgtdoc/xml/lgt2txt
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk.dtd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.dtd
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk.rng "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.rng
ln -sf "$LOGTALKHOME"/tools/lgtdoc/xml/logtalk.xsd "$LOGTALKUSER"/tools/lgtdoc/xml/logtalk.xsd
echo "Finished copying Logtalk files and directories."
echo
echo "You may want to customize the default Logtalk compiler flags by renaming"
echo "and editing the \"settings-sample.lgt\" file found in the directory"
echo "\"\$LOGTALKUSER\". For more information on customizing Logtalk and your"
echo "working environment, consult the \"\$LOGTALKUSER/CUSTOMIZE.md\" file."
echo
