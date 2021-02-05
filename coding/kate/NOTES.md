________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


This directory contains files that provide support for code folding and 
syntax highlighting for editing Logtalk source files with the KDE Kate 
and KWrite text editors (http://kate.kde.org/) and with the KDevelop IDE 
(http://www.kdevelop.org/).

Note that Kate or KWrite may include outdated syntax highlight support for
Logtalk source files. If that's the case, install the `logtalk.xml` in this
folder as described below to override the outdated syntax highlight support
file bundled with Kate or KWrite.

These support files are dual-licensed under the Apache License 2.0 and the
KDE license.

On POSIX systems, to install copy the file `logtalk.xml` to one of the
directories:

    KDE3:	$KDEDIR/share/apps/katepart/syntax/
    KDE4:	$KDEDIR/share/kde4/apps/katepart/syntax/
	KDE5:	$KDEDIR/share/katepart5/syntax

or:
    
    KDE3:	~/.kde/share/apps/katepart/syntax/
    KDE4:	~/.kde/share/kde4/apps/katepart/syntax/
	KDE5:	~/.local/share/katepart5/syntax

In recent versions, try:

	~/.local/share/org.kde.syntax-highlighting/syntax

On macOS systems, to install copy the file `logtalk.xml` to the directory:

	~/Library/Application Support/org.kde.syntax-highlighting/syntax

On Windows systems, to install copy the file `logtalk.xml` to the directory:

	%USERPROFILE%/AppData/Local/org.kde.syntax-highlighting/syntax

After, restart Kate and KWrite. Logtalk source files (including the library
entities and the programming examples) are indented using tabs (a common
setting is a tab width equivalent to 4 spaces); you may set the tab width
on the editor preference panel.

Note that you will either need to choose a theme that highlights all relevant
syntax elements (e.g. Vim dark) or play with the style settings for Logtalk
source files. The default theme doesn't highlight e.g. directives.
