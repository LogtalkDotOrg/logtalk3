________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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


This directory contains the Logtalk.plist file that provides basic syntax 
coloring, code completion, and entity index (using the "function" list)
for editing Logtalk source files with the Smultron 2.2 text editor:

	http://smultron.sourceforge.net/

These support files are dual-licensed under the Apache License 2.0 and the
Smultron license.

To install:

1. Copy the file "logtalk.plist" to the application bundle (ctrl-click on 
Smultron and choose Show Package Contents and then navigate to the folder
Contents/Resources/Syntax Definitions/).

2. Copy the file "SyntaxDefinitions.plist" to the folder:

	~/Library/Application Support/Smultron

If the file already exists, merge the contents of the two files.

Logtalk source files (including the library entities and the programming
examples) are formatted using tabs (the recommended setting is a tab width
equivalent to 4 spaces); you may set the tab width on the editor preference
panel.
