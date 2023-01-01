________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains files that provide syntax highlighting, automatic
code indenting, code completion, and programming templates for editing 
Logtalk source files with the text editor jEdit 4.3 or later version:

	http://www.jedit.org/

These support files are dual-licensed under the Apache License 2.0 and the
jEdit license.

To install:

1.	Copy the file logtalk.xml to the modes subdirectory of your jEdit 
	installation directory.

2.	Open the "catalog" file in the same subdirectory and add (if not 
	present) the following entries:

	<MODE NAME="logtalk" FILE="logtalk.xml" FILE_NAME_GLOB="*.{lgt,logtalk}" />

3.	Install (or update if necessary) the jEdit plug-in Templates 4.0.0
	or later version.

4.	Copy the contents of the subdirectory templates to the templates 
	directory set in the plug-in preferences.

5.	Install the jEdit plug-in CodeBrowser 1.4.2 or later version and the 
	Logtalk support for Exuberant C Tags found on "coding/ctags" directory.

Notes:

You can use the jEdit code folding indent mode with Logtalk source files.

You can customize the templates for objects, categories, and protocols 
to use your name by default by changing the line that reads:

#prompt ( "Author name:" $authorname )

to:

#prompt ( "Author name:" $authorname "Your Name" )
