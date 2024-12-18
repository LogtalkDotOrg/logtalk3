________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains the Logtalk.tmbundle bundle, which provides code 
folding, code completion, syntax coloring, entity index, auto-indentation, 
and snippets of code for editing Logtalk source files with the TextMate 1.x 
text editor: 

	http://macromates.com/

To install copy the bundle "Logtalk.tmbundle" to one of the following 
folders:

	~/Library/Application Support/TextMate/Bundles/
	/Library/Application Support/TextMate/Bundles/

(create the chosen folder if it does not exist).

The Logtalk.tmbundle itself is distributed under the default license for
TextMate language bundles; the Logtalk distribution license doesn't apply.

In order to use the Logtalk bundle commands to compile and automatically 
generate (X)HTML documentation for the frontmost source file window, open 
the TextMate Preferences window, select the "Advanced" tab, click on the 
"Shell Variables" button and add the following new variables:

LOGTALK_COMMAND
	set to the name of the shell script you use to launch Logtalk 
	followed by the option to run a goal at startup (e.g., "yaplgt -g")
LOGTALK_HTML_COMMAND
	set to the name of the shell script you use to generate the (X)HTML 
	documentation (e.g., lgt2html)
LOGTALK_PDF_COMMAND
	set to the name of the shell script you use to generate the PDF 
	documentation (e.g., lgt2pdf)
LOGTALK_TXT_COMMAND
	set to the name of the shell script you use to generate the TXT 
	documentation (e.g., lgt2txt)

If you're using the TmCodeBrowser plug-in available at:

	http://www.cocoabits.com/TmCodeBrowser/

Copy the `ctags` file to `$HOME/.ctags.tmcodebrowser` or append its
contents to the `$HOME/.ctags.tmcodebrowser` file if it already exists.

Logtalk source files (including the library entities and the programming
examples) are indented using tabs and are expected to remain perfectly 
indented no matter your tab width preference (a common setting is a tab
width equivalent to 4 spaces).
