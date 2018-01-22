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


This directory contains files that provide support for using version 2.0 
or later of the source-highlight package by Lorenzo Bettini with Logtalk 
source files.  A detailed description on the source-highlight package is 
available from:

	http://www.gnu.org/software/src-highlite/

These support files are dual-licensed under the Apache License 2.0 and the
source-highlight license.

In order to check if your source-highlight installation already includes 
support for Logtalk, use the following command:

	% source-highlight --lang-list

If support for Logtalk is not included or if it is outdated, copy the file
"logtalk.lang" to the folder:

	$prefix/share/source-highlight

where $prefix is the source-highlight installation directory (by default, 
/usr/local) and then edit, if necessary, the file:

	$prefix/share/source-highlight/lang.map

and add the entries:

	lgt = logtalk.lang
	logtalk = logtalk.lang

Usage example for a "foo.lgt" Logtalk source file:

	% source-highlight --tab=4 --out-format xhtml --input foo.lgt --output foo.html

Usually, Logtalk files are formatted using tabs (the recommended setting is
a tab width equivalent to 4 spaces). You may want to use the "--tab=4" option
to keep you source code formatting choices.

You may also use the "logtalk.lang" file with plain Prolog files by using 
the "--src-lang" option. For example:

	% source-highlight --src-lang lgt --out-format xhtml --input foo.pl --output foo.html
