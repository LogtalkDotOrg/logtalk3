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


This directory contains files that provide support for using version 3.15 
or later of the Highlight package by Andre Simon with Logtalk source files.
A detailed description on the Highlight package is available from:

	http://www.andre-simon.de/


These support files are dual-licensed under the Apache License 2.0 and the
Highlight license.

In order to check if your Highlight installation already includes support 
for Logtalk, you may use the following command:

	% highlight --list-scripts=langs

Look in the resulting listing for the `lgt` extension. If support for Logtalk 
is not included or if it is outdated, copy the file `logtalk.lang` to the
folder:

	$prefix/share/highlight/langDefs/

where `$prefix` is the Highlight installation directory (by default, `/usr`).
There's also a `lgt.lang` file for older 2.x versions of Highlight.

Usage example for a `foo.lgt` Logtalk source file:

	% highlight --replace-tabs=4 --out-format=xhtml --input=foo.lgt --output=foo.html --style=molokai

You should try different style files (using the `--style` option) in order to 
find which ones provide the more satisfactory results. As Logtalk files are 
usually indented using tabs (a common setting is a tab width equivalent to 4
spaces), the option `--replace-tabs=4` may be necessary to keep you source
code properly indented.

You may also use the `logtalk.lang` file with plain Prolog files by using the 
`--syntax` option. For example:

	% highlight --syntax=lgt --out-format=xhtml --input=foo.pl --output=foo.html

THIS SYNTAX COLORING FILE IS UNDER DEVELOPMENT.
