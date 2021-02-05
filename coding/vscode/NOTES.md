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


DEPRECATED: For a more full featured Logtalk extension for the Visual Studio
Code text editor see instead:

https://marketplace.visualstudio.com/items?itemName=arthurwang.vsc-logtalk#overview

Notes for the contents of this directory follows.


This directory contains a preliminary extension for editing Logtalk source
files with the Visual Studio Code text editor: 

	http://code.visualstudio.com/

To install, copy the `pmoura.logtalk-0.0.2` folder to the following folder:

	~/.vscode/extensions

Create the folder if it does not exist. This extension includes support
for syntax highlighting and code folding. Snippets are also provided for
entities and predicate declarations. In the case of entities, the defined
triggers are `object`, `class`, `instance`, `protocol`, and `category`.
In the case of predicates, the defined triggers are `public`, `protected`,
and `private`.

Copy the file `tasks.json` to the your workspace `.vscode` folder. Edit it to
choose and customize the Prolog integration shortcut that you want to use.
Defined tasks include file compilation, running unit tests, scanning for
dead code, running doclets, generating HTML documentation, and generating
SVG diagrams.

Logtalk source files (including the library entities and the programming
examples) are indented using tabs and are expected to remain perfectly 
indented no matter your tab width preference (a common setting is a tab
width equivalent to 4 spaces).
