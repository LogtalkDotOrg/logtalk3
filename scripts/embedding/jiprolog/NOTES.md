________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains example scripts for creating JIProlog `.jip` files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `jiprolog_logtalk_jip.sh`  
	Bash shell script for POSIX systems
- `jiprolog_logtalk_jip.ps1`  
	PowerShell script for Windows systems

Both scripts create a `logtalk.jip` file with the Logtalk compiler and
runtime and an optional `application.jip` file for an application.

The `.jip` files can be loaded using the `load/1` JIProlog predicate. It
is also possible to create a JAR file from the `.jip` files. For example,
by creating a `init.pl` with the contents:

	:- load('logtalk.jip').
	:- load('application.jip').

Possibly also with an `initialization/1` directive to auto-start the
application (using any Logtalk goal in canonical notation). And then
typing:

	$ jar cf logtalk.jar init.pl *.jip

The `logtalk.jar` file could then be distributed with the other JIProlog
`.jar` files. Logtalk could also be loaded on demand from the `logtalk.jar`
file by using the `load_library/1` JIProlog built-in predicate:

	| ?- load_library('logtalk.jar').

Usage
-----

Use `jiprolog_logtalk_jip.sh -h` or `jiprolog_logtalk_jip.ps1 -h` for a
list and description of the script options.

See the script usage examples in the `../SCRIPT.txt` file.
