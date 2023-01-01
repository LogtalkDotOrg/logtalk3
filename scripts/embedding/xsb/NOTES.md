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


This directory contains example scripts for creating XSB `.xwam` files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `xsb_logtalk_xwam.sh`  
	Bash shell script for POSIX systems
- `xsb_logtalk_xwam.ps1`  
	PowerShell script for Windows systems

Both scripts create a `logtalk.xwam` file with the Logtalk compiler and
runtime and an optional `application.xwam` file for an application.

The `.xwam` files can be loaded by XSB using the `loader:load/1` predicate.
For example:

	| ?- loader:load(logtalk).

Usage
-----

Use `xsb_logtalk_xwam.sh -h` or `xsb_logtalk_xwam.ps1 -h` for a list and
description of the script options.

See the script usage examples in the `../SCRIPT.txt` file.

Known issues
------------

A ISO Prolog standard compliance bug in the handling of `initialization/1`
directives by XSB currently prevents passing a settings file to the script.
The same bug may also result also in predicate redefined warnings when
loading generated `application.xwam` files. This bug may be fixed in
recent XSB versions.
