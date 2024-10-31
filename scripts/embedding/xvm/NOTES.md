________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains example scripts for creating XVM Prolog files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `xvm_logtalk_pl.sh`  
	Bash shell script for POSIX systems
- `xvm_logtalk_pl.ps1`  
	PowerShell script for Windows systems

Both scripts create a `logtalk.pl` file with the Logtalk compiler and
runtime and an optional `application.pl` file for an application plus
an optional application startup goal. When a loader option is used, a
`loader.pl` file is also created.

When the encrypt option is used, `*.plx` files are generated instead
of `*.pl` files.

Usage
-----

Use `xvm_logtalk_pl.sh -h` or `xvm_logtalk_pl.ps1 -h` for a list and
description of the script options.

See the script usage examples in the `../SCRIPT.txt` file.
