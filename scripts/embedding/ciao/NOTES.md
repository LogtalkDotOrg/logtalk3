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


This directory contains example scripts for creating Ciao Prolog files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `ciao_logtalk_pl.sh`  
	Bash shell script for POSIX systems
- `ciao_logtalk_qlf.ps1`  
	PowerShell script for Windows systems

Both scripts create a `logtalk.pl` file with the Logtalk compiler and
runtime and an optional `application.pl` file for an application.

The `.pl` files can be loaded using the `-u` command-line option. For
example, `ciaosh --iso-strict -u logtalk.pl -u application.pl`.

Usage
-----

Use `ciao_logtalk_pl.sh -h` or  `ciao_logtalk_pl.ps1 -h` for a list and
description of the script options.

See the script usage examples in the `../SCRIPT.txt` file.
