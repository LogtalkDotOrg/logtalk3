________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains example scripts for creating SICStus Prolog `.po`
files plus saved states from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `sicstus_logtalk_po.sh`  
	Bash shell script for POSIX systems
- `sicstus_logtalk_po.ps1`  
	PowerShell script for Windows systems

Both scripts create a `logtalk.po` file with the Logtalk compiler and
runtime and an optional `application.po` file for an application.

The `.po` files can be loaded using the `load_files/1-2` predicates. The
saved states can be loaded using `sicstus -r SavedStateFile`.

Usage
-----

Use `sicstus_logtalk_po.sh -h` or `sicstus_logtalk_po.ps1 -h` for a list
and description of the script options.

See the script usage examples in the `../SCRIPT.txt` file on how to
create a SICStus Prolog saved state that includes a Logtalk application.
