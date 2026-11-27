________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains example scripts for creating Shumway Prolog
executables and WebAssembly bundles containing the Logtalk compiler/runtime
and optionally an application with an optional startup goal."

The following scripts are provided:

- `shumway_logtalk_pl.sh`  
	Bash shell script for POSIX systems
- `shumway_logtalk_pl.ps1`  
	PowerShell script for Windows systems

Both scripts create an intermediate `logtalk.pl` file with the Logtalk
compiler/runtime and optionally an intermediate `application.pl` file
for an application. By default, these intermediate files are deleted
(use the `-k` option to keep them). When creating an executable or
WebAssembly bundle for a Logtalk application, it's also possible to
specify a startup goal.

Usage
-----

Use `shumway_logtalk_pl.sh -h` or `shumway_logtalk_pl.ps1 -h` for a
list and description of the script options.

See the script usage examples in the `../SCRIPT.txt` file.
