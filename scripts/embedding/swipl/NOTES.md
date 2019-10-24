________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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


This directory contains example scripts for creating SWI-Prolog QLF files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `swipl_logtalk_qlf.sh`  
	creates a `logtalk.qlf` file with the Logtalk compiler and runtime
	and an optional `application.qlf` file for an application

See the script usage examples in the `../SCRIPT.txt` file on how to
create a SWI-Prolog saved state that includes a Logtalk application.

Usage
-----

Use `swipl_logtalk_qlf.sh -h` for a list and description of the script
options.
