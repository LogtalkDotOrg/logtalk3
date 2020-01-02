________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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


This directory contains example scripts for creating ECLiPSe `.eco` files
from Logtalk core files and Logtalk source files. Note that loading these
`.eco` files requires starting ECLiPSe with the command-line options
`-L iso -t user`.

The following scripts are provided:

- `eclipse_logtalk_eco.sh`  
	creates a `logtalk.eco` file with the Logtalk compiler and runtime
	and an optional `application.eco` file for an application
	
The `.eco` files can be loaded using the `-f` command-line option. For
example, `eclipse -L iso -t user -f logtalk.eco -f application.eco`.

Usage
-----

Use `eclipse_logtalk_eco.sh -h` for a list and description of the script
options.
