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


This directory contains example scripts for creating GNU Prolog generated
executable files that embed Logtalk and an optional Logtalk application.

The following scripts are provided:

- `gprolog_embed_logtalk.sh`  
	Bash shell script for POSIX systems
- `gprolog_embed_logtalk.ps1`  
	PowerShell script for Windows systems

Both scrips create a `logtalk` executable that embeds the Logtalk
compiler/runtime and an optional application; starts the top-level
interpreter when run.

Usage
-----

Use `gprolog_embed_logtalk.sh -h` or `gprolog_embed_logtalk.ps1 -h` for a
list and description of the script options.

To define an application goal to be called automatically when running an
executable, try e.g.

	$ gprolog_embed_logtalk.sh ... -- --strip --no-top-level /full/path/to/init.pl

With the contents of the `init.pl` file being an `initialization/1`
directive calling the startup goal. For example:

	:- initialization('::'(app,start)).

To avoid syntax errors, you may need to use canonical notation in the argument
of the directive as exemplified.

See the script usage examples in the `../SCRIPT.txt` file.

Known issues
------------

The script generates some `suspicious predicate {}/1` warnings for the adapter
file, `gnu.pl`. These can be safely ignored.

A `gplc` limitation when compiling calls to multifile predicates requires
files that contain those calls but don't define clauses for the multifile
predicates to include the multifile predicate directives (or to meta-call
the multifile predicates). For example, if your code calls the predicate
`logtalk_library_path/2` but doesn't define clauses for it (in the same
file):

	:- if(current_logtalk_flag(prolog_dialect, gnu)).
		% workaround gplc limitation when dealing with multifile predicates
		% that are called from a file but not defined in that file
		:- multifile(logtalk_library_path/2).
	:- endif.

Be sure to read the GNU Prolog manual on `gplc`, specially the discussion
on how the calling order for initialization goals found in different files
is machine-dependent. This may dictate listing the files being compiled
by it in a specific order to avoid runtime predicate existence errors.
