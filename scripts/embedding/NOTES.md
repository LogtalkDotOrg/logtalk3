________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


This directory contains sample scripts for selected backend Prolog
compilers for embedding Logtalk and Logtalk applications. See the
`SCRIPT.txt` file for usage examples.

These scripts allow pre-compilation of the Logtalk compiler/runtime
and optionally of a Logtalk application. This is a common requirement
for embedding, specially when deploying applications in read-only file
systems. These sample scripts should be regarded as starting points
as actual use often requires customization (e.g., the starting goal,
inclusion of a top-level interpreter, etc).

An alternative, available in some backend Prolog compilers such as
SICStus Prolog, SWI-Prolog, and YAP, is to create a *saved state*
after loading Logtalk and a Logtalk application (or the generated
Prolog code for Logtalk and a Logtalk application). Saved states
are usually executable files that embed both a Prolog runtime and
application code. See the `SCRIPT.txt` file for some examples.
For details on creating and using saved states, see your backend
Prolog compiler documentation.

Note that the provided scripts don't handle applications composed of
both Logtalk and Prolog source files. In these cases, either move
(if possible) the Prolog code into Logtalk source files or customize
the embedding scripts to include the contents of the Prolog files
into the generated embedding files.

Running the embedding scripts
-----------------------------

On POSIX systems, the scripts require the Bash shell.

On Windows, experimental PowerShell scripts are available for selected
backends. Using the PowerShell scripts may require first changing the
execution policy:

	PowerShell.exe -ExecutionPolicy Unrestricted

In alternative, a possible solution to run the Bash shell scripts is to
install either the [Windows Subsystem for Linux (WSL)](https://docs.microsoft.com/en-us/windows/wsl/)
or [Git for Windows](https://gitforwindows.org). Both provide access
to a Bash shell. In both cases, use full paths with forward slashes
(e.g., `C:/Users/jdoe/collect`) and without environment variables. You
must also specify the temporary directory for intermediate files using
the scripts `-t` option. Also, symbolic links may not work and use of
actual paths to files may be required.

Settings for embedded applications
----------------------------------

The scripts accept a command-line option for specifying a settings file.
See the `settings-embedding-sample.lgt` file for settings suggestions for
embedding applications. Notably, the `reload` flag should usually be set
to `skip` to prevent reloading of already loaded code when running the
embedded application or saved state. You may also want to turn off the
`source_data` flag to reduce the size of your application. When a settings
file is passed as argument to the embedding scripts, the backend adapter
file ia automatically patched, changing the value of the `settings_file`
flag to `deny`, to prevent using any settings file accessible on a computer
where the embedded application is run to disturb it.

It's also possible to not include any settings file by using the `-s none`
option. This is mostly useful when generating a new top-level executable
that only includes the Logtalk compiler/runtime.

Library paths for embedded applications
---------------------------------------

The scripts also accept a command-line option for specifying a paths file.
This paths file must provide definitions for the `logtalk_library_path/2`
predicate for all libraries loaded by the application being embedded. The
following query can be used to find the names of all the libraries used
by a loaded application:

	| ?- setof(Library, File^(logtalk::loaded_file_property(File, library(Library))), Libraries).

To help ensure that the embedded application is relocatable, load it and
verify that the following query fails:

	| ?- setof(Library, File^(logtalk::loaded_file_property(File, library(Library))), Libraries),
	     member(Missing, Libraries),
	     \+ logtalk_library_path(Missing, _).

Avoiding dependencies on the Logtalk environment variables
----------------------------------------------------------

To avoid dependencies on the Logtalk `LOGTALKHOME` and `LOGTALKUSER`
environment variables, the sample scripts support an option for
expanding library aliases paths in the paths and settings files. This
option also allows (in general) the safe inclusion of the application
loader files. If for some reason this option is not desirable, the
embedded application or saved state should be run in a process that
sets (possibly just for itself) the `LOGTALKHOME` and `LOGTALKUSER`
environment variables to the values used during the pre-compilation
of the Logtalk resources. For example, in a POSIX system, you can
use something like:

	$ LOGTALKHOME=/usr/local/share/logtalk LOGTALKUSER=/home/user/logtalk ./saved_state

Known issues
------------

The Bash version of the embedding scripts use GNU `sed` when available.
Notably on macOS, if you get `sed` errors when running the embedding
scripts, try installing GNU `sed` (using e.g. Homebrew or MacPorts).
