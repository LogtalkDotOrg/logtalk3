________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

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
for embedding. These sample scripts should be regarded as starting
points as actual use requires customization (e.g. the starting goal,
inclusion of a top-level interpreter, etc). The scripts also accept
command-lines options for specifying a paths files and a settings
file. See the `settings-embedding-sample.lgt` file for settings
suggestions for embedding applications.

An alternative, available in some backend Prolog compilers such as
SICStus Prolog, SWI-Prolog, and YAP, is to create a *saved state*
after loading Logtalk and a Logtalk application (or the generated
Prolog code for Logtalk and a Logtalk application). Saved states
are usually executables that embed both a Prolog runtime and
application code. See the `SCRIPT.txt` file for some examples.
For details on creating and using saved states, see your backend
Prolog compiler documentation.

In both solutions, the `reload` flag should usually be set to `skip`
(in the used settings file) to prevent reloading of already loaded
code when running the embedded application or saved state.

To avoid dependencies on the Logtalk `LOGTALKHOME` and `LOGTALKUSER`
environment variables, the sample scripts allow expansion of library
aliases paths in the paths and settings files. If for some reason
this option is not desirable, the embedded application or saved state
should be run in a process that sets (possibly just for itself) the
`LOGTALKHOME` and `LOGTALKUSER` environment variables to the values
used during the pre-compilation of the Logtalk resources. For example,
in a POSIX system, you can use something like:

	$ LOGTALKHOME=/usr/local/share/logtalk LOGTALKUSER=/home/user/logtalk ./saved_state
