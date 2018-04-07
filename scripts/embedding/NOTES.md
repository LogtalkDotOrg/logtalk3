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


This directory contains scripts for selected backend Prolog compilers
for embedding Logtalk and Logtalk applications. Embedding usually
requires precompiling the Logtalk core files and the application source
files. The scripts should be regarded as starting points as actual use
requires customization (e.g. starting goal, inclusion of a top-level
interpreter, custom startup settings, etc). An alternative, available
in some backend Prolog compilers such as SICStus Prolog, SWI-Prolog,
and YAP is to create a *saved state* after loading Logtalk and a Logtalk
application. In both solutions, the `reload` flag should usually be set
to `skip` (in the used settings file) to prevent reloading of already
loaded code when running the embedded application and the embedded
application or saved state should be run in a process that sets (just
for itself) the `LOGTALKHOME` and `LOGTALKUSER` environment variables
to the values used during the pre-compilation of the Logtalk resources.

See the `settings-embedding-sample.lgt` for suggestions on defined a
settings file for embedding applications.
