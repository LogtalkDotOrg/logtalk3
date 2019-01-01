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


This directory contains example scripts for creating JIProlog `.jip` files
from Logtalk core files and Logtalk source files.

The following scripts are provided:

- `jiprolog_logtalk_jip.sh`  
	creates a `logtalk.jip` file with the Logtalk compiler and runtime
	and an optional `application.jip` file for an application

The `.jip` files can be loaded using the `load/1` JIProlog predicate. It
is also possible to create a JAR file from the `.jip` files. For example,
by creating a `init.pl` with the contents:

	:- load('logtalk.jip').
	:- load('application.jip').

And then typing:

	$ jar cf logtalk.jar init.pl *.jip

The `logtalk.jar` file could then be distributed with the other JIProlog
`.jar` files. Logtalk could also be loaded on demand from the `logtalk.jar`
file by using the `load_library/1` JIProlog built-in predicate:

	| ?- load_library('logtalk.jar').
