________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2024s Paulo Moura <pmoura@logtalk.org>  
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


`tutor`
=======

This tool adds explanations and suggestions to selected warning and error
messages from the compiler/runtime and the developer tools. It's specially
useful for new users not yet familiar with the warning and error messages.


API documentation
-----------------

This tool API documentation is available at:

[../../docs/library_index.html#tutor](../../docs/library_index.html#tutor)


Loading
-------

This tool can be loaded using the query:

	| ?- logtalk_load(tutor(loader)).


Usage
-----

Simply load the tool at startup (e.g. from a settings file). As an example,
with this tool loaded, instead of terse compiler warnings such as:

	*     No matching clause for goal: baz(a)
	*       while compiling object main_include_compiler_warning
	*       in file logtalk/examples/errors/include_compiler_warning.lgt between lines 37-38
	*     
	*     Duplicated clause: b(one)
	*       first found at or above line 45
	*       while compiling object main_include_compiler_warning
	*       in file logtalk/examples/errors/include_compiler_warning.lgt at or above line 48

the user will get:

	*     No matching clause for goal: baz(a)
	*       while compiling object main_include_compiler_warning
	*       in file logtalk/examples/errors/include_compiler_warning.lgt between lines 37-38
	*     Calls to locally defined predicates without a clause with a matching head
	*     fail. Typo in a predicate argument? Predicate definition incomplete?
	*     
	*     Duplicated clause: b(one)
	*       first found at or above line 45
	*       while compiling object main_include_compiler_warning
	*       in file logtalk/examples/errors/include_compiler_warning.lgt at or above line 48
	*     Duplicated clauses are usually a source code editing error and can
	*     result in spurious choice-points, degrading performance. Delete or
	*     correct the duplicated clause to fix this warning.
