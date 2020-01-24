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


`hook_flows`
============

The `hook_pipeline` and `hook_set` parametric objects in this library
implement common scenarios of combining multiple hook objects for the
expansion of source files.


API documentation
-----------------

Open the [../../docs/library_index.html#hook_flows](../../docs/library_index.html#hook_flows)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(hook_flows(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(hook_flows(tester)).


Usage
-----

Pre-processing a source file with a hook object can be accomplished by either
compiling the source file using the option `hook(HookObject)` or by adding to
the top of the file the directive:

	:- set_logtalk_flag(hook, HookObject).

Note that `set_logtalk_flag/2` directives are local to a source file.

The `hook_pipeline(Pipeline)` is a parametric object where the parameter is a
list of hook objects, interpreted as a pre-processing pipeline: the results of
a hook object are passed to the next hook object.

The `hook_set(Set)` is a parametric object where the parameter is a list of
hook objects, interpreted as a set of hook object: hooks in the set are tried
until one of them succeeds.

Note that these two basic hook flows can be freely combined and also used as
examples of how to construct custom hook flows.
