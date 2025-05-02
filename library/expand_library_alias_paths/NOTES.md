________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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


`expand_library_alias_paths`
============================

This library provides provides a hook object, `expand_library_alias_paths`,
for expanding library alias paths in `logtalk_library_path/2 facts` in source
files. It is mainly used when embedding Logtalk and Logtalk applications.


API documentation
-----------------

Open the [../../apis/library_index.html#expand-library-alias-paths](../../apis/library_index.html#expand-library-alias-paths)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` utility file:

	| ?- logtalk_load(expand_library_alias_paths(loader)).


Usage
-----

Use the `hook/1` option when compiling a source file:

	| ?- logtalk_load(my_source_file, [hook(expand_library_alias_paths)]).
	...

Alternatively, assuming it is the only hook object you are using, you can
set it as thew default hook object:


	| ?- set_logtalk_flag(hook, expand_library_alias_paths).
	...
