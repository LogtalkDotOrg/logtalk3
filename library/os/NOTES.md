________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

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


`os`
====

This library entities define a *portable* operating-system interface for the
supported backend Prolog compilers. Some predicates may only be supported
by a subset of backend Prolog compilers on a subset of operating-systems.
They should be used with care and fully tested in your application domain
as some backend Prolog compilers have buggy and inconsistent interfaces,
specially across operating-systems.

The `os_types` category defines some useful operating-system types for
type-checking when using with the `type` library object.


API documentation
-----------------

Open the [../../docs/library_index.html#os](../../docs/library_index.html#os)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(os(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(os(tester)).


Known issues
------------

Some predicates are not supported by all backends. See the remarks section
in the `os` object documentation for details.
