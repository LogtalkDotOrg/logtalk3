________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


`uuid`
======

This library implements a Universally unique identifier (UUID) generator.
Currently only version 1 and version 4 UUIDs are supported.


API documentation
-----------------

Open the [../../docs/library_index.html#uuid](../../docs/library_index.html#uuid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(uuid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(uuid(tester)).


Generating version 1 UUIDs
--------------------------




Generating version 4 UUIDs
--------------------------

By default, version 4 UUIDs are generated as atoms. For example:

	| ?- uuid::uuid_v4(UUID).
	UUID = '1c652782-69c5-4252-88c8-09e576a44db5'
	yes

To generate a UUID using a list of characters representation, use instead the
`uuid/1` parametric object:

	| ?- uuid(chars)::uuid_v4(UUID).
	UUID = [d,'3',d,'3','3','5','1','3',-,'8','1',e,c,-,'4',d,'2','6',-,'9',f,'2','2',-,e,d,'9','5',e,'0','0',e,'1','5','7','0']
	yes

Similar to get a UUID using a list of character codes representation:

	| ?- uuid(codes)::uuid_v4(UUID).
	UUID = [102,97,52,54,57,98,100,50,45,51,57,54,51,45,52,97,100,55,45,98,50,50,55,45,101,100,52,99,56,55,99,54,53,55,102,98]
	yes


Generating the null UUID
------------------------

A predicate is also provided that returns the null UUID:

	| ?- uuid::uuid_null(UUID).
	UUID = '00000000-0000-0000-0000-000000000000'
	yes
