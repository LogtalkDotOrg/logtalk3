________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


`ulid`
======

This library implements a Universally Unique Lexicographically Sortable
Identifier (ULID) generator.

https://github.com/ulid/spec

Note that some backends provide time stamps with lower granularity than
required (i.e. seconds but not milliseconds).

The generation of ULIDs uses the `/dev/urandom` random number generator
when available. This includes macOS, Linux, *BSD, and other POSIX
operating-systems. On Windows, a pseudo-random generator is used but
randomized using the current wall time. 

ULIDs can be generated as atoms, lists of characters, or lists of
character codes.

See also the `ids` and `uuid` libraries.


API documentation
-----------------

Open the [../../docs/library_index.html#ulid](../../docs/library_index.html#ulid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(ulid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(ulid(tester)).


Generating ULIDs
----------------

By default, ULIDs are generated as atoms. For example:

	| ?- ulid::generate(ULID).
	ULID = '01H0J31SYQXHJZWPRAKHQ6YVYH'
	yes

To generate a ULID using a list of characters representation, use instead
the `ulid/1` parametric object:

	| ?- ulid(chars)::generate(ULID).
	ULID = ['0','1','H','0','J','3','2','Y','V','5','V','S','P','K','5','P','4','5','G','G','0','9','8','8','M','2']
	yes

Similar to get a ULID using a list of character codes representation:

	| ?- ulid(codes)::generate(ULID).
	ULID = [48,49,72,48,74,51,52,66,54,48,55,57,54,49,67,82,70,65,67,51,67,67,86,82,48,66]
	yes
