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
Currently only version 1 and version 4 UUIDs are supported. For reference
material, see e.g.

https://en.wikipedia.org/wiki/Universally_unique_identifier

Some backends provide time stamps with low granularity (e.g. seconds but not
milliseconds or nanoseconds). To compensate, the generation of version 1 UUIDs
uses 14 random bits for the clock sequence.

The generation of version 4 UUIDs uses the `/dev/urandom` random number
generator when available. This includes macOS, Linux, *BSD, and other POSIX
operating-systems. On Windows, a pseudo-random generator is used but
randomized using the current wall time. 


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

By default, version 1 UUIDs are generated as atoms. For example:

	| ?- uuid::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
	UUID = '00a66fc0-82cf-11eb-bc83-f2d19094dc4b'
	yes
	
To generate a UUID using a list of characters representation, use instead the
`uuid/1` parametric object:

	| ?- uuid(chars)::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
	UUID = ['0','0',d,e,'9','0',c,'0',-,'8','2',c,f,-,'1','1',e,b,-,
	        a,'9','8','5',-,f,'2',d,'1','9','0','9','4',d,c,'4',b]
	yes

Similar to get a UUID using a list of character codes representation:

	| ?- uuid(codes)::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
	UUID = [48,48,52,99,99,54,99,48,45,56,50,99,102,45,49,49,101,98,45,
	        98,57,102,52,45,102,50,100,49,57,48,57,52,100,99,52,98]
	yes


Generating version 4 UUIDs
--------------------------

By default, version 4 UUIDs are generated as atoms. For example:

	| ?- uuid::uuid_v4(UUID).
	UUID = '1c652782-69c5-4252-88c8-09e576a44db5'
	yes

To generate a UUID using a list of characters representation, use instead the
`uuid/1` parametric object:

	| ?- uuid(chars)::uuid_v4(UUID).
	UUID = [d,'3',d,'3','3','5','1','3',-,'8','1',e,c,-,'4',d,'2','6',-,
	        '9',f,'2','2',-,e,d,'9','5',e,'0','0',e,'1','5','7','0']
	yes

Similar to get a UUID using a list of character codes representation:

	| ?- uuid(codes)::uuid_v4(UUID).
	UUID = [102,97,52,54,57,98,100,50,45,51,57,54,51,45,52,97,100,55,45,
	        98,50,50,55,45,101,100,52,99,56,55,99,54,53,55,102,98]
	yes


Generating the null UUID
------------------------

A predicate is also provided that returns the null UUID:

	| ?- uuid::uuid_null(UUID).
	UUID = '00000000-0000-0000-0000-000000000000'
	yes
