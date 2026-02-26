________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
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


`nanoid`
========

This library generates random NanoID identifiers:

https://github.com/ai/nanoid

By default, identifiers are represented as atoms with 21 symbols and use
the standard URL-safe alphabet:

```
_-0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ
```

Custom size, alphabet, and representation (atoms, lists of characters,
or lists of character codes) are supported using a parametric object.

The generation of random identifiers uses the `/dev/urandom` random number
generator when available. This includes macOS, Linux, *BSD, and other POSIX
operating systems. On Windows, a pseudo-random generator is used, randomized
using the current wall time.

See also the `ids`, `uuid`, and `ulid` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#nanoid](../../apis/library_index.html#nanoid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(nanoid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(nanoid(tester)).


Usage
-----

To generate an identifier using the default configuration:

	| ?- nanoid::generate(NanoID).
	NanoID = 'V1StGXR8_Z5jdHi6B-myT'
	yes

To generate a 10-symbol identifier represented as a list of characters:

	| ?- nanoid(chars, 10, 'abcdef012345')::generate(NanoID).
	NanoID = ['4','b','d','f','a','0','3','2','1','e']
	yes

To generate a 32-symbol identifier represented as a list of character codes:

	| ?- nanoid(codes, 32, [0'a,0'b,0'c,0'd,0'e,0'f,0'0,0'1,0'2,0'3])::generate(NanoID).
	NanoID = [49,97,99,100,48,102,98,101,50,51,48,101,99,102,49,50,98,100,49,97,48,51,101,98,100,99,49,102,48,97,50,100]
	yes
