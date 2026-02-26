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


`snowflakeid`
=============

This library generates Snowflake-style identifiers using a generic generator
object and predefined profile objects.

**This library requires a backend supporting unbounded integer arithmetic.**

See also the `cuid2`, `ids`, `nanoid`, `ksuid`, `uuid`, and `ulid` libraries.


API documentation
-----------------

Open the [../../apis/library_index.html#snowflakeid](../../apis/library_index.html#snowflakeid)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(snowflakeid(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(snowflakeid(tester)).


Usage
-----

The generic generator is the parametric object:

	snowflakeid(Representation, EpochMilliseconds, TimeUnitMilliseconds, TimestampBits, NodeBits, SequenceBits, Node)

where `Representation` can be `integer`, `atom`, `chars`, or `codes`.

Predefined profiles are provided as objects extending the generic object:

- `snowflakeid_twitter(_Representation_)`
- `snowflakeid_sonyflake(_Representation_)`
- `snowflakeid_instagram(_Representation_)`

The default object `snowflakeid` uses the Twitter-style profile with atom
representation.


To generate a Snowflake ID using the default Twitter-style profile:

	| ?- snowflakeid::generate(ID).
	ID = '1917401915609202688'
	yes

To generate a Twitter-style Snowflake ID represented as an integer:

	| ?- snowflakeid_twitter(integer)::generate(ID).
	ID = 1917401915609202688
	yes

To generate a Sonyflake-style Snowflake ID represented as chars:

	| ?- snowflakeid_sonyflake(chars)::generate(ID).
	ID = ['9','2','8','5','6','7','2','8','0','1','4','8','9','5','5','7','5','9']
	yes

To define a custom profile:

	| ?- snowflakeid(atom, 1704067200000, 1, 41, 10, 12, 7)::generate(ID).
	ID = '28842191400263680'
	yes
