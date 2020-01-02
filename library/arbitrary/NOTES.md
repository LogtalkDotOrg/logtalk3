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


About
-----

The `arbitrary` category adds predicates for generating random values
for selected types to the `type` object, complementing its type checking
predicates. Both the object and the category predicates can be extended
by the user with definitions for new types by defining clauses for
multifile predicates. This library is notably used in the QuickCheck
implementation by the `lgtunit` tool.


API documentation
-----------------

Open the [../docs/index.html](../docs/index.html) file in a web browser
and choose the library index.


Loading
-------

The `arbitrary_loader.lgt` file loads the `type` object and the `arbitrary`
category:

	| ?- logtalk_load(library(arbitrary_loader)).


Usage
-----

To define a generator of arbitrary values for a type, define a clause for the
`type::arbitrary/1` multifile predicate specifying the type and a clause for
the `type::arbitrary/2` multifile predicate generating an arbitrary term of
the specified type. For example:

    :- multifile(type::arbitrary/1).
    type::arbitrary(foo).

    :- multifile(type::arbitrary/2).
    type::arbitrary(foo, Arbitrary) :-
        ...

Optionally, define a clause for the `type::shrink/3` multifile predicate
for shrinking arbitrary values for QuickCheck usage. For example:

    :- multifile(type::shrink/3).
    type::shrink(foo, Large, Small) :-
        ...


Examples
--------

See the implementation of the `optional` and `expected`  libraries.


Known issues
------------

Some Prolog systems either don't support the null character or provide buggy
results when calling `char_code/2` with a code of zero. When that's the case,
the null character is excluded when generating arbitrary characters or
character codes.
