________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>  
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


`types`
=======

This library implements predicates over standard Prolog term types and 
also terms representing common data structures such as lists and pairs.

It also includes a user-extensible `type` object defining type checking
predicates over common Logtalk and Prolog term types. The types define
a hierarchy with the Prolog type `term` at the root (i.e. type-checking
a predicate argument of type `term` trivially succeeds). Some types are
only meaningful for backend Prolog systems supporting non-universal
features (e.g. `cyclic` or `char(CharSet)` with a Unicode character set).
See the API documentation for a full list of the types defined by default.


API documentation
-----------------

Open the [../../docs/library_index.html#types](../../docs/library_index.html#types)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(types(loader)).

In case your code only requires the most basic types, you can load in
alternative the file:

	| ?- logtalk_load(basic_types(loader)).

See the notes on the `basic_types` virtual library for details.


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(types(tester)).


Type-checking
-------------

This library `type` object can be used to type-check common Logtalk and Prolog
term types (see the object documentation for a listing of all the pre-defined
types). The `valid/2` predicate succeeds or fails if a term is of a given type.
For example:

	| ?- type::valid(positive_integer, 42).
	yes

	| ?- type::valid(positive_integer, -13).
	no

The `check/2` and `check/3` predicates throw an exception if a term is not of
a given type. For example:

	| ?- catch(type::check(integer, abc), Error, true).
	Error = type_error(integer, abc)
	yes

If we require a standard `error/2` exception term, the `check/3` predicate
takes a *context* argument. For example:

	| ?- catch(type::check(integer, abc, foo/3), Error, true).
	Error = error(type_error(integer, abc), foo/3)
	yes

Typically, the context is provided by calling the built-in `context/1` method.


Defining new types
------------------

To define a custom type, define clauses for both the `type::type/1` and
`type::check/2` multifile predicates. For example:

	:- multifile(type::type/1).
	type::type(age).

	:- multifile(type::check/2).
	type::check(age, Term) :-
		type::check(between(non_negative_integer, 0, 150), Term).

Be careful to ensure that new type definitions don't introduce spurious
choice-points for these predicates. The unit tests of the `types` library
perform this check for ground types.

When defining a meta-type (i.e. a type with arguments that are also types),
add also a clause for the `type::meta_type/3` multifile predicate. For
example:

	:- multifile(type::meta_type/3).
	type::meta_type(tuple(Type1, Type2, Type3), [Type1, Type2, Type3], []).

This predicate is called when checking if a type is a defined type. For
meta-types, that check must extend to the sub-types.


Examples
--------

See e.g. the `os` library implementation of custom types for files and
directories. Or the `expecteds` and `optionals` libraries custom types.
See also the `my_types` programming example.
