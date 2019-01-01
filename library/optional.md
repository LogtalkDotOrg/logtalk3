________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

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

This library provides an implementation of *optional term references* with
an API modeled after the Java 8 `Optional` class (mainly due to this library
being requested by users working in Logtalk/Java hybrid applications). An
optional term reference is an opaque term that may or may not contain a
non-null term. Optional term references avoid forcing the user to define a
representation for a null term by providing an API with predicates that
depend on the presence or absence of an optional term. Optional term
references also allow separating the code that retrieves or constructs
optional terms from the code that processes them, which is then free to
deal if necessary and at its convenience with any case where the optional
terms are not present.


API documentation
-----------------

Open the [../docs/index.html](../docs/index.html) file in a web browser
and choose the library index.


Loading
-------

To load all entities in this library load the `optional_loader.lgt` loader
file:

	| ?- logtalk_load(library(optional_loader)).


Usage
-----

The `optional` object provides constructors for optional term references. For
example:

	| ?- optional::of(1, Ref).
	...

The created optional term references can then be passed as parameters to the
`optional/1` parametric object. For example:

	| ?- optional::of(1, Ref), optional(Ref)::or_else(Term, 0).
	Ref = the(1),
	Term = 1
	yes

	| ?- optional::empty(Ref), optional(Ref)::or_else(Term, 0).
	Ref = empty,
	Term = 0
	yes

The `maybe` object provides types and predicates for type-checking of the
optional term wrapped by optional term references. It also provides some 
predicates for handling lists of optional term references.
