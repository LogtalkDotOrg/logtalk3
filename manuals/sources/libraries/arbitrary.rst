.. _library_arbitrary:

``arbitrary``
=============

The ``arbitrary`` library defines an ``arbitrary`` category providing
predicates for generating random values for selected types to the
``type`` object, complementing its type checking predicates. Both the
object and the category predicates can be extended by the user with
definitions for new types by defining clauses for multifile predicates.
This library is notably used in the QuickCheck implementation by the
``lgtunit`` tool. See also the documentation of the ``mutations``
library for related functionality.

API documentation
-----------------

Open the
`../../docs/library_index.html#arbitrary <../../docs/library_index.html#arbitrary>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(arbitrary(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(arbitrary(tester)).

Pre-defined types
-----------------

This library defines random generators for most common Logtalk and
Prolog types. See the `API
documentation <../../docs/library_index.html#arbitrary>`__ for a listing
of all the pre-defined types.

Usage
-----

The ``arbitrary`` category complements the ``type`` object and thus its
predicates are accessed via this object. For example:

::

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = -816
   yes

Defining new generators and shrinkers
-------------------------------------

To define a generator of arbitrary values for a type, define a clause
for the ``arbitrary::arbitrary/1`` multifile predicate specifying the
type and a clause for the ``arbitrary::arbitrary/2`` multifile predicate
generating an arbitrary term of the specified type. As a simple example,
assume that we want to define an "odd integer type". We start by
defining both the type checker and the arbitrary generator:

::

   :- multifile(type::type/1).
   type::type(odd).

   :- multifile(type::check/2).
   type::check(odd, Term) :-
       (   var(Term) ->
           throw(instantiation_error)
       ;   integer(Term),
           Term mod 2 =:= 1 ->
           true
       ;   throw(type_error(odd, Term))
       ).

   :- multifile(arbitrary::arbitrary/1).
   arbitrary::arbitrary(odd).

   :- multifile(arbitrary::arbitrary/2).
   arbitrary::arbitrary(odd, Arbitrary) :-
       type::arbitrary(integer, Arbitrary0),
       (   Arbitrary0 mod 2 =:= 1 ->
           Arbitrary = Arbitrary0
       ;   Arbitrary is Arbitrary0 + 1
       ).

We can also define a clause for the ``arbitrary::shrinker/1`` multifile
predicate to declare a new shrinker and a ``arbitrary::shrink/3``
multifile predicate for shrinking arbitrary values for QuickCheck usage:

::

   :- multifile(arbitrary::shrinker/1).
   arbitrary::shrinker(odd).

   :- multifile(arbitrary::shrink/3).
   arbitrary::shrink(odd, Large, Small) :-
       integer(Large),
       (   Large < -1 ->
           Small is Large + 2
       ;   Large > 1,
           Small is Large - 2
       ).

Definitions for the ``shrink/3`` predicate should either succeed or fail
but never throw an exception. The ``shrink_sequence/3`` predicate can be
used to help testing that shrinking a value results in a finite sequence
of values.

It is also possible to define edge cases for a given type for use with
QuickCheck implementations. For example:

::

   :- multifile(arbitrary::edge_case/2).
   arbitrary::edge_case(odd,  1).
   arbitrary::edge_case(odd, -1).

Edge cases are tried before resorting to generating arbitrary values for
a type.

A more complex example is generating arbitrary values for a recursive
type. A simple example of a recursive type is a binary tree. Assuming
that we are working with a binary tree holding integers where each node
is represented by a ``node(Left, Right)`` compound term, we can define a
``node(Depth)`` type where ``Depth`` is the maximum depth of the tree.
This argument allows us to prevent excessively deep trees:

::

   :- category(binary_tree).

       :- multifile(type::type/1).
       type::type(node(_)).

       :- multifile(type::check/2).
       type::check(node(_), Term) :-
           (   check(Term) ->
               true
           ;   var(Term) ->
               throw(instantiation_error)
           ;   throw(type_error(node(_), Term))
           ).

       check(Term) :-
           (   integer(Term) ->
               true
           ;   compound(Term),
               Term = node(Left, Right),
               check(Left),
               check(Right)
           ).

       :- multifile(arbitrary::arbitrary/1).
       arbitrary::arbitrary(node(_)).

       :- multifile(arbitrary::arbitrary/2).
       arbitrary::arbitrary(node(Depth), Arbitrary) :-
       (   Depth > 1 ->
           NewDepth is Depth - 1,
           type::arbitrary(
               types_frequency([
                   integer - 1,
                   compound(
                       node,
                       [
                           types([node(NewDepth), integer]),
                           types([node(NewDepth), integer])
                       ]
                   ) - 3
               ]),
               Arbitrary
           )
       ;   type::arbitrary(
       integer, Arbitrary)
       ).

   :- end_category.

In this second example, we use some of the pre-defined types provided by
the library. The ``types_frequency(Pairs)`` type supports generating
random terms for a type in the ``Type-Frequency`` pairs list where the
type ie randomly chosen after the types relative frequency. The
``compound(Name, Types)`` type supports generating compound term with a
given name and random arguments after the given types:

::

   | ?- type::arbitrary(node(4), Arbitrary).
   Arbitrary = 907
   yes

   | ?- type::arbitrary(node(4), Arbitrary).
   Arbitrary = node(node(node(522, 509), node(83, 453)), node(454, -197))
   yes

   | ?- type::arbitrary(node(4), Arbitrary).
   Arbitrary = node(node(-875, -866), -254)
   yes

   | ?- type::arbitrary(node(4), Arbitrary).
   Arbitrary = node(-133, -831)
   yes

The source code of these examples can be found in the ``test_files``
directory. Other examples of arbitrary term generators can be found in
the implementation of the ``optionals`` and ``expecteds`` libraries.

Scoped generators and shrinkers
-------------------------------

Declaring a new generator and possibly a shrinker for a custom type
rises the possibility of a conflict with third-party defined generators
and shrinkers. An alternative is to use the ``(::)/2`` meta-type to
define scoped generators and shrinkers. For example:

::

   :- object(scoped).

       % the same predicate is used for both generating and validating
       :- public(custom/1).
       custom(Term) :-
           (   var(Term) ->
               % assume predicate used as a generator
               random::random(Term)
           ;   % assume predicate used as a validator
               float(Term)
           ).

       % a predicate with the same name is used for shrinking
       :- public(custom/2).
       custom(Larger, Small) :-
           Small is Larger / 2.

   :- end_object.

Some sample calls:

::

   | ?- type::arbitrary(scoped::custom, Arbitrary).
   Arbitrary = 0.5788130906607927
   yes

   | ?- type::valid(scoped::custom, foo).
   no

   | ?- type::check(scoped::custom, _).
   ERROR: type_error(instantiation_error)

   | ?- type::check(scoped::custom, foo).
   ERROR: type_error(scoped::custom, foo)

   | ?- type::shrink(scoped::custom, 0.42, Smaller).
   Smaller = 0.21
   yes

The source code of this example can be found in the ``test_files``
directory.

Reproducing sequences of arbitrary terms
----------------------------------------

The ``arbitrary`` category provides access to the pseudo-random
generator it uses via the ``get_seed/1`` and ``set_seed/1``. This allows
sequences of arbitrary values to be reproduced. For example:

::

   | ?- type::get_seed(Seed).
   Seed = seed(3172, 9814, 20125)
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = -816
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = -113
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = 446

   | ?- type::set_seed(seed(3172, 9814, 20125)).
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = -816
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = -113
   yes

   | ?- type::arbitrary(integer, Arbitrary).
   Arbitrary = 446
   yes

The seed should be regarded as an opaque term and handled using the
``get_seed/1`` and ``set_seed/1`` predicates. These predicates are
notably used in the QuickCheck implementation provided by the
``lgtunit`` tool.

Default size of generated terms
-------------------------------

The library uses the value 42 for the default size of generated terms
for types where size is meaningful and implicit. To override this
default value, define a clause for the ``arbitrary::max_size/1``
multifile predicate. The new default size must be a positive integer.
For example:

::

   :- multifile(arbitrary::max_size/1).
   arbitrary::max_size(7).

When multiple definitions exist, the first valid one found is used. When
no definition is valid, the default value of 42 is used.

Known issues
------------

Some Prolog systems either don't support the null character or provide
buggy results when calling ``char_code/2`` with a code of zero. When
that's the case, the null character is excluded when generating
arbitrary characters or character codes.

Generating arbitrary Unicode characters (instead of Unicode codepoints)
is inherently problematic as the process first generates codepoints and
then tries to use the standard ``char_code/2`` to convert them to
characters. But, depending on the backend Prolog system and its internal
(if any) Unicode normalization, it may not be possible to convert a
codepoint to a single character.
