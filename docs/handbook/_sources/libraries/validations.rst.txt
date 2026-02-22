.. _library_validations:

``validations``
===============

This library provides an implementation of *validation terms* with an
API for applicative-style error accumulation. A validation term is an
opaque compound term that either contains a valid value
(``valid(Value)``) or a list of errors (``invalid(Errors)``).

The library follows the same design pattern as the ``optionals`` and
``expecteds`` libraries:

- ``validation`` — factory object for constructing validation terms
- ``validation/1`` — parametric object wrapping a validation term
- ``validated`` — companion helper object for list operations,
  type-checking, and QuickCheck support

The naming follows the convention used in Scala Cats and Kotlin Arrow,
where the companion type is called ``Validated``.

API documentation
-----------------

Open the
`../../apis/library_index.html#validations <../../apis/library_index.html#validations>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(validations(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(validations(tester)).

Usage
-----

The ``validation`` object provides constructors for validation terms.
For example:

::

   | ?- validation::of_valid(1, Validation).
   ...

The created validation terms can then be passed as parameters to the
``validation/1`` parametric object. For example:

::

   | ?- validation::of_valid(1, V1), validation::of_valid(2, V2),
        validation::sequence([V1, V2], Validation).
   Validation = valid([1,2])
   yes

   | ?- validation::of_invalid(e1, V1), validation::of_invalid(e2, V2),
        validation::sequence([V1, V2], Validation).
   Validation = invalid([e1,e2])
   yes

   | ?- validation(valid(1))::flat_map([X,V]>>(Y is X+1, V = valid(Y)), Validation).
   Validation = valid(2)
   yes

Validation terms can be constructed from goals, optionals, and
expecteds:

::

   | ?- validation::from_goal(succ(1, Value), Value, Validation).
   Validation = valid(2)
   yes

   | ?- validation::from_optional(empty, no_value, Validation).
   Validation = invalid([no_value])
   yes

   | ?- validation::from_expected(unexpected(e), Validation).
   Validation = invalid([e])
   yes

Validation terms can be converted to optionals and expecteds:

::

   | ?- validation(valid(42))::to_optional(Optional).
   Optional = optional(42)
   yes

   | ?- validation(invalid([e1,e2]))::to_expected(Expected).
   Expected = unexpected([e1,e2])
   yes

The key feature of validation is error accumulation via ``zip/3``:

::

   | ?- validation::of_valid(1, V1), validation::of_invalid(e1, V2),
        validation(V1)::zip([_,_,_]>>true, V2, Result).
   Result = invalid([e1])
   yes

   | ?- validation::of_invalid(e1, V1), validation::of_invalid(e2, V2),
        validation(V1)::zip([_,_,_]>>true, V2, Result).
   Result = invalid([e1,e2])
   yes

The companion ``validated`` object provides predicates for handling
lists of validation terms, including ``valids/2``, ``invalids/2``,
``partition/3``, and ``map/3-4``.

::

   | ?- validation::of_valid(1, V1), validation::of_invalids([e1,e2], V2), validation::of_valid(2, V3),
        validated::partition([V1, V2, V3], Values, Errors).
   Values = [1,2],
   Errors = [e1,e2]
   yes

   | ?- validated::map([Term,Validation]>>(
          integer(Term) -> validation::of_valid(Term, Validation)
        ; validation::of_invalid(not_integer(Term), Validation)
        ), [1,a,2,b], ValuesErrors).
   ValuesErrors = [1,2]-[not_integer(a),not_integer(b)]
   yes

   | ?- validated::map([Term,Validation]>>(
          integer(Term) -> validation::of_valid(Term, Validation)
        ; validation::of_invalid(not_integer(Term), Validation)
        ), [1,a,2,b], Values, Errors).
   Values = [1,2],
   Errors = [not_integer(a),not_integer(b)]
   yes

Comparison with the ``expecteds`` library
-----------------------------------------

Both this library and the ``expecteds`` library wrap computations that
may succeed or fail. The fundamental difference is the **error model**:

- **``expecteds``** carries a *single* error and **short-circuits** on
  the first failure (monadic error handling). Once an unexpected term is
  produced, subsequent operations are skipped.

- **``validations``** carries a *list* of errors and **accumulates all
  failures** (applicative error handling). Operations such as ``zip/3``,
  ``sequence/2``, and ``traverse/3`` collect every error.

Use ``expecteds`` when you only care about the first error. Use
``validations`` when you want to **report all problems at once** (e.g.
form validation, configuration checking, or batch input processing).

Example: form validation with error accumulation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Consider validating a form with a name (must be an atom) and an age
(must be a positive integer). Each field is validated independently and
the results are combined with ``validated::sequence/2``, which
accumulates all errors:

::

   | ?- Name = 123, Age = young,
        validation::from_goal(atom(Name), Name, invalid_name, V1),
        validation::from_goal((integer(Age), Age > 0), Age, invalid_age, V2),
        validated::sequence([V1, V2], Result),
        validation(Result)::or_else_throw(_).
   uncaught exception: [invalid_name,invalid_age]

Both errors are reported. When all fields are valid, ``or_else_throw/1``
returns the list of valid values:

::

   | ?- Name = john, Age = 25,
        validation::from_goal(atom(Name), Name, invalid_name, V1),
        validation::from_goal((integer(Age), Age > 0), Age, invalid_age, V2),
        validated::sequence([V1, V2], Result),
        validation(Result)::or_else_throw(Values).
   Values = [john,25]
   yes

Compare with the ``expecteds`` library, where ``either::sequence/2``
short-circuits at the first failure:

::

   | ?- Name = 123, Age = young,
        expected::from_goal(atom(Name), Name, invalid_name, E1),
        expected::from_goal((integer(Age), Age > 0), Age, invalid_age, E2),
        either::sequence([E1, E2], Result),
        expected(Result)::or_else_throw(_).
   uncaught exception: invalid_name

Only ``invalid_name`` is reported — ``either::sequence/2`` stops at the
first unexpected term.

The two libraries are complementary. Use ``expecteds`` for sequential
pipelines where only the first error matters. Use ``validations`` with
``sequence/2``, ``traverse/3``, or ``zip/3`` for independent validations
where all errors should be collected.

See also
--------

The ``expecteds`` and ``optionals`` libraries.
