``optionals``
=============

This library provides an implementation of *optional terms* with an API
modeled after the Java 8 ``Optional`` class (originally due to requests
by users working in Logtalk/Java hybrid applications). An optional term
is an opaque compound term that may or may not hold a value. Optional
terms avoid forcing the user to define a representation for the absence
of a value by providing an API with predicates that depend on the
presence or absence of a value. Optional terms also allow separating the
code that constructs optional terms from the code that processes them,
which is then free to deal if necessary and at its convenience with any
case where the values hold by optional terms are not present.

API documentation
-----------------

Open the
`../../docs/library_index.html#optionals <../../docs/library_index.html#optionals>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(optionals(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(optionals(tester)).

Usage
-----

The ``optional`` object provides constructors for optional terms. For
example:

::

   | ?- optional::of(1, Optional).
   ...

The created optional terms can then be passed as parameters to the
``optional/1`` parametric object. For example:

::

   | ?- optional::of(1, Optional), optional(Optional)::or_else(Term, 0).
   Optional = optional(1),
   Term = 1
   yes

   | ?- optional::empty(Optional), optional(Optional)::or_else(Term, 0).
   Optional = empty,
   Term = 0
   yes

The ``maybe`` object provides types and predicates for type-checking of
the term hold by optional terms. It also provides some predicates for
handling lists of optional terms.

See also
--------

The ``expecteds`` library.
