.. _library_mutations:

``mutations``
=============

Experimental library. Should not be used in production code. Details can
be changed without advance notice.

The ``mutations`` library provides support for generating random
mutations of selected types. The library defines default mutation
algorithms for the following basic types:

-  ``atom``
-  ``integer``
-  ``float``
-  ``compound``
-  ``list``

The user can add additional mutation algorithms for these or other types
by defining objects or categories providing clauses for the
``mutation/3`` predicate and expanding the entity source files using the
``mutations_store`` object as the hook object.

This library is expected to eventually be used to support mutation-based
*fuzz testing*.

By default, loading this library loads a set of default mutation
algorithms. These can be overriden by defining alternative mutations and
a custom loader file.

API documentation
-----------------

Open the
`../../docs/library_index.html#mutations <../../docs/library_index.html#mutations>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(mutations(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(mutations(tester)).

Usage
-----

The ``mutations`` category complements the ``type`` object and thus its
predicates are accessed via this object. For example:

::

   | ?- type::mutation(integer, 123, M).
   M = 1293
   yes

   | type::mutation(integer, 123, M).
   M = 5123
   yes

   | type::mutation(integer, 123, M).
   M = -123
   yes

   | type::mutation(integer, 123, M).
   M = 23
   yes

When there are multiple mutation algorithms for a given type, the
predicate ``type::mutation/3`` choses one of them randomly. We can query
the number of mutation algorithms available per type using the
``mutations_store::counter/2`` predicate:

::

   | ?- mutations_store::counter(Type, Count).
   Type = atom,
   Count = 6 ;
   Type = integer,
   Count = 7 ;
   ...

Loading this library also loads the ``arbitrary`` library, which
provides ``get_seed/1`` and ``set_seed/1`` predicates that can be used
to control the pseudo-random number generator.
