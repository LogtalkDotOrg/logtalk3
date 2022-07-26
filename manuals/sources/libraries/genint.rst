.. _library_genint:

``genint``
==========

The ``genint`` library implements predicates for generating positive
integers in increasing order. The public predicates are declared
synchronized to prevent race conditions when using a backend Prolog
compiler with multi-threading support.

API documentation
-----------------

Open the
`../../docs/library_index.html#genint <../../docs/library_index.html#genint>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(genint(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(genint(tester)).

Usage
-----

The ``genint_core`` category implements the library predicates. This
category is imported by the default ``genint`` object to provide
application global named counters. To make the counters local and thus
minimize the potential for counter name clashes, the category can be
imported by one of more application objects. Use protected or private
import to restrict the scope of the library predicates.
