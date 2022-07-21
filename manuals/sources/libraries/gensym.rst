.. _library_gensym:

``gensym``
==========

The ``gensym`` library implements predicates for generating unique
atoms. The public predicates are declared synchronized to prevent race
conditions when using a backend Prolog compiler with multi-threading
support.

API documentation
-----------------

Open the
`../../docs/library_index.html#gensym <../../docs/library_index.html#gensym>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(gensym(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(gensym(tester)).

Usage
-----

The ``gensym_core`` category implements the library predicates. This
category is imported by the default ``gensym`` object to provide
application global generators. To make the generators local and thus
minimize the potential for generator name clashes, the category can be
imported by one of more application objects. Use protected or private
import to restrict the scope of the library predicates.
