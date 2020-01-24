``basic_types``
===============

The ``basic_types`` library is a virtual library that loads only basic
types from the ``types`` library:

-  ``termp``, ``term``,
-  ``atomic``, ``atom``, ``number``, ``float``, ``integer``
   -``compound``, ``listp``, ``list``
-  ``type``.

API documentation
-----------------

Open the
`../../docs/library_index.html#types <../../docs/library_index.html#types>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(basic_types(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file for the
``types`` library:

::

   | ?- logtalk_load(types(tester)).

