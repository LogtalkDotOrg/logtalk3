.. _term_io:

``term_io``
===========

This library implements predicates for reading/writing terms from/to
atoms, chars (lists of characters), and codes (lists of character
codes). These predicates are implemented using a single temporary file
created when the library is loaded. This temporary file is unique per
Logtalk process. The predicates can be safely used in multi-threaded
applications.

API documentation
-----------------

Open the
`../../docs/library_index.html#term-io <../../docs/library_index.html#term-io>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(term_io(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(term_io(tester)).
