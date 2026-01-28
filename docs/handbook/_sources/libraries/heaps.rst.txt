.. _library_heaps:

``heaps``
=========

This library defines a heap protocol and provides minimum and maximum
heaps using two different implementations: pairing heaps and binary
heaps.

For backward-compatibility, the ``legacy.lgt`` file defines the
``heapp`` protocol using the ``heap_protocol`` protocol plus the
``heap/1``, ``minheap``, and ``maxheap`` objects using the
``binary_heap/1`` objects. These legacy protocol and objects are
deprecated and should not be used in new code.

API documentation
-----------------

Open the
`../../apis/library_index.html#heaps <../../apis/library_index.html#heaps>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(heaps(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(heaps(tester)).

Credits
-------

Original binary heap code by Richard O'Keefe and adapted to Logtalk by
Paulo Moura and Victor Lagerkvist.
