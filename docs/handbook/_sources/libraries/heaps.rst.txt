.. _library_heaps:

``heaps``
=========

This library defines a heap protocol and provides minimum and maximum
heaps using two different implementations: pairing heaps and binary
heaps.

The heap representations should be regarded as opaque terms, subjected
to be changed without notice, and only accessed using the library
predicates.

For backward-compatibility, the ``legacy.lgt`` file defines the
``heapp`` protocol extending the ``heap_protocol`` protocol plus the
``heap/1``, ``minheap``, and ``maxheap`` objects extending the
``binary_heap/1`` object. These legacy protocol and objects are
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

Usage
-----

Choose the heap implementation that best suits your needs. The pairing
heap implementation is usually the best choice, specially if insertion
and merging operations dominate in your use case. You can easily
benchmark and compare the two implementations using either a ``uses/2``
directive with a parameter variable for the heap object and calling the
heap predicates using implicit message-sending goals or a ``uses/1``
directive defining an object alias for the heap object and using the
alias with explicit message-sending goals.

The two main library objects are ``binary_heap(Order)`` and
``pairing_heap(Order)``, where ``Order`` is either ``<`` for a minimum
heap or ``>`` for a maximum heap. For convenience, there are also
``binary_heap_min`` and ``pairing_heap_min`` objects for minimum heaps
and ``binary_heap_max`` and ``pairing_heap_max`` objects for maximum
heaps.

Credits
-------

Original binary heap code by Richard O'Keefe and adapted to Logtalk by
Paulo Moura and Victor Lagerkvist.
