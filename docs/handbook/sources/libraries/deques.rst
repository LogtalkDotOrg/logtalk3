.. _library_deques:

``deques``
==========

This library implements double-ended queues, deques. The queue
representation should be regarded as an opaque term and only accessed
using this library predicates.

API documentation
-----------------

Open the
`../../apis/library_index.html#deques <../../apis/library_index.html#deques>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(deques(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(deques(tester)).

Usage
-----

To create a new deque, use the ``new/1`` predicate:

::

   | ?- deque::new(Deque).
   Deque = ...
   yes

Elements can be added and removed both at the front and at the back of
the deque using the ``push_front/3``, ``push_back/3``, ``pop_front/3``,
and ``pop_back/3``, predicates. For example:

::

   | ?- deque::(as_deque([1,2,3], Deque0), push_front(0, Deque0, Deque)).
   Deque0 = ...,
   Deque = ...
   yes

We can also peek at the front and back of the deque using the
``peek_front/2`` and ``peek_back/2`` predicates. For example:

::

   | ?- deque::(as_deque([1,2,3], Deque), peek_back(Deque, Element)).
   Deque = ...,
   Element = 3
   yes

Mapping a closure over all elements of a deque can be done using the
``map/2`` and ``map/3`` predicates. For example:

::

   | ?- deque::(as_deque([1,2,3], Deque), map(write, Deque)).
   123
   Deque = ...
   yes

For details on these and other provided predicates, consult the library
API documentation.
