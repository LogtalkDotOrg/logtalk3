.. _library_queues:

``queues``
==========

This library implements queues. The queue representation should be
regarded as an opaque term and only accessed using the library
predicates.

API documentation
-----------------

Open the
`../../docs/library_index.html#queues <../../docs/library_index.html#queues>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(queues(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(queues(tester)).

Usage
-----

To create a new queue, use the ``new/1`` predicate:

::

   | ?- queue::new(Queue).
   Queue = ...
   yes

Elements can be added to either the end of the queue or the front of the
queue using, respectively, the ``join/3`` and ``join_all/3`` predicates
or the ``jump/3`` and ``jump_all/3``. For example:

::

   | ?- queue::(new(Queue0), join_all([1,2,3], Queue0, Queue1)).
   Queue0 = ...,
   Queue1 = ...
   yes

We can query the head of the queue or remove the head of the queue
using, respectively, the ``head/2`` and ``serve/3`` predicates. For
example:

::

   | ?- queue::(new(Queue0), join(1, Queue0, Queue1), head(Queue1, Head)).
   Queue0 = ...,
   Queue1 = ...,
   Head = 1
   yes

For details on these and other provided predicates, consult the library
API documentation.
