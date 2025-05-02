.. _library_intervals:

``intervals``
=============

This library provides an ``intervalp`` protocol and an ``interval``
object that implement basic temporal interval relations protocol (based
on the James F. Allen Interval Algebra work).

API documentation
-----------------

Open the
`../../apis/library_index.html#intervals <../../apis/library_index.html#intervals>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(intervals(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(intervals(tester)).
