.. _library_hierarchies:

``hierarchies``
===============

This library provides categories implementing reflection predicates over
class and prototype hierarchies. These categories can be imported by any
object that requires reasoning about hierarchies.

API documentation
-----------------

Open the
`../../apis/library_index.html#hierarchies <../../apis/library_index.html#hierarchies>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hierarchies(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hierarchies(tester)).
