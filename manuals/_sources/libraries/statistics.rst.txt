.. _statistics:

``statistics``
==============

The entities in this group define some useful predicates for descriptive
statistics. Data is represented as a list of numbers (integers or
floats). Use the object ``sample`` of your data represents a sample. Use
the object ``population`` if your data represents a population.

API documentation
-----------------

Open the
`../../docs/library_index.html#statistics <../../docs/library_index.html#statistics>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(statistics(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(statistics(tester)).
