.. _meta:

``meta``
========

This library provides implementations of common meta-predicates. The
``meta`` object implements common meta-predicates like ``map/3`` and
``fold_left/4``.

See also the ``meta_compiler`` library, which provides optimized
compilation of meta-predicate calls.

API documentation
-----------------

Open the
`../../docs/library_index.html#meta <../../docs/library_index.html#meta>`__
link in a web browser.

Loading
-------

To load the main entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(meta(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(meta(tester)).

Usage
-----

See e.g. the ``metapredicates`` example and unit tests.
