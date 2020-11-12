``git``
=======

This library provides access to a git project current branch and latest
commit data (e.g. commit hash).

API documentation
-----------------

Open the
`../../docs/library_index.html#git <../../docs/library_index.html#git>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(git(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(git(tester)).

