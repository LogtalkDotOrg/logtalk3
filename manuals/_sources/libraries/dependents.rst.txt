``dependents``
==============

The ``observer`` and ``subject`` categories implement the Smalltalk
dependents handling mechanism. This mechanism can be used as an
alternative to Logtalk's system-wide support for event-driven
programming.

API documentation
-----------------

Open the
`../../docs/library_index.html#dependents <../../docs/library_index.html#dependents>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(dependents(loader)).

