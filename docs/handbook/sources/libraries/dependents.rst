.. _library_dependents:

``dependents``
==============

The ``observer`` and ``subject`` categories implement the Smalltalk
dependents handling mechanism. This mechanism can be used as an
alternative to Logtalk system-wide support for event-driven programming.

API documentation
-----------------

Open the
`../../apis/library_index.html#dependents <../../apis/library_index.html#dependents>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(dependents(loader)).
