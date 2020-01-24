``types``
=========

This library implements predicates over standard Prolog term types and
also term representing common data structures such as lists and pairs.

It also includes a user-extensible ``type`` object defining type
checking predicates over common Logtalk and Prolog term types.

API documentation
-----------------

Open the
`../../docs/library_index.html#types <../../docs/library_index.html#types>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(types(loader)).

In case your code only requires the most basic types, you can load in
alternative the file:

::

   | ?- logtalk_load(basic_types(loader)).

See the notes on the ``basic_types`` virtual library for details.

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(types(tester)).

