``options``
===========

This library provides useful predicates for managing developer tool and
application options.

API documentation
-----------------

Open the
`../../docs/library_index.html#options <../../docs/library_index.html#options>`__
file in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(options(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(options(tester)).

Usage
-----

The ``options`` category is usually imported by the root object of the
developer tool or application. The importing object should define the
``default_option/1`` predicate and, if option type-checking is required,
the ``valid_option/1`` predicate. This library requires options to be
represented by compound terms but leaves otherwise to the clients the
actual representation.

The library also supports a user-defined ``fix_option/2`` predicate. An
usage example is when an option value can be a relative file path that
should be expanded before used. Another usage example would be
converting from a user-friendly option to a form more suitable for
internal processing. When a call to the ``fix_option/2`` predicate
fails, the option is used as-is.
