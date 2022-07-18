.. _library_os:

``os``
======

This library entities define a *portable* operating-system interface for
the supported backend Prolog compilers.

The ``os_types`` category defines some useful operating-system types for
type-checking when using with the ``type`` library object.

API documentation
-----------------

Open the
`../../docs/library_index.html#os <../../docs/library_index.html#os>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(os(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(os(tester)).

Known issues
------------

Some predicates may only be supported by a subset of backend Prolog
compilers on a subset of operating-systems. They should be used with
care and fully tested in your application domain as some backend Prolog
compilers have buggy and inconsistent interfaces, specially across
operating-systems. See the remarks section in the ``os`` object
documentation for details.
