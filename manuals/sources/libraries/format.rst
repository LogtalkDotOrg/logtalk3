``format``
==========

The ``format`` object provides a portable abstraction over how the de
facto standard ``format/2-3`` predicates are made available by the
supported backend Prolog systems. Some system provide these predicates
as built-in predicates while others make them available using a library
that must be explicitly loaded. Note that some Prolog systems provide
only a subset of the expected format specifiers. Notably, table related
format specifiers are only fully supported by a few systems. For wide
portability, use atoms for the format specifier argument.

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on for most of the backends. When that's the
case, there is no overhead compared with calling the abstracted
predicates directly.

API documentation
-----------------

Open the
`../../docs/library_index.html#format <../../docs/library_index.html#format>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(format(loader)).

Testing
-------

Minimal tests for this library predicates can be run by loading the
``tester.lgt`` file:

::

   | ?- logtalk_load(format(tester)).

Detailed tests for the ``format/2-3`` predicates are available in the
``tests/prolog/predicates`` directory as part of the Prolog standards
conformance test suite. Use those tests to confirm the portability of
the format specifiers that you want to use.
