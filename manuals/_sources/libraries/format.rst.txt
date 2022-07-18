.. _library_format:

``format``
==========

The ``format`` object provides a portable abstraction over how the de
facto standard ``format/2-3`` predicates are made available by the
supported backend Prolog systems. Some system provide these predicates
as built-in predicates while others make them available using a library
that must be explicitly loaded.

Calls to the library predicates are inlined when compiled with the
``optimize`` flag turned on for most of the backends. When that's the
case, there is no overhead compared with calling the abstracted
predicates directly.

This library provides linter checks for calls to the ``format/2-3``
predicates. Given the differences between implementation of these
predicates among Prolog systems, the linter checks focus on detecting
common errors such as missing arguments and too many arguments. The
linter warnings are printed when the ``suspicious_calls`` flag is set to
``warning`` (its usual default).

Portability
-----------

Some Prolog systems provide only a subset of the expected format
specifiers. Notably, table related format specifiers are only fully
supported by a few systems. See the section below on testing.

Only some of the supported Prolog backends provide implementations of
the ``format/2-3`` predicates that allow using not only an atom or a
list of character codes for the format string (as de facto standard) but
also using a list of characters. These currently include ECLiPSe, LVM,
SICStus Prolog, SWI-Prolog, Trealla Prolog, and YAP. Therefore, when
wide portability is sought, atoms must be used for the format specifier
argument. Some systems, like Scryer Prolog and Tau Prolog, only accept a
list of characters for the format string. In this case, this library
will convert the atom format string before calling these systems native
implementations.

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
