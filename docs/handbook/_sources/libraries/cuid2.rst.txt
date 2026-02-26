.. _library_cuid2:

``cuid2``
=========

This library generates random Cuid2 identifiers:

https://github.com/paralleldrive/cuid2

By default, identifiers are represented as atoms with 24 symbols and use
a lowercase alphanumeric alphabet:

::

   abcdefghijklmnopqrstuvwxyz0123456789

Custom size, alphabet, and representation (atoms, lists of characters,
or lists of character codes) are supported using a parametric object.

The generation of random identifiers uses the ``/dev/urandom`` random
number generator when available. This includes macOS, Linux, \*BSD, and
other POSIX operating systems. On Windows, a pseudo-random generator is
used, randomized using the current wall time.

See also the ``ids``, ``nanoid``, ``ksuid``, ``uuid``, and ``ulid``
libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#cuid2 <../../apis/library_index.html#cuid2>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(cuid2(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(cuid2(tester)).

Usage
-----

To generate an identifier using the default configuration:

::

   | ?- cuid2::generate(Cuid2).
   Cuid2 = 'k4f9mdd51t2r9y53i8h4j1bz'
   yes

To generate a 10-symbol identifier represented as a list of characters:

::

   | ?- cuid2(chars, 10, 'abcdef012345')::generate(Cuid2).
   Cuid2 = ['a','2','f','e','5','c','1','d','0','b']
   yes
