.. _library_ksuid:

``ksuid``
=========

This library generates random KSUID identifiers:

https://github.com/segmentio/ksuid

**This library requires a backend supporting unbounded integer
arithmetic.**

By default, identifiers are represented as atoms and encoded using the
canonical Base62 alphabet:

::

   0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz

The generation of random identifiers uses the ``/dev/urandom`` random
number generator when available. This includes macOS, Linux, \*BSD, and
other POSIX operating systems. On Windows, a pseudo-random generator is
used, randomized using the current wall time.

Identifiers can be generated as atoms, lists of characters, or lists of
character codes.

See also the ``cuid2``, ``ids``, ``nanoid``, ``snowflakeid``, ``uuid``,
and ``ulid`` libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#ksuid <../../apis/library_index.html#ksuid>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ksuid(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ksuid(tester)).

Usage
-----

To generate a KSUID using the default configuration:

::

   | ?- ksuid::generate(KSUID).
   KSUID = '2YBXxVf8R5A1x6Yx5Y1AL7bEmel'
   yes

To generate a KSUID represented as a list of characters:

::

   | ?- ksuid(chars, '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')::generate(KSUID).
   KSUID = ['2','Y','B','X','x','V','f','8','R','5','A','1','x','6','Y','x','5','Y','1','A','L','7','b','E','m','e','l']
   yes
