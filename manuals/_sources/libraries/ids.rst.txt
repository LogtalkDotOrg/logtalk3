.. _library_ids:

``ids``
=======

This library generates random identifiers given the number of bytes of
randomness. The identifiers are Base64 encoded. By default, 20 bytes
(160 bits) are used.

The generation of random identifiers uses the ``/dev/urandom`` random
number generator when available. This includes macOS, Linux, \*BSD, and
other POSIX operating-systems. On Windows, a pseudo-random generator is
used but randomized using the current wall time.

Identifiers can be generated as atoms, lists of characters, or lists of
character codes.

See also the ``uuid`` and ``ulid`` libraries.

API documentation
-----------------

Open the
`../../docs/library_index.html#ids <../../docs/library_index.html#ids>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(ids(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(ids(tester)).

Usage
-----

To generate an identifier using the default 160 bits of randomness:

::

   | ?- ids::generate(Identifier).
   Identifier = '2gpMzqAFXBO5mYFIPX1qMkHxgGE='
   yes

To generate an identifier represented by an atom using 240 bits (30
bytes) of randomness:

::

   | ?- ids(atom, 30)::generate(Identifier).
   Identifier = 'ie/jYcLsqo8ZguCOF1ZNPFDRvJ03Ww5Qa9e0FxRB'
   yes

To generate an identifier represented by a list of characters using 64
bits (8 bytes) of randomness:

::

   | ?- ids(chars, 8)::generate(Identifier).
   Identifier = ['5','0','8','V',d,'S',c,y,n,o,'A',=]
   yes

To generate an identifier represented by a list of character codes using
64 bits (8 bytes) of randomness:

::

   | ?- ids(codes, 8)::generate(Identifier).
   Identifier = [111,81,86,55,99,79,70,77,65,74,103,61]
   yes
