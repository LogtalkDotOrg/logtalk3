.. _library_ids:

``ids``
=======

This library generates random identifiers (atoms) given the number of
bytes of randomness. The identifiers are Base64 encoded. By default, 20
bytes (160 bits) are used.

The generation of random identifiers uses the ``/dev/urandom`` random
number generator when available. This includes macOS, Linux, \*BSD, and
other POSIX operating-systems. On Windows, a pseudo-random generator is
used but randomized using the current wall time.

See also the ``uuid`` library.

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

To generate an identifier using the 240 bits (30 bytes) of randomness:

::

   | ?- ids(30)::generate(Identifier).
   Identifier = 'ie/jYcLsqo8ZguCOF1ZNPFDRvJ03Ww5Qa9e0FxRB'
   yes
