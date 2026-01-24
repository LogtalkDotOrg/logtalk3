.. _library_base58:

``base58``
==========

The ``base58`` library provides predicates for encoding and decoding
data in the Base58 format using the Bitcoin alphabet variant. Base58 is
commonly used in Bitcoin addresses and other cryptocurrency
applications.

The Bitcoin alphabet excludes visually ambiguous characters:

- ``0`` (zero), ``O`` (uppercase o)
- ``I`` (uppercase i), ``l`` (lowercase L)

Alphabet: ``123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz``

This library requires a backend supporting unbounded integer arithmetic.

API documentation
-----------------

Open the
`../../apis/library_index.html#base58 <../../apis/library_index.html#base58>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(base58(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(base58(tester)).

Encoding
--------

Encoding a list of bytes in Base58 format is accomplished by the
``base58::generate/2`` predicate. For example:

::

   | ?- atom_codes('Hello World', Bytes),
        base58::generate(atom(Base58), Bytes).
   Base58 = 'JxF12TrwUP45BMd'
   Bytes = [72,101,108,108,111,32,87,111,114,108,100]
   yes

Leading zero bytes are preserved and encoded as '1' characters:

::

   | ?- base58::generate(atom(Base58), [0, 0, 0, 1, 2, 3]).
   Base58 = '111Ldp'
   yes

Decoding
--------

Decoding of Base58 data is accomplished using the ``base58::parse/2``
predicate. For example:

::

   | ?- base58::parse(atom('JxF12TrwUP45BMd'), Bytes),
        atom_codes(Atom, Bytes).
   Atom = 'Hello World'
   Bytes = [72,101,108,108,111,32,87,111,114,108,100]
   yes
