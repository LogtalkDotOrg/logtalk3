.. _library_base32:

``base32``
==========

The ``base32`` library provides predicates for encoding and decoding
data in the Base32 format as per the specification found at:

https://www.rfc-editor.org/rfc/rfc4648

API documentation
-----------------

Open the
`../../apis/library_index.html#base32 <../../apis/library_index.html#base32>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(base32(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(base32(tester)).

Encoding
--------

Encoding a list of bytes in Base32 format is accomplished by the
``base32::generate/2`` predicate. For example:

::

   | ?- atom_codes('Hello!', Bytes),
        base32::generate(atom(Base32), Bytes).
   Base32 = 'JBSWY3DPEE======'
   Bytes = [72,101,108,108,111,33]
   yes

   | ?- atom_codes('Hello!', Bytes),
        base32::generate(codes(Base32), Bytes).
   Base32 = [74,66,83,87,89,51,68,80,69,69,61,61,61,61,61,61]
   Bytes = [72,101,108,108,111,33]
   yes

The Base32 result can also be represented using a list of chars, written
to a file or to a stream. See the API documentation for details.

Decoding
--------

Decoding of Base32 data is accomplished using the ``base32::parse/2``
predicate. For example:

::

   | ?- base32::parse(atom('JBSWY3DPEE======'), Bytes),
        atom_codes(Atom, Bytes).
   Atom = 'Hello!'
   Bytes = [72,101,108,108,111,33]
   yes

   | ?- base32::parse(chars(['J','B','S','W','Y','3','D','P','E','E','=','=','=','=','=','=']), Bytes),
        atom_codes(Atom, Bytes).
   Atom = 'Hello!'
   Bytes = [72,101,108,108,111,33]
   yes

The ``base32::parse/2`` predicate accepts other input sources such as a
file or a stream. See the API documentation for details. Both uppercase
and lowercase letters are accepted when decoding.
