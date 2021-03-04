``cbor``
========

The ``cbor`` library implements predicates for importing and exporting
data in the Concise Binary Object Representation (CBOR) format:

-  https://www.rfc-editor.org/rfc/rfc8949.html
-  http://cbor.io/

This library is a work-in-progress. Currently it requires a backend
supporting unbounded integer arithmetic.

Representation
--------------

-  Maps are represented using curly-bracketed terms, ``{Pairs}``, where
   each pair uses the representation ``Key-Value``.

-  Arrays are represented using lists.

-  Byte strings uses ``bytes(List)`` compound terms.

-  Text strings can be represented as atoms, ``chars(List)``, or
   ``codes(List)``. The default when decoding is to use atoms when using
   the ``cbor`` object. To decode text strings into lists of chars or
   code, use the ``cbor/1`` with the parameter bound to ``chars`` or
   ``codes``. For example:

   ::

        | ?- cbor::parse([0x65,0x68,0x65,0x6c,0x6c,0x6f], Term).
        Term = hello
        yes

        | ?- cbor(atom)::parse([0x65,0x68,0x65,0x6c,0x6c,0x6f], Term).
        Term = hello
        yes

        | ?- cbor(chars)::parse([0x65,0x68,0x65,0x6c,0x6c,0x6f], Term).
        Term = chars([h,e,l,l,o])
        yes

        | ?- cbor(codes)::parse([0x65,0x68,0x65,0x6c,0x6c,0x6f], Term).
        Term = codes([104,101,108,108,111])
        yes

-  Tagged data uses ``tag(Tag, Data)`` compound terms.

-  Simple values can be represented using ``simple(Simple)`` compound
   terms.

-  The CBOR elements ``false``, ``true``, ``null``, and ``undefined``
   are represented by, respectively, the ``@false``, ``@true``,
   ``@null``, and ``@undefined`` compound terms.

-  The compound terms ``@infinity``, ``@negative_infinity``, and
   ``@not_a_number`` are used to represent the corresponding CBOR
   elements.

-  Only some backends distinguish between positive zero and negative
   zero. The compound terms ``@zero`` and ``@negative_zero`` can be used
   as an alternative for encoding. The decoder, however, produces the
   ``0.0`` and ``-0.0`` floats.

Encoding
--------

The encoding of arrays and maps uses indefinite-length encoding. All
floats are currently encoded using decimal fractions. Encoding
indicators and big floats are not currently supported.

API documentation
-----------------

Open the
`../../docs/library_index.html#cbor <../../docs/library_index.html#cbor>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(cbor(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(cbor(tester)).
