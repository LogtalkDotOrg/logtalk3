.. _library_avro:

``avro``
========

The ``avro`` library implements predicates for reading (parsing) and
writing (generating) data in the Apache Avro binary format:

- https://avro.apache.org/docs/current/specification/

**This library requires a backend supporting unbounded integer
arithmetic.**

API documentation
-----------------

Open the
`../../apis/library_index.html#avro <../../apis/library_index.html#avro>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(avro(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(avro(tester)).

Schema representation
---------------------

Schema files use the ``.avsc`` extension and their contents are always
valid JSON values. They can be parsed using the
``json(curly,dash,atom)`` library object.

Data representation
-------------------

Data to be serialized to Avro binary format files can be represented in
the JSON Lines format (using the ``.jsonl`` extension) and parsed using
the ``json_lines(curly, dash, atom)`` library object.

Schema Examples
---------------

Primitive type schemas:

- ``null``
- ``boolean``
- ``int``
- ``long``
- ``float``
- ``double``
- ``bytes``
- ``string``

Array schema example represented as a JSON term:

::

   {type-array, items-int}

Encoding
--------

Encoding is accomplished using the ``generate/3`` or ``generate/4``
predicates. For example, assuming the schema is just ``int``:

::

   | ?- avro::generate(bytes(Bytes), int, 42).
   Bytes = [84]
   yes

Or an array of ``int``:

::

   | ?- avro::generate(bytes(Bytes), {type-array,items-int}, [42,37,13,17]).
   Bytes = [8,84,74,26,34,0]
   yes

To include the schema in the output (as an Avro Object Container File),
use the ``generate/4`` predicate with the second argument set to
``true``. For example:

::

   | ?- avro::generate(file('output.avro'), true, {type-array,items-int}, [42,37,13,17]).
   yes

Decoding
--------

Decoding is accomplished using the ``parse/2`` or ``parse/3``
predicates.

When parsing a file that includes a schema (Avro Object Container File),
use ``parse/2`` which returns a ``Schema-Data`` pair. For example:

::

   | ?- avro::parse(file('input.avro'), Schema-Data).

When the schema is not present in the file, ``Schema`` is unified with
``false``.

When parsing with a known schema, use instead the ``parse/3`` predicate.
For example:

::

   | ?- avro::parse(bytes([84]), int, Data).
   Data = 42
   yes
