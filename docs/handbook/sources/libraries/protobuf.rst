.. _library_protobuf:

``protobuf``
============

The ``protobuf`` library implements predicates for reading (parsing) and
writing (generating) data in the Google Protocol Buffers binary format.

This implementation is based on the Protocol Buffers Language Guide
(proto3) and the Protocol Buffers Encoding specification:

- https://protobuf.dev/programming-guides/proto3/
- https://protobuf.dev/programming-guides/encoding/

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

Protocol Buffers Overview
-------------------------

Protocol Buffers (protobuf) is a language-neutral, platform-neutral
extensible mechanism for serializing structured data. It is widely used
for data storage, communication protocols, and more.

Schema Representation
---------------------

In this library, Protocol Buffers schemas (normally defined in
``.proto`` files) are represented as Logtalk terms. The schema
representation uses a curly-bracketed syntax similar to JSON
representation.

A message schema is represented as:

::

   {message-MessageName, fields-FieldList}

Where ``FieldList`` is a list of field definitions, each with:

::

   {number-FieldNumber, name-FieldName, type-FieldType}

Supported Types
~~~~~~~~~~~~~~~

Scalar Value Types
^^^^^^^^^^^^^^^^^^

- ``int32`` - Uses variable-length encoding. Inefficient for negative
  numbers (use ``sint32`` instead).
- ``int64`` - Uses variable-length encoding. Inefficient for negative
  numbers (use ``sint64`` instead).
- ``uint32`` - Uses variable-length encoding.
- ``uint64`` - Uses variable-length encoding.
- ``sint32`` - Uses variable-length encoding with ZigZag encoding. More
  efficient for negative numbers.
- ``sint64`` - Uses variable-length encoding with ZigZag encoding. More
  efficient for negative numbers.
- ``bool`` - Boolean value: ``true`` or ``false``.
- ``fixed32`` - Always four bytes. More efficient than ``uint32`` if
  values are often greater than 2^28.
- ``fixed64`` - Always eight bytes. More efficient than ``uint64`` if
  values are often greater than 2^56.
- ``sfixed32`` - Always four bytes. Signed fixed-width integer.
- ``sfixed64`` - Always eight bytes. Signed fixed-width integer.
- ``float`` - 32-bit IEEE 754 floating point.
- ``double`` - 64-bit IEEE 754 floating point.
- ``string`` - UTF-8 encoded text.
- ``bytes`` - Arbitrary byte sequence.

Complex Types
^^^^^^^^^^^^^

- ``{message-Name, fields-Fields}`` - Embedded message (nested
  structure).

Schema Examples
~~~~~~~~~~~~~~~

Simple primitive type schemas:

- ``int32``
- ``string``
- ``bool``

Message schema example:

::

   {message-'Person', fields-[
       {number-1, name-name, type-string},
       {number-2, name-id, type-int32},
       {number-3, name-email, type-string}
   ]}

Nested message schema:

::

   {message-'AddressBook', fields-[
       {number-1, name-people, type-{message-'Person', fields-[
           {number-1, name-name, type-string},
           {number-2, name-id, type-int32}
       ]}}
   ]}

Data Representation
-------------------

Data to be serialized is represented as a list of field name-value pairs
using the ``-`` operator. For example:

::

   [name-'John Doe', id-42, email-'john@example.com']

For primitive types, data is represented directly as Logtalk values:

- Integers: ``42``, ``-17``
- Booleans: ``true``, ``false``
- Floats: ``3.14``, ``-273.15``
- Strings: ``'Hello World'``, ``hello``
- Bytes: ``[0x48, 0x65, 0x6c, 0x6c, 0x6f]``

Encoding
--------

Encoding is accomplished using the ``generate/3`` or ``generate/4``
predicates.

Simple Value Encoding
~~~~~~~~~~~~~~~~~~~~~

For example, encoding an integer using the ``int32`` schema:

::

   | ?- protobuf::generate(bytes(Bytes), int32, 150).
   Bytes = [150, 1]
   yes

Encoding a string:

::

   | ?- protobuf::generate(bytes(Bytes), string, 'testing').
   Bytes = [116, 101, 115, 116, 105, 110, 103]
   yes

Message Encoding
~~~~~~~~~~~~~~~~

To encode a complete message:

::

   | ?- Schema = {message-'Person', fields-[
   |        {number-1, name-name, type-string},
   |        {number-2, name-id, type-int32}
   |    ]},
   |    Data = [name-'John', id-42],
   |    protobuf::generate(bytes(Bytes), Schema, Data).

   Schema = {...},
   Data = [name-'John', id-42],
   Bytes = [10, 4, 74, 111, 104, 110, 16, 84]
   yes

Including Schema in Output
~~~~~~~~~~~~~~~~~~~~~~~~~~

To include the schema in the output (as a custom wrapper message), use
the ``generate/4`` predicate with the second argument set to ``true``:

::

   | ?- protobuf::generate(file('output.pb'), true, Schema, Data).
   yes

This generates a wrapper message with:

- Field 1: schema (as a JSON string)
- Field 2: data (as encoded bytes)

Decoding
--------

Decoding is accomplished using the ``parse/2`` or ``parse/3``
predicates.

Parsing with Schema Embedded
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When parsing a file that includes a schema (wrapper message format), use
``parse/2`` which returns a ``Schema-Data`` pair:

::

   | ?- protobuf::parse(file('input.pb'), Schema-Data).

When the schema is not present in the file, ``Schema`` is unified with
``false``.

Parsing with Known Schema
~~~~~~~~~~~~~~~~~~~~~~~~~

When the schema is known and not embedded in the file, use ``parse/3``:

::

   | ?- Schema = {message-'Person', fields-[
   |        {number-1, name-name, type-string},
   |        {number-2, name-id, type-int32}
   |    ]},
   |    protobuf::parse(file('person.pb'), Schema, Data).

   Schema = {...},
   Data = [name-'Alice', id-123]
   yes

Parsing from Different Sources
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The library supports three types of sources:

- ``bytes(List)`` - Parse from a list of bytes
- ``stream(Stream)`` - Parse from an open binary stream
- ``file(Path)`` - Parse from a file

Wire Format Details
-------------------

Protocol Buffers uses an efficient binary wire format with the following
wire types:

- **0 (Varint)**: ``int32``, ``int64``, ``uint32``, ``uint64``,
  ``sint32``, ``sint64``, ``bool``
- **1 (64-bit)**: ``fixed64``, ``sfixed64``, ``double``
- **2 (Length-delimited)**: ``string``, ``bytes``, embedded messages
- **5 (32-bit)**: ``fixed32``, ``sfixed32``, ``float``

Each field is encoded with a tag (field number and wire type) followed
by the value.

Binary Format Compatibility
---------------------------

The binary format produced by this library is compatible with:

- Official Protocol Buffers implementations (C++, Java, Python, Go,
  etc.)
- Other third-party implementations that follow the protobuf
  specification

Note: This library implements the core binary encoding format. Advanced
features like ``oneof``, ``map``, ``enum``, and ``repeated`` fields are
not yet supported in this initial version.

Examples and Test Files
-----------------------

The ``test_files`` subdirectory contains example schemas and data files
that demonstrate the library's capabilities:

.. _personproto:

person.proto
~~~~~~~~~~~~

A simple Person message schema. This is a classic Protocol Buffers
example demonstrating basic field types:

- **Source**: Adapted from the official Protocol Buffers tutorial
- **URL**: https://protobuf.dev/getting-started/
- **License**: Apache License 2.0 (compatible with Protocol Buffers
  documentation)

.. _addressbookproto:

addressbook.proto
~~~~~~~~~~~~~~~~~

An AddressBook schema demonstrating nested messages. Shows how to
represent complex hierarchical data structures:

- **Source**: Adapted from the official Protocol Buffers tutorial
- **URL**: https://protobuf.dev/getting-started/
- **License**: Apache License 2.0

Performance Considerations
~~~~~~~~~~~~~~~~~~~~~~~~~~

Protocol Buffers binary format is designed for:

- **Compactness**: Variable-length encoding for integers
- **Speed**: Simple encoding/decoding rules
- **Forward/backward compatibility**: Unknown fields can be skipped

For optimal performance:

- Use ``sint32``/``sint64`` for negative numbers
- Use ``fixed32``/``fixed64`` for large numbers
- Keep field numbers low (1-15 use only 1 byte for tags)

Further Reading
---------------

- Protocol Buffers Language Guide:
  https://protobuf.dev/programming-guides/proto3/
- Protocol Buffers Encoding Guide:
  https://protobuf.dev/programming-guides/encoding/
- Protocol Buffers Tutorials: https://protobuf.dev/getting-started/
