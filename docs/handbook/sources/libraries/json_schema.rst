.. _library_json_schema:

``json_schema``
===============

The ``json_schema`` library provides predicates for parsing JSON Schema
documents and validating JSON data against schemas. It is based on the
JSON Schema specification found at:

- https://json-schema.org/

This library builds on top of the ``json`` library for JSON parsing and
uses the same representation choices for JSON data.

API documentation
-----------------

Open the
`../../apis/library_index.html#json_schema <../../apis/library_index.html#json_schema>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(json_schema(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(json_schema(tester)).

Schema Parsing
--------------

Schemas can be parsed from various sources using the ``parse/2``
predicate:

::

   | ?- json_schema::parse(file('schema.json'), Schema).

   | ?- json_schema::parse(atom('{"type": "string"}'), Schema).

   | ?- json_schema::parse(codes(Codes), Schema).

Validation
----------

JSON data can be validated against a schema using the ``validate/2`` or
``validate/3`` predicates:

::

   % Success/failure validation
   | ?- json_schema::parse(atom('{"type": "string"}'), Schema),
        json_schema::validate(Schema, hello).
   yes

   | ?- json_schema::parse(atom('{"type": "string"}'), Schema),
        json_schema::validate(Schema, 42).
   no

   % Validation with error collection
   | ?- json_schema::parse(atom('{"type": "string"}'), Schema),
        json_schema::validate(Schema, 42, Errors).
   Errors = [error([], expected_type(string))]

Supported Keywords
------------------

The library supports the following JSON Schema keywords:

**Type validation:**

- ``type`` (string, number, integer, boolean, array, object, null)
- Multiple types: ``{"type": ["string", "number"]}``

**Generic validation:**

- ``enum`` - value must be one of the specified values
- ``const`` - value must be exactly the specified value

**String validation:**

- ``minLength`` - minimum string length
- ``maxLength`` - maximum string length

**Numeric validation:**

- ``minimum`` - minimum value (inclusive)
- ``maximum`` - maximum value (inclusive)
- ``exclusiveMinimum`` - minimum value (exclusive)
- ``exclusiveMaximum`` - maximum value (exclusive)
- ``multipleOf`` - value must be a multiple of this number

**Array validation:**

- ``items`` - schema for array items
- ``prefixItems`` - schemas for array items by position
- ``minItems`` - minimum number of items
- ``maxItems`` - maximum number of items
- ``uniqueItems`` - all items must be unique
- ``contains`` - at least one item must match the schema

**Object validation:**

- ``properties`` - schemas for object properties
- ``required`` - list of required property names
- ``minProperties`` - minimum number of properties
- ``maxProperties`` - maximum number of properties
- ``propertyNames`` - schema for property names

**Composition:**

- ``allOf`` - value must match all schemas
- ``anyOf`` - value must match at least one schema
- ``oneOf`` - value must match exactly one schema
- ``not`` - value must not match the schema

**Conditional:**

- ``if``/``then``/``else`` - conditional schema application

**Schema References:**

- ``$defs``/``definitions`` - schema definitions
- ``$ref`` - local JSON Pointer references (e.g., ``#/$defs/name``)

**Format validation:**

The ``format`` keyword validates string formats. Non-string values pass
format validation. Unknown formats are ignored per JSON Schema
specification. Supported formats:

- ``email`` - basic email format (local@domain)
- ``date`` - ISO 8601 date (YYYY-MM-DD)
- ``time`` - ISO 8601 time (HH:MM:SS) with optional fractional seconds
  and timezone
- ``date-time`` - ISO 8601 combined date and time
- ``uri`` - URI with scheme (e.g., ``http://example.com``)
- ``uri-reference`` - URI or relative reference
- ``ipv4`` - IPv4 address (dotted decimal notation)
- ``ipv6`` - IPv6 address (hex groups with colons, supports ::
  compression)
- ``uuid`` - UUID format (8-4-4-4-12 hex pattern)

**Boolean schemas:**

- ``true`` - accepts any value
- ``false`` - rejects all values

Known Limitations
-----------------

The following JSON Schema features are not yet supported:

- ``pattern`` - regular expression validation (requires regex support)
- ``patternProperties`` - property matching by pattern
- ``format`` - formats not listed above (e.g., ``hostname``, ``regex``,
  ``json-pointer``)
- ``$ref`` with remote URLs - external schema references require HTTP
  support
