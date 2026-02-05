.. _library_json_ld:

``json_ld``
===========

The ``json_ld`` library provides predicates for parsing, generating,
expanding, and compacting JSON-LD 1.1 (JSON-based Serialization for
Linked Data) documents based on the W3C Recommendation found at:

https://www.w3.org/TR/json-ld11/

This library builds on top of the ``json`` library for JSON parsing and
generation, and uses the same representation choices for JSON data. It
includes parametric objects whose parameters allow selecting the
representation for parsed JSON objects (``curly`` or ``list``), JSON
text strings (``atom``, ``chars``, or ``codes``) and JSON pairs
(``dash``, ``equal``, or ``colon``).

API documentation
-----------------

Open the
`../../apis/library_index.html#json_ld <../../apis/library_index.html#json_ld>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(json_ld(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(json_ld(tester)).

Parsing
-------

JSON-LD documents can be parsed from various sources using the
``parse/2`` predicate. Since JSON-LD is a superset of JSON, any valid
JSON-LD document is also valid JSON:

::

   | ?- json_ld::parse(file('document.jsonld'), Term).

   | ?- json_ld::parse(atom('{"@context": {"name": "http://schema.org/name"}, "name": "Manu Sporny"}'), Term).

Generating
----------

JSON-LD documents can be generated from Logtalk terms using the
``generate/2`` predicate:

::

   | ?- json_ld::generate(atom(Atom), {'@context'-{name-'http://schema.org/name'}, name-'Manu Sporny'}).

Expansion
---------

Expansion is the process of removing the context and representing all
properties and types as full IRIs. This is useful for processing JSON-LD
data in a context-independent way:

::

   | ?- json_ld::parse(atom('{"@context": {"name": "http://schema.org/name"}, "name": "Manu Sporny"}'), Doc),
        json_ld::expand(Doc, Expanded).
   Doc = {'@context'-{name-'http://schema.org/name'}, name-'Manu Sporny'},
   Expanded = [{'http://schema.org/name'-[{'@value'-'Manu Sporny'}]}]
   yes

Compaction
----------

Compaction is the process of applying a context to shorten IRIs to terms
or compact IRIs. This is the inverse of expansion:

::

   | ?- json_ld::expand({'@context'-{name-'http://schema.org/name'}, name-'Manu Sporny'}, Expanded),
        json_ld::compact(Expanded, {'@context'-{name-'http://schema.org/name'}}, Compacted).

Supported Features
------------------

The library supports the following JSON-LD 1.1 features:

**Context processing:**

- Inline context definitions (``@context``)
- Vocabulary mapping (``@vocab``)
- Base IRI (``@base``)
- Default language (``@language``)
- Base direction (``@direction``)
- Compact IRIs (prefix:suffix notation)
- Term definitions (simple and expanded)
- Type coercion (``@type`` in term definitions)
- Context arrays (multiple contexts)

**Node objects:**

- Node identifiers (``@id``)
- Node types (``@type``)
- Blank node identifiers (``_:name``)

**Value objects:**

- Typed values (``@value`` with ``@type``)
- Language-tagged strings (``@value`` with ``@language``)
- Direction-tagged strings (``@value`` with ``@direction``)

**Graph support:**

- Named graphs (``@graph``)
- Default graph

**Collections:**

- Ordered lists (``@list``)
- Unordered sets (``@set``)

**Other features:**

- Reverse properties (``@reverse``)
- Included blocks (``@included``)
- Index preservation (``@index``)

**Not currently supported:**

- Remote context fetching (contexts referenced by URL)
- Full JSON-LD API algorithms (flattening, framing)
- ``@import`` context processing

Representation
--------------

JSON-LD documents are represented using the same term conventions as the
``json`` library. The JSON-LD keywords (``@context``, ``@id``,
``@type``, etc.) are represented as atoms in the parsed terms. For
example:

+----------------------------------------------------+-----------------------------------------------------------+
| JSON-LD                                            | term (default)                                            |
+====================================================+===========================================================+
| {"@id":                                            | {'@id'-'`http://example.org/'} <http://example.org/'}>`__ |
| "`http://example.org/"} <http://example.org/"}>`__ |                                                           |
+----------------------------------------------------+-----------------------------------------------------------+
| {"@type": "Person"}                                | {'@type'-'Person'}                                        |
+----------------------------------------------------+-----------------------------------------------------------+
| {"@value": "hello", "@language": "en"}             | {'@value'-hello, '@language'-en}                          |
+----------------------------------------------------+-----------------------------------------------------------+
| {"@list": [1, 2, 3]}                               | {'@list'-[1, 2, 3]}                                       |
+----------------------------------------------------+-----------------------------------------------------------+
| {"@graph": [...]}                                  | {'@graph'-[...]}                                          |
+----------------------------------------------------+-----------------------------------------------------------+

As with the ``json`` library, objects can be represented using curly
terms (default) or list terms, and pairs can use dash, equal, or colon
notation.
