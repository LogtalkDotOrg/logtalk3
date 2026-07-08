.. _library_json_ld:

``json_ld``
===========

The ``json_ld`` library provides predicates for parsing, generating,
expanding, compacting, flattening, and framing JSON-LD 1.1 (JSON-based
Serialization for Linked Data) documents based on the W3C Recommendation
found at:

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
or compact IRIs. The current implementation provides basic key and value
compaction but does not yet implement the full JSON-LD 1.1 inverse
context selection algorithm:

::

   | ?- json_ld::expand({'@context'-{name-'http://schema.org/name'}, name-'Manu Sporny'}, Expanded),
        json_ld::compact(Expanded, {'@context'-{name-'http://schema.org/name'}}, Compacted).

Flattening
----------

Flattening uses node map generation to collect node objects into an
expanded flat array, with nested node objects replaced by references.
Blank node identifiers are generated for nodes that don't have an
``@id`` and existing blank node identifiers are relabeled consistently:

::

   | ?- json_ld::parse(atom('{"@context":{"knows":"http://xmlns.com/foaf/0.1/knows"},"@id":"http://example.org/john","knows":{"@id":"http://example.org/jane"}}'), Doc),
        json_ld::expand(Doc, Expanded),
        json_ld::flatten(Expanded, Flattened).

Framing
-------

Framing selects and shapes nodes from a JSON-LD document using a frame.
The ``frame/3`` predicate expands the input document and frame,
generates a node map, matches nodes by ``@id``, ``@type``, and framed
properties, and embeds referenced nodes according to the framing
options. When the frame includes a context, the framed result is
compacted using that context:

\| ?- json_ld::frame(
{'@context'-{name-'`http://schema.org/name'} <http://schema.org/name'}>`__,
'@id'-'http://example.org/alice', name-'Alice'},
{'@context'-{name-'`http://schema.org/name'} <http://schema.org/name'}>`__,
'@id'-'`http://example.org/alice'} <http://example.org/alice'}>`__,
Framed).

Implemented Features
--------------------

The library currently implements the following JSON-LD 1.1 processing
features:

**Context processing:**

- Inline context definitions (``@context``)
- Vocabulary mapping (``@vocab``)
- Base IRI (``@base``)
- Relative IRI resolution against ``@base``, including common path,
  query, and fragment references
- Default language (``@language``) for string values
- Validation of base direction (``@direction``) values
- Compact IRIs (prefix:suffix notation)
- Term definitions with simple IRI mappings and expanded ``@id``
  mappings
- Type coercion for ``@id``, ``@vocab``, and typed literals in term
  definitions
- Context arrays (multiple contexts)
- Validation of ``@version``, ``@base``, ``@vocab``, ``@language``,
  ``@direction``, ``@propagate``, and ``@protected`` context values

**Node objects:**

- Node identifiers (``@id``)
- Node types (``@type``)
- Blank node identifiers (``_:name``)

**Value objects:**

- String, numeric, and boolean property values expanded as value objects
- Typed values (``@value`` with ``@type``)
- Language-tagged strings (``@value`` with ``@language``)
- Direction-tagged value objects (``@value`` with ``@direction``)
- Ordinary ``null`` property values are dropped during expansion

**Graph support:**

- Named graphs (``@graph``)
- Default graph

**Collections:**

- Ordered lists (``@list``)
- Unordered sets (``@set``)
- ``@list`` rejects nested arrays during expansion

**Other features:**

- Reverse properties (``@reverse``)
- Included blocks (``@included``)
- Index preservation (``@index``)
- Nested property blocks (``@nest``)
- Basic compaction (``compact/3``)
- Node map based flattening (``flatten/2``), including named graphs,
  reverse properties, included blocks, lists, duplicate suppression,
  type merging, conflicting index detection, and deterministic blank
  node relabeling
- Framing (``frame/3``) with ``@id`` and ``@type`` matching, nested
  frames, ``@embed``, ``@explicit``, ``@requireAll``, ``@omitDefault``,
  ``@default``, ``@reverse``, ``@list``, cycle prevention, and
  context-based compaction of framed output

Known Limitations
-----------------

The implementation is intentionally portable and currently incomplete
with respect to full JSON-LD 1.1 API conformance. Known limitations
include:

**Context processing:**

- Remote contexts referenced by URL are not fetched.
- ``@import`` is rejected with a
  ``domain_error(json_ld_context_import, Value)`` error; imported
  contexts are not loaded or merged.
- ``@protected`` and ``@propagate`` values are validated but their full
  JSON-LD scoping and term redefinition semantics are not yet
  implemented.
- ``@container``, ``@prefix``, scoped contexts, reverse properties in
  term definitions, index mappings, language maps, and graph containers
  are not yet implemented.
- ``@direction`` is preserved in explicit value objects and validated in
  contexts, but default direction is not yet applied to string values
  during expansion.
- ``@type: @json`` and ``@type: @none`` coercion are not yet
  implemented.

**Expansion:**

- Expansion implements common node, value, list, set, reverse, included,
  index, and nest cases but does not yet implement all validation rules
  from the JSON-LD 1.1 expansion algorithm.
- Container expansion for ``@language``, ``@index``, ``@id``, ``@type``,
  ``@graph``, ``@list``, and ``@set`` containers is not yet implemented.
- Reverse property validation is partial.
- Absolute IRI detection and compact IRI processing are sufficient for
  common ``://``, ``urn:``, and prefix:suffix cases but are not a
  complete RFC 3986/3987 implementation.

**Compaction:**

- ``compact/3`` performs basic key and value compaction but does not yet
  build or use the JSON-LD inverse context.
- Term selection does not yet account for container, type, language,
  direction, reverse, or index preferences.
- The result is a compacted term tree and does not yet implement the
  full JSON-LD compact API output shape, including context injection and
  all compact arrays rules.

**Flattening:**

- ``flatten/2`` returns expanded flattened output and does not implement
  a separate compacted flattening API variant with context injection.
- The node map implementation covers common node, named graph, reverse,
  included, list, duplicate suppression, type merging, and blank node
  relabeling cases, but some edge-case validation from the full JSON-LD
  1.1 algorithms is still partial.

**Framing:**

- ``frame/3`` implements portable JSON-LD framing for common expanded
  and compacted inputs, including nested embedding, reverse properties,
  lists, defaults, and cycle prevention.
- Full JSON-LD 1.1 framing edge-case semantics are not yet implemented,
  including all ``@embed`` modes beyond boolean behavior, all default
  object shapes, graph map framing details, ``@included`` framing
  details, and all validation/error cases from the Recommendation.

**Not currently supported:**

- Remote context fetching (contexts referenced by URL)
- Full JSON-LD 1.1 compaction algorithm
- RDF conversion, normalization, or dataset processing

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
