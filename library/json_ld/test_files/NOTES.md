________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


Test Files
==========

The JSON-LD test files in this directory are used to test the `json_ld`
library parsing, generating, expanding, and compacting functionality.


Origins
-------

The test files are either synthetic examples or based on patterns from
the following freely available sources:

- **W3C JSON-LD 1.1 Specification** (https://www.w3.org/TR/json-ld11/)
  Most test files are derived from examples in the W3C Recommendation
  for JSON-LD 1.1 (16 July 2020), which is published under the
  W3C Document License (https://www.w3.org/Consortium/Legal/2015/doc-license).

- **Schema.org** (https://schema.org/)
  Some test files use Schema.org vocabulary for realistic examples.
  Schema.org vocabulary is published under the Creative Commons
  Attribution-ShareAlike License (version 3.0).

- **JSON-LD Playground** (https://json-ld.org/playground/)
  Patterns from the JSON-LD Playground examples were used as inspiration
  for some test files.

All test files have been created specifically for this test suite and
are not direct copies of any copyrighted material.


Files
-----

- `simple_person.jsonld` - basic person with inline @context and simple properties (W3C spec Example 1 pattern)
- `context_vocab.jsonld` - using @vocab for default vocabulary mapping
- `context_base.jsonld` - using @base for base IRI resolution
- `typed_value.jsonld` - typed value with @value and @type (xsd:date)
- `language_tagged.jsonld` - language-tagged strings with @language
- `multiple_types.jsonld` - node with multiple @type values
- `graph.jsonld` - @graph with multiple node objects
- `reverse_property.jsonld` - @reverse for inverse relationships
- `list.jsonld` - @list for ordered collections
- `set.jsonld` - @set for unordered sets
- `compact_iri.jsonld` - compact IRI notation (prefix:suffix)
- `blank_node.jsonld` - blank node identifiers (_:name)
- `expanded.jsonld` - pre-expanded form (full IRIs, no context)
- `direction.jsonld` - @direction for text direction
- `included.jsonld` - @included for auxiliary node definitions
- `index.jsonld` - @index for data indexing
- `multiple_contexts.jsonld` - context arrays (multiple contexts)
- `type_coercion.jsonld` - @type coercion in term definitions (@id, typed literals)
- `schema_org_event.jsonld` - schema.org Event with nested Place and Organization
- `default_language.jsonld` - default @language in context
- `empty_context.jsonld` - empty context with plain IRI property
- `nested_objects.jsonld` - deeply nested objects sharing the same context
- `expanded_term_definition.jsonld` - expanded term definitions with @id and @type
