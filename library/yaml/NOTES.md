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


`yaml`
======

The `yaml` library provides predicates for parsing and generating data
in the YAML format:

https://yaml.org

It includes parsing and generation of simple scalar values, lists, mappings,
and nested structures.


API documentation
-----------------

Open the [../../apis/library_index.html#yaml](../../apis/library_index.html#yaml)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(yaml(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(yaml(tester)).


API
---

### Parsing

The `parse/2` predicate parses YAML content from various sources into a
structured Logtalk term representation.

```logtalk
parse(+Source, -YAML)
```

Sources can be:
- `file(Path)` - parse YAML from a file
- `stream(Stream)` - parse YAML from an open stream
- `codes(Codes)` - parse YAML from a list of character codes
- `chars(Chars)` - parse YAML from a list of characters
- `atom(Atom)` - parse YAML from an atom

The parsed YAML is returned as a compound term with the functor `yaml/1`.

### Generating

The `generate/2` predicate generates YAML output from a structured term.

```logtalk
generate(+Sink, +YAML)
```

Sinks can be:
- `file(Path)` - write YAML to a file
- `stream(Stream)` - write YAML to an open stream
- `codes(Codes)` - get YAML as a list of character codes
- `chars(Chars)` - get YAML as a list of characters
- `atom(Atom)` - get YAML as an atom

The input must be a compound term representing YAML data (e.g., `yaml([...])` for mappings, `[...]` for sequences).

### Multi-Document Parsing

The `parse_all/2` predicate parses all YAML documents from a source into a list.

```logtalk
parse_all(+Source, -YAMLs)
```

Documents are separated by `---` markers and optionally terminated by `...` markers.
Returns a list of YAML terms, even for single-document sources.

### Multi-Document Generation

The `generate_all/2` predicate generates YAML output with multiple documents.

```logtalk
generate_all(+Sink, +YAMLs)
```

Takes a list of YAML terms and generates output with `---` separators between documents.
For a single document, no separator is added.

Data Representation
-------------------

YAML data is represented using the following mappings:

| YAML Value      | Logtalk Representation  |
|-----------------|-------------------------|
| `null`          | `'@'(null)`             |
| `true`          | `'@'(true)`             |
| `false`         | `'@'(false)`            |
| Number (42)     | `42`                    |
| String ("text") | `text` (as atom)        |
| Array/List      | `[item1, item2, ...]`   |
| Mapping/Object  | `yaml([key1-val1, ...])` |

Examples
--------

### Parsing a Simple YAML String

```logtalk
?- parse(atom('name: John\nage: 30'), YAML).
YAML = yaml([name-'John', age-30]).
```

### Parsing a YAML List

```logtalk
?- parse(codes([0'-, 0' , 0'a, ...]), YAML).
YAML = yaml([apple, banana, orange]).
```

### Parsing a YAML File

```logtalk
?- parse(file('config.yaml'), YAML).
% Reads and parses config.yaml
```

### Generating YAML from a Term

```logtalk
?- generate(atom(Result), yaml([title-'Project', version-1])).
Result = '{title:Project,version:1}'.
```

### Generating YAML to a File

```logtalk
?- Data = yaml([name-'Alice', score-95]),
   generate(file('output.yaml'), Data).
% Writes YAML data to output.yaml
```

### Working with Nested Structures

```logtalk
?- YAML = yaml([
     person-yaml([
       name-'Bob',
       age-25,
       skills-[logtalk, prolog, python]
     ])
   ]),
   generate(atom(Result), YAML).
% Generates YAML representation of nested structure
```

Features
--------

- **Scalar values**: Supports atoms, numbers, booleans, and null
- **Collections**: Supports lists (sequences) and mappings (objects)
- **Nested structures**: Fully supports nested mappings and sequences
- **Comments**: Handles YAML comments (lines starting with #)
- **Multiple source formats**: Parse from files, streams, codes, chars, or atoms
- **Multiple output formats**: Generate to files, streams, codes, chars, or atoms
- **Error handling**: Provides appropriate error messages for invalid input

Supported YAML Features
-----------------------

The library currently supports a subset of YAML 1.2 features suitable for
common configuration and data serialization tasks:

**Scalars**

- Strings: unquoted, single-quoted (`'...'`), and double-quoted (`"..."`)
- Multi-word keys: keys containing spaces (e.g., `Mark McGwire: 65`)
- Numbers:
  - Integers: decimal (`42`), signed positive (`+12345`), octal (`0o14`), hexadecimal (`0xC`)
  - Floats: decimal (`3.14`), scientific notation (`1.23e+3`)
  - Special values: `.inf`, `-.inf`, `.nan` (represented as `'@'(inf)`, `'@'(-inf)`, `'@'(nan)`)
- Booleans: `true` and `false`
- Null: `null` or empty values
- Timestamps: ISO 8601 dates and times (parsed as strings)

**Collections**

- Block sequences: using `-` indicator with proper indentation
- Flow sequences: using `[ ]` syntax with comma separation
- Block mappings: using `key: value` syntax with proper indentation
- Flow mappings: using `{ key: value }` syntax
- Nested structures: arbitrary nesting of sequences and mappings
- Trailing commas: supported in flow collections

**Block Scalars**

- Literal block scalars: `|` style preserves newlines exactly
- Folded block scalars: `>` style folds newlines into spaces
- Chomping indicators: `-` (strip), `+` (keep), default (clip) for trailing newlines
- More-indented lines: preserved with original indentation in folded scalars
- Blank lines: preserved in both literal and folded block scalars

**Anchors and Aliases**

- Anchor definitions: `&name` to define a reusable node
- Alias references: `*name` to reference a previously defined anchor
- Merge key: `<<` to merge mappings from aliases
- Works with scalars, sequences, and mappings

**Document Structure**

- Document start marker: `---` is recognized and skipped
- Document end marker: `...` is recognized and handled
- Multi-document streams: multiple `---` or `...` separated documents (via `parse_all/2`)
- Comments: lines starting with `#` and inline comments after values
- Tags: `!tag`, `!!type`, and `!<uri>` tags are recognized and skipped

**Escape Sequences** (in double-quoted strings)

- `\\` - backslash
- `\"` - double quote
- `\/` - forward slash
- `\n` - newline
- `\t` - tab
- `\r` - carriage return
- `\b` - backspace
- `\f` - form feed
- `\0` - null character
- `\xXX` - hex escape (2 hex digits)
- `\uXXXX` - Unicode escape (4 hex digits)

Limitations
-----------

The following YAML features are **not** currently supported:

- Tag interpretation: Tags are recognized and skipped, but not interpreted
- Complex keys: `?` indicator for multi-line or complex keys
- Multi-line plain scalars: plain scalars spanning multiple lines
- Multi-line flow scalars: line folding in flow context
- Aliases in flow sequences: `[*alias1, *alias2]` (aliases work in block context)
- Merge with list of aliases: `<<: [*a, *b]` (single alias merge works)
- Directives: `%YAML` and `%TAG` directives

Error Handling
--------------

The library raises the following errors:

- `instantiation_error` - when a required argument is a variable
- `domain_error(yaml_source, Source)` - when an invalid source is provided
- `domain_error(yaml_sink, Sink)` - when an invalid sink is provided
- `domain_error(yaml_term, Term)` - when an invalid YAML term is provided

File Organization
------------------

- `yaml_protocol.lgt` - Defines the protocol for YAML parser/generator
- `yaml.lgt` - Main implementation object
- `loader.lgt` - Library loader
- `tester.lgt` - Test runner
- `test_files/` - Directory containing test files and sample YAML files for testing

Test Files
----------

The library includes several sample YAML files for testing:

- `simple.yaml` - Simple key-value pairs
- `list.yaml` - A YAML list
- `nested.yaml` - Nested structures
- `complex.yaml` - Lists of mappings
- `config.yaml` - Configuration file example
- `metadata.yaml` - Metadata example with quoted strings

Performance Considerations
---------------------------

- Parsing is done recursively, which may affect performance with deeply nested structures
- Large YAML files are processed in memory
- For very large files, consider splitting them into smaller chunks

Integration with Other Libraries
---------------------------------

The YAML library can be integrated with other Logtalk libraries:

- Use with `os` library for file path handling
- Use with `reader` library for stream management
- Compatible with `term_io` library for term serialization

Future Enhancements
-------------------

Potential future enhancements may include:

- Support for tag interpretation (e.g., `!!binary` for base64 decoding)
- Support for complex keys using `?` indicator
- Support for multi-line plain scalars
- Aliases in flow sequences
- Merge key with list of aliases
- Parametric objects for custom representation choices

See Also
--------

- `json_lines` library for JSON Lines format parsing and generation
- Logtalk documentation on DCGs for grammar rules
- YAML specification: <https://yaml.org>
