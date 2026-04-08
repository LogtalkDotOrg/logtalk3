.. _library_toml:

``toml``
========

The ``toml`` library provides predicates for parsing and generating data
in the TOML format:

https://toml.io

It includes parametric objects whose parameters allow selecting the
representation for parsed TOML tables (``compound`` or ``curly``), TOML
text strings (``atom``, ``chars``, or ``codes``) and TOML pairs
(``dash``, ``equal``, or ``colon``).

API documentation
-----------------

Open the
`../../apis/library_index.html#toml <../../apis/library_index.html#toml>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(toml(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(toml(tester)).

Status
------

The full TOML 1.0.0 data model is covered:

- key/value assignments
- standard tables using ``[table]`` headers
- dotted keys
- arrays
- inline tables
- array-of-tables
- booleans
- decimal, hexadecimal, octal, and binary integers
- decimal floats plus ``inf`` and ``nan``
- single-line and multiline basic and literal strings
- local dates, local times, local date-times, and offset date-times
- parsing and generation from files, streams, chars, codes, and atoms

Representation
--------------

The following choices of syntax have been made to represent TOML
elements as terms:

- By default, TOML tables are represented using the compound term
  ``toml(Pairs)``, where ``Pairs`` is a list using the selected pair
  representation.

- Alternatively, TOML tables can be represented using curly-bracketed
  terms, ``{Pairs}``, where ``Pairs`` uses the selected pair
  representation.

- Arrays are represented using lists.

- Text strings can be represented as atoms, ``chars(List)``, or
  ``codes(List)``. The default when decoding is to use atoms when using
  the ``toml`` object. To decode text strings into lists of chars or
  codes, use the ``toml/1`` object with the parameter bound to ``chars``
  or ``codes``.

- TOML booleans are represented by ``@true`` and ``@false``.

- TOML special floats are represented by ``@(inf)``, ``@(-inf)``, and
  ``@(nan)``.

Examples using the default ``toml`` object:

============================ ====================================
TOML                         term
============================ ====================================
``title = "Example"``        ``toml([title-'Example'])``
``point = {x = 1, y = 2}``   ``toml([point-toml([x-1, y-2])])``
``[owner]`` ``name = "Tom"`` ``toml([owner-toml([name-'Tom'])])``
``enabled = true``           ``toml([enabled-@true])``
``limits = [1, 2, 3]``       ``toml([limits-[1,2,3]])``
============================ ====================================

Examples
--------

Parsing a TOML String
~~~~~~~~~~~~~~~~~~~~~

::

   |?- toml::parse(atom('title = "Example"\n[owner]\nname = "Tom"\n'), TOML).
   TOML = toml([title-'Example', owner-toml([name-'Tom'])]).

Parsing with Curly Table Representation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

::

   | ?- toml(curly,dash,atom)::parse(atom('title = "Example"\n[owner]\nname = "Tom"\n'), TOML).
   TOML = {title-'Example', owner-{name-'Tom'}}.

Generating TOML
~~~~~~~~~~~~~~~

::

   | ?- toml::generate(atom(Atom), toml([title-'Example', owner-toml([name-'Tom'])])).
   Atom = 'title = "Example"\n\n[owner]\nname = "Tom"\n'.

Notes
-----

Generation is canonical and semantic. The current implementation does
not preserve comments, original quoting style, or whether a table was
originally written as an inline table or a standard table.
