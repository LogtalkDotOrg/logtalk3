.. _library_json_pointer:

``json_pointer``
================

The ``json_pointer`` library provides predicates for parsing,
generating, and evaluating JSON Pointers as specified by RFC 6901, plus
support for Relative JSON Pointer:

- https://www.rfc-editor.org/rfc/rfc6901
- https://json-schema.org/draft/2020-12/relative-json-pointer

It supports both the plain string syntax and the URI fragment syntax for
absolute JSON Pointers. Relative JSON Pointers use the plain string
syntax. Reference tokens can be represented as atoms, ``chars(List)``,
or ``codes(List)``.

API documentation
-----------------

Open the
`../../apis/library_index.html#json_pointer <../../apis/library_index.html#json_pointer>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(json_pointer(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(json_pointer(tester)).

Representation
--------------

The library defines the ``json_pointer(_Representation_)`` parametric
object where ``_Representation_`` can be one of:

- ``atom`` - reference tokens are represented as atoms
- ``chars`` - reference tokens are represented as ``chars(List)``
- ``codes`` - reference tokens are represented as ``codes(List)``

When using the default ``json_pointer`` object, reference tokens are
represented as atoms.

Relative JSON Pointer values are represented as
``relative(Up, Shift, Suffix)``:

- ``Up`` is the number of ancestor steps
- ``Shift`` is the optional signed array index adjustment
- ``Suffix`` is either a list of reference tokens or the atom ``'#'``

Examples
--------

Parse and evaluate a pointer:

::

   | ?- json_pointer::parse(atom('/foo/0'), Pointer),
        json_pointer::evaluate(Pointer, {foo-[bar, baz]}, Value).
   Pointer = [foo, '0']
   Value = bar
   yes

Generate a URI fragment representation:

::

   | ?- json_pointer::generate_fragment(atom(Fragment), ['a b', 'c/d']).
   Fragment = '#/a%20b/c~1d'
   yes

Evaluate a Relative JSON Pointer from a known context path:

::

   | ?- json_pointer::evaluate_relative(relative(1, 0, ['0']), [foo, '1'], {foo-[bar, baz]}, Value).
   Value = bar
   yes
