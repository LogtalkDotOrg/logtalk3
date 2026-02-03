.. _library_strings:

``strings``
===========

This library provides string manipulation predicates with support for
different string representations: atoms, character lists, or character
code lists. Its API is partially based on work and libraries found in
ECLiPSe and SWI-Prolog.

The predicates are defined in the ``string(_Representation_)``
parametric object where ``_Representation_`` can be one of:

- ``atom`` - strings are represented as atoms
- ``chars`` - strings are represented as lists of characters
- ``codes`` - strings are represented as lists of character codes

The parameter must be bound when sending messages to the object.

API documentation
-----------------

Open the
`../../apis/library_index.html#strings <../../apis/library_index.html#strings>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(strings(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(strings(tester)).

Predicates
----------

The library provides the following compatibility predicates:

- ``atom_string/2`` - converts between atoms and strings
- ``number_string/2`` - converts between numbers and strings
- ``string_chars/2`` - converts between strings and character lists
- ``string_codes/2`` - converts between strings and character code lists
- ``string_concat/3`` - concatenates two strings
- ``string_length/2`` - returns the length of a string
- ``sub_string/5`` - extracts substrings
- ``string_upper/2`` - converts a string to uppercase
- ``string_lower/2`` - converts a string to lowercase
- ``split_string/4`` - splits a string into substrings using separators
  and padding
- ``atomics_to_string/2`` - concatenates a list of atomic terms into a
  string
- ``atomics_to_string/3`` - concatenates a list of atomic terms into a
  string with separator

It also provides the following string trimming predicates:

- ``trim/2-3`` - trims leading and trailing characters from a string
- ``trim_left/2-3`` - trims leading characters from a string
- ``trim_right/2-3`` - trims trailing characters from a string

For converting between terms and strings, see the ``term_io`` library.
