.. _library_n_grams:

``n_grams``
===========

This library provides predicates for generating and counting token and
character n-grams represented as atoms, character lists, or character
code lists.

API documentation
-----------------

Open the
`../../apis/library_index.html#n-grams <../../apis/library_index.html#n-grams>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(n_grams(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(n_grams(tester)).

Usage
-----

The ``n_grams(Representation)`` parametric object accepts the ``atom``,
``chars``, and ``codes`` representations. Token n-grams are always lists
of tokens in the configured representation. Character n-grams are text
values in that representation.

To generate word bigrams:

::

   | ?- n_grams(atom)::bigrams([the, quick, brown, fox], NGrams).
   NGrams = [[the,quick], [quick,brown], [brown,fox]]
   yes

To generate character trigrams:

::

   | ?- n_grams(atom)::character_n_grams(3, hello, NGrams).
   NGrams = [hel,ell,llo]
   yes

The default step is one. The ``step(Step)`` option changes the distance
between successive window starting positions. Only complete windows are
returned:

::

   | ?- n_grams(atom)::n_grams(2, [a,b,c,d,e], [step(2)], NGrams).
   NGrams = [[a,b], [c,d]]
   yes

Padding uses an explicit marker and adds ``N-1`` copies on each
requested side. The supported values are ``padding(none)``,
``padding(left(Marker))``, ``padding(right(Marker))``, and
``padding(both(Marker))``:

::

   | ?- n_grams(atom)::n_grams(2, [a,b], [padding(both('<pad>'))], NGrams).
   NGrams = [['<pad>',a], [a,b], [b,'<pad>']]
   yes

For token n-grams, the marker is a text value in the configured
representation. For character n-grams, it is a character atom when using
the ``atom`` or ``chars`` representation and a character code when using
``codes``.

When ``N`` is greater than the effective input length after padding, the
result is the empty list. For ``N = 1``, padding adds no markers.

Counting
--------

The ``count/2`` predicate preserves the order in which distinct n-grams
first occur:

::

   | ?- n_grams(atom)::count([ab,ba,ab,ca,ba], Counts).
   Counts = [ab-2, ba-2, ca-1]
   yes

The ``count/3`` predicate supports three orderings:

- ``first_occurrence`` preserves first occurrence order and is the
  default.
- ``standard`` uses standard term order.
- ``frequency_descending`` orders by decreasing count and uses standard
  term order to break ties.

Composability
-------------

The library consumes already-tokenized text and composes directly with
token filters. For example:

::

   | ?- stop_words(atom, stopwords_en)::exclude([the,quick,brown,fox], Tokens),
        n_grams(atom)::trigrams(Tokens, NGrams).
   Tokens = [quick,brown,fox],
   NGrams = [[quick,brown,fox]]
   yes

Tokenization, case folding, Unicode normalization, and streaming
generation are outside the scope of this library.
