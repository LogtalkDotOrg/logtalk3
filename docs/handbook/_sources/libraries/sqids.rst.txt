.. _library_sqids:

``sqids``
=========

This library implements the Sqids specification:

- Specification: https://github.com/sqids/sqids-spec
- Homepage: https://sqids.org
- Default blocklist source: https://github.com/sqids/sqids-blocklist

It encodes a lists of non-negative integers into short, obfuscated (but
*not* encrypted), URL-safe ids, and decodes them back. Typical uses are
shortening database primary keys, or batches of keys, for public-facing
URLs. Sqids is **not** suitable for security or privacy purposes: given
the alphabet, ids are trivially reversible.

API documentation
-----------------

Open the
`../../apis/library_index.html#sqids <../../apis/library_index.html#sqids>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(sqids(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(sqids(tester)).

Usage
-----

Encoding and decoding:

::

   | ?- sqids::encode([1,2,3], Id).
   Id = '86Rf07'.

   | ?- sqids::decode('86Rf07', Numbers).
   Numbers = [1, 2, 3].

Using encoding options:

::

   | ?- sqids::encode([1,2,3], [min_length(10)], Id).
   Id = '86Rf07xd4z'.

   | ?- sqids::encode([1,2,3], [alphabet(abc)], Id).
   Id = aacacbaa.

   | ?- sqids::encode([1,2,3], [blocklist(['86Rf07'])], Id).
   Id = se8ojk.

Options
-------

The ``encode/3`` and ``decode/3`` predicates take an options list. The
common option is:

- ``alphabet(Atom)`` — an atom with at least three unique characters.
  The default value is the standard 62-character alphabet. In the case
  of the ``decode/3`` predicate, it must be the same alphabet used to
  encode.

THere are also two ``encode/3`` only options:

- ``min_length(Integer)`` — an integer between 0 and 255 (default: 0).

- ``blocklist(list(atom))`` — words that must not occur in the generated
  identifier. When the canonical encoding contains a blocked word, an
  alternative encoding is tried. The default value is ``[]``. See the
  note below on the default blocklist.

Default profanity blocklist
---------------------------

This library does not bundle a default profanity blocklist (unlike some
official Sqids implementations). The reference blocklist is maintained
separately at:

https://github.com/sqids/sqids-blocklist

It can be supplied by the caller via the ``blocklist/1`` option. This
mirrors the reference TypeScript implementation itself, which imports
its default blocklist from a separate JSON data file rather than
embedding it in the core algorithm.

Arbitrary-precision integers
----------------------------

Most Sqids implementations impose an artificial maximum encodable value
(typically ``Number.MAX_SAFE_INTEGER`` or a fixed-width integer type
limit) because their host language lacks unbounded integers. Depending
on the Prolog backend, integers are either bound or have arbitrary
precision. Therefore, the ``encode/2-3`` predicates only reject numbers
that are not integers or that are negative with no explicit upper bound.
