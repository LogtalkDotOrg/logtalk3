``cbor``
========

The ``cbor`` library implements predicates for importing and exporting
data in the Concise Binary Object Representation (CBOR) format:

https://www.rfc-editor.org/rfc/rfc8949.html http://cbor.io/

This library is a work-in-progress. Currently it requires a backend
supporting unbounded integer arithmetic.

Representation
--------------

-  Maps are represented using curly-bracketed terms, ``{Pairs}``, where
   each pair uses the representation ``Key-Value``.

-  Arrays are represented using lists.

-  Both byte strings and text streams are represented using atoms.

Encoding
--------

The encoding atoms, arrays, and maps uses indefinite-length encoding.
All floats are currently encoded using decimal fractions. Encoding
indicators are not currently supported.

API documentation
-----------------

Open the
`../../docs/library_index.html#cbor <../../docs/library_index.html#cbor>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(cbor(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(cbor(tester)).
