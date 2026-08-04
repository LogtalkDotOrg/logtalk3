.. _library_byte_order:

``byte_order``
==============

The ``byte_order`` library provides predicates for converting between
integers and fixed-size byte lists in big-endian and little-endian
order. Both unsigned and signed two's-complement conversions are
supported.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(byte_order(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(byte_order(tester)).

API
---

The ``byte_order`` object provides the following predicates:

- ``integer_to_bytes/4-5``
- ``bytes_to_integer/3,5``
- ``signed_integer_to_bytes/4-5``
- ``bytes_to_signed_integer/3,5``

The byte-order argument is either ``big`` or ``little``. Predicates with
an output tail support difference-list construction. Five-argument
decoding predicates consume a fixed-size prefix and return the remaining
bytes.

These are low-level predicates that assume valid instantiated arguments.
They do not validate the byte order, byte count, integer range, byte
values, or list length. Encoding retains the requested number of
low-order bytes. Calling formats and protocols remain responsible for
domain validation and errors.
