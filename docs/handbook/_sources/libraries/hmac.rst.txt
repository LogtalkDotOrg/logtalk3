.. _library_hmac:

``hmac``
========

The ``hmac`` library provides a portable implementation of HMAC
(Keyed-Hashing for Message Authentication) as specified in RFC 2104:

https://datatracker.ietf.org/doc/html/rfc2104.html

The library exports a single object, ``hmac``, implementing the
``hmac_protocol`` protocol with the predicates:

- ``digest/4``
- ``hex_digest/4``
- ``digest/5``
- ``hex_digest/5``

The first argument is a hash object implementing the
``hash_digest_protocol`` protocol from the ``hashes`` library. Currently
supported hash objects are:

- ``md5``
- ``sha1``
- ``sha256``
- ``sha512``
- ``sha512_256``
- ``sha3_224``
- ``sha3_256``
- ``sha3_384``
- ``sha3_512``

On backend Prolog compilers supporting only bounded integer arithmetic,
only ``md5`` is available. On backends supporting unbounded integer
arithmetic, all the listed hash objects are available.

The ``digest/5`` and ``hex_digest/5`` predicates implement the
truncation rule described in RFC 2104 and RFC 2202 by returning the
requested number of leftmost digest bytes.

API documentation
-----------------

Open the
`../../apis/library_index.html#hmac <../../apis/library_index.html#hmac>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hmac(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hmac(tester)).

Examples
--------

Compute the HMAC-SHA-256 digest for a text message and return it as a
hexadecimal atom:

::

   | ?- atom_codes('Jefe', Key),
        atom_codes('what do ya want for nothing?', Message),
        hmac::hex_digest(sha256, Key, Message, Digest).
   Key = [74,101,102,101],
   Message = [119,104,97,116,32,100,111,32,121,97,32,119,97,110,116,32,102,111,114,32,110,111,116,104,105,110,103,63],
   Digest = '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843'
   yes

Compute a truncated 16-byte HMAC-SHA-256 digest:

::

   | ?- atom_codes('Jefe', Key),
        atom_codes('what do ya want for nothing?', Message),
        hmac::hex_digest(sha256, Key, Message, 16, Digest).
   Key = [74,101,102,101],
   Message = [119,104,97,116,32,100,111,32,121,97,32,119,97,110,116,32,102,111,114,32,110,111,116,104,105,110,103,63],
   Digest = '5bdcc146bf60754e6a042426089575c7'
   yes
