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
- ``sha224``
- ``sha256``
- ``sha384``
- ``sha512``
- ``sha512_256``
- ``sha3_224``
- ``sha3_256``
- ``sha3_384``
- ``sha3_512``
- ``blake2b``
- ``blake2s``

On backend Prolog compilers supporting only bounded integer arithmetic,
only ``md5`` and ``blake2s`` are available. On backends supporting
unbounded integer arithmetic, all the listed hash objects are available.

Note that only the non-parametric ``blake2b`` and ``blake2s`` objects
should be used as HMAC provides its own keying construction and expects
the full digest length in order to compute standard HMAC-BLAKE2b or
HMAC-BLAKE2s digests compatible with other implementations. Using the
BLAK2 parametric objects with a non-empty ``Key`` still works (no error
is raised, since a keyed instance implements ``hash_digest_protocol``
just as well as an unkeyed one), but it applies BLAKE2's own keying on
top of HMAC's ipad/opad-mixed input, which is not standard
HMAC-BLAKE2b/BLAKE2s and will not match the output of other
implementations. If a key is already available, using BLAKE2's own
native keying directly, without wrapping it in HMAC, is both simpler and
the more usual choice; ``hmac`` over an unkeyed BLAKE2 instance remains
useful when interoperating with systems, message formats, or test
vectors that specifically call for HMAC-BLAKE2b or HMAC-BLAKE2s.

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

Compute the HMAC-BLAKE2b digest for a text message, using the unkeyed,
full-length (64-byte) ``blake2b`` object:

::

   | ?- atom_codes('Jefe', Key),
        atom_codes('what do ya want for nothing?', Message),
        hmac::hex_digest(blake2b, Key, Message, Digest).
   Key = [74,101,102,101],
   Message = [119,104,97,116,32,100,111,32,121,97,32,119,97,110,116,32,102,111,114,32,110,111,116,104,105,110,103,63],
   Digest = '6ff884f8ddc2a6586b3c98a4cd6ebdf14ec10204b6710073eb5865ade37a2643b8807c1335d107ecdb9ffeaeb6828c4625ba172c66379efcd222c2de11727ab4'
   yes

Compute the HMAC-BLAKE2s digest for the same key and message, using the
unkeyed, full-length (32-byte) ``blake2s`` object; ``blake2s`` is
available even on backend Prolog compilers with only bounded integer
arithmetic:

::

   | ?- atom_codes('Jefe', Key),
        atom_codes('what do ya want for nothing?', Message),
        hmac::hex_digest(blake2s, Key, Message, Digest).
   Key = [74,101,102,101],
   Message = [119,104,97,116,32,100,111,32,121,97,32,119,97,110,116,32,102,111,114,32,110,111,116,104,105,110,103,63],
   Digest = '90b6281e2f3038c9056af0b4a7e763cae6fe5d9eb4386a0ec95237890c104ff0'
   yes
