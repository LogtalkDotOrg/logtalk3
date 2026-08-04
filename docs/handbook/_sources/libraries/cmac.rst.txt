.. _library_cmac:

``cmac``
========

The ``cmac`` library provides a portable implementation of the
Cipher-based Message Authentication Code specified in NIST SP 800-38B:

https://doi.org/10.6028/NIST.SP.800-38B

The AES-CMAC specialization and test vectors are also described in RFC
4493:

https://datatracker.ietf.org/doc/html/rfc4493.html

The library exports a single object, ``cmac``, implementing the
``cmac_protocol`` protocol with the predicates:

- ``digest/4``
- ``hex_digest/4``
- ``digest/5``
- ``hex_digest/5``

The first argument is a block cipher object implementing the
``block_cipher_prepared_key_protocol`` protocol from the
``block_ciphers`` library. CMAC supports the standard 8-byte and 16-byte
block sizes. The currently available cipher objects are ``aes128``,
``aes192``, and ``aes256``, all using 16-byte blocks.

The ``digest/5`` and ``hex_digest/5`` predicates return the requested
number of leftmost digest bytes. The length must be between one and the
cipher block size. The communicating parties must agree on a fixed tag
length before a key is used and must not change it during the lifetime
of that key.

API documentation
-----------------

Open the
`../../apis/library_index.html#cmac <../../apis/library_index.html#cmac>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(cmac(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(cmac(tester)).

Security considerations
-----------------------

CMAC authenticates data and provides integrity protection but does not
encrypt data. Use an authenticated-encryption construction when
confidentiality is also required.

Keep keys secret and generate them using a cryptographically secure
source. NIST SP 800-38B recommends tags of at least 8 bytes as
protection against guessing attacks. Applications comparing received and
computed tags should use an appropriate constant-time comparison
operation.

The portable block cipher implementations used by this library are not
guaranteed to execute in constant time and make no side-channel
resistance claims.

Examples
--------

Compute the RFC 4493 AES-CMAC digest for an empty message:

::

   | ?- Key = [0x2b,0x7e,0x15,0x16,0x28,0xae,0xd2,0xa6,0xab,0xf7,0x15,0x88,0x09,0xcf,0x4f,0x3c],
        cmac::hex_digest(aes128, Key, [], Digest).
   Digest = 'bb1d6929e95937287fa37d129b756746'
   yes

Compute an 8-byte truncated AES-CMAC digest:

::

   | ?- Key = [0x2b,0x7e,0x15,0x16,0x28,0xae,0xd2,0xa6,0xab,0xf7,0x15,0x88,0x09,0xcf,0x4f,0x3c],
        Message = [0x6b,0xc1,0xbe,0xe2,0x2e,0x40,0x9f,0x96,0xe9,0x3d,0x7e,0x11,0x73,0x93,0x17,0x2a],
        cmac::hex_digest(aes128, Key, Message, 8, Digest).
   Digest = '070a16b46b4d4144'
   yes
