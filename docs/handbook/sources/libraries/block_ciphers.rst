.. _library_block_ciphers:

``block_ciphers``
=================

The ``block_ciphers`` library provides portable block cipher
implementations. It defines a ``block_cipher_protocol`` protocol with
the predicates:

- ``encrypt_block/3``
- ``block_size/1``
- ``key_size/1``

The ``encrypt_block/3`` predicate encrypts exactly one plaintext block.
Keys, plaintext blocks, and ciphertext blocks are represented by lists
of bytes.

API documentation
-----------------

Open the
`../../apis/library_index.html#block_ciphers <../../apis/library_index.html#block_ciphers>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(block_ciphers(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(block_ciphers(tester)).

Supported block ciphers
-----------------------

The library implements AES as specified in FIPS 197 using three objects:

- ``aes128``, accepting a 16-byte key
- ``aes192``, accepting a 24-byte key
- ``aes256``, accepting a 32-byte key

All three objects use 16-byte blocks. They extend ground instances of
the shared parametric object ``aes_common(KeySize, Nk, Nr)``, which
contains the key expansion and encryption round implementation.

Security considerations
-----------------------

This library exposes raw block encryption. It does not provide a cipher
mode, padding, message authentication, or authenticated encryption.
Encrypting individual message blocks directly is not a secure
message-encryption scheme.

The portable table-based implementation is not guaranteed to execute in
constant time and makes no side-channel resistance claims.

CMAC compatibility
------------------

The protocol is designed to support a future generic CMAC library. Such
a library can query ``block_size/1`` and ``key_size/1`` and use
``encrypt_block/3`` without depending on AES-specific constants or
implementation details.

Example
-------

Encrypt the standard FIPS 197 AES-128 example block:

::

   | ?- aes128::encrypt_block(
            [0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f],
            [0x00,0x11,0x22,0x33,0x44,0x55,0x66,0x77,0x88,0x99,0xaa,0xbb,0xcc,0xdd,0xee,0xff],
            Ciphertext
        ).
   Ciphertext = [105,196,224,216,106,123,4,48,216,205,183,128,112,180,197,90]
   yes
