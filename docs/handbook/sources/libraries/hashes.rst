.. _library_hashes:

``hashes``
==========

The ``hashes`` library provides portable implementations of several
commonly used hashing algorithms. All hash objects implement the
``hash_protocol`` protocol by providing a ``hash/2`` predicate that
takes a list of bytes and returns the computed hash as a lowercase
hexadecimal atom.

The library implements the following hashing algorithms:

- DJB2 32-bit (``djb2_32``)
- DJB2 64-bit (``djb2_64``)
- SDBM 32-bit (``sdbm_32``)
- SDBM 64-bit (``sdbm_64``)
- FNV-1a 32-bit (``fnv1a_32``)
- FNV-1a 64-bit (``fnv1a_64``)
- SipHash (``siphash_2_4``)
- CRC32 (``crc32``)
- MurmurHash3 x86 32-bit (``murmurhash3_x86_32``)
- MurmurHash3 x86 128-bit (``murmurhash3_x86_128``)
- MurmurHash3 x64 128-bit (``murmurhash3_x64_128``)
- MD5 (``md5``)
- SHA1 (``sha1``)
- SHA256 (``sha256``)

The ``djb2_64``, ``sdbm_64``, ``fnv1a_64``, ``siphash_2_4``,
``murmurhash3_x86_128``, ``murmurhash3_x64_128``, ``sha1``, and
``sha256`` objects are only loaded on backend Prolog compilers
supporting unbounded integer arithmetic.

The ``siphash_2_4`` object uses the standard reference key
``00 01 02 ... 0f``. For custom keys, use the parametric object
``siphash_2_4(Key)`` where ``Key`` is a list of 16 bytes.

The implementations of the hashing algorithms make no attempt to
validate that the input is a list of bytes. When necessary, use the
``types`` library ``type::check(list(byte), Input)`` goal before calling
the ``hash/2`` predicate.

API documentation
-----------------

Open the
`../../apis/library_index.html#hashes <../../apis/library_index.html#hashes>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hashes(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hashes(tester)).

Examples
--------

Compute the SHA-256 hash of a text message:

::

   | ?- atom_codes('The quick brown fox jumps over the lazy dog', Bytes),
        sha256::hash(Bytes, Hash).
   Bytes = [84,104,101,32,113,117,105,99,107,32,98,114,111,119,110,32,102,111,120,32,106,117,109,112,115,32,111,118,101,114,32,116,104,101,32,108,97,122,121,32,100,111,103],
   Hash = 'd7a8fbb307d7809469ca9abcb0082e4f8d5651e46d3cdb762d02d0bf37c9e592'
   yes

Compute the CRC32 checksum for the standard ``123456789`` test vector:

::

   | ?- atom_codes('123456789', Bytes), crc32::hash(Bytes, Hash).
   Bytes = [49,50,51,52,53,54,55,56,57],
   Hash = 'cbf43926'
   yes

Use SipHash-2-4 with a custom key on a backend supporting unbounded
integer arithmetic:

::

   | ?- siphash_2_4([0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15])::hash([0,1,2,3], Hash).
   Hash = 'cf2794e0277187b7'
   yes
