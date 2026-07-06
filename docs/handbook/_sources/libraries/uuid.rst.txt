.. _library_uuid:

``uuid``
========

This library implements a Universally Unique Identifier (UUID)
generator. Currently version 1, version 3, version 4, version 5, and
version 7 UUIDs are supported. Version 5 support requires a Prolog
backend with support for unbounded integer arithmetic. For reference
material, see e.g.

https://en.wikipedia.org/wiki/Universally_unique_identifier

Some backends provide time stamps with low granularity (e.g., seconds
but not milliseconds or nanoseconds). To compensate, the generation of
version 1 UUIDs uses 14 random bits for the clock sequence.

Some backends only provide access to local time. On those backends, the
``uuid_v1/2`` and ``uuid_v7/1`` predicates preserve the backend behavior
and use the available local time fields when computing the UUID
timestamp. To compute UUID timestamps using UTC instead, use the
``uuid_v1/3`` and ``uuid_v7/2`` predicates and pass the current local
UTC offset as ``Z``, ``+HH:MM``, or ``-HH:MM``.

The generation of version 4 and version 7 UUIDs uses the
``/dev/urandom`` random number generator when available. This includes
macOS, Linux, \*BSD, and other POSIX operating-systems. On Windows, a
pseudo-random generator is used, but randomized using the current wall
time.

Version 3 and version 5 UUIDs are namespace-name based UUIDs using the
MD5 and SHA-1 hash functions, respectively.

Version 7 UUIDs are time-ordered using a Unix Epoch timestamp in
milliseconds, as specified in RFC 9562. They are recommended over
version 1 UUIDs for new applications due to improved entropy and
sortability characteristics.

UUIDs can be generated as atoms, lists of characters, or lists of
character codes.

See also the ``cuid2``, ``ksuid``, ``ids``, ``nanoid``, ``snowflakeid``,
and ``ulid`` libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#uuid <../../apis/library_index.html#uuid>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(uuid(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(uuid(tester)).

Generating version 1 UUIDs
--------------------------

By default, version 1 UUIDs are generated as atoms. For example:

::

   | ?- uuid::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
   UUID = '00a66fc0-82cf-11eb-bc83-f2d19094dc4b'
   yes

To generate a UUID using a list of characters representation, use
instead the ``uuid/1`` parametric object:

::

   | ?- uuid(chars)::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
   UUID = ['0','0',d,e,'9','0',c,'0',-,'8','2',c,f,-,'1','1',e,b,-,
           a,'9','8','5',-,f,'2',d,'1','9','0','9','4',d,c,'4',b]
   yes

Similarly, to get a UUID using a list of character codes representation:

::

   | ?- uuid(codes)::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], UUID).
   UUID = [48,48,52,99,99,54,99,48,45,56,50,99,102,45,49,49,101,98,45,
           98,57,102,52,45,102,50,100,49,57,48,57,52,100,99,52,98]
   yes

When the backend only provides access to local time, use ``uuid_v1/3``
with the current local UTC offset to compute the UUID timestamp using
UTC:

::

   | ?- uuid::uuid_v1([0xf2,0xd1,0x90,0x94,0xdc,0x4b], '+01:00', UUID).
   UUID = '00a66fc0-82cf-11eb-bc83-f2d19094dc4b'
   yes

Generating version 4 UUIDs
--------------------------

By default, version 4 UUIDs are generated as atoms. For example:

::

   | ?- uuid::uuid_v4(UUID).
   UUID = '1c652782-69c5-4252-88c8-09e576a44db5'
   yes

To generate a UUID using a list of characters representation, use
instead the ``uuid/1`` parametric object:

::

   | ?- uuid(chars)::uuid_v4(UUID).
   UUID = [d,'3',d,'3','3','5','1','3',-,'8','1',e,c,-,'4',d,'2','6',-,
           '9',f,'2','2',-,e,d,'9','5',e,'0','0',e,'1','5','7','0']
   yes

Similar to get a UUID using a list of character codes representation:

::

   | ?- uuid(codes)::uuid_v4(UUID).
   UUID = [102,97,52,54,57,98,100,50,45,51,57,54,51,45,52,97,100,55,45,
           98,50,50,55,45,101,100,52,99,56,55,99,54,53,55,102,98]
   yes

Generating version 3 UUIDs
--------------------------

Version 3 UUIDs are namespace-name based UUIDs using the MD5 hash
function. For example:

::

   | ?- uuid::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = '3d813cbb-47fb-32ba-91df-831e1593ac29'
   yes

To generate a UUID using a list of characters representation, use
instead the ``uuid/1`` parametric object:

::

   | ?- uuid(chars)::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = ['3',d,'8','1','3',c,b,b,-,'4','7',f,b,-,'3','2',b,a,-,
           '9','1',d,f,-,'8','3','1',e,'1','5','9','3',a,c,'2','9']
   yes

Similarly, to get a UUID using a list of character codes representation:

::

   | ?- uuid(codes)::uuid_v3('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = [51,100,56,49,51,99,98,98,45,52,55,102,98,45,51,50,98,97,45,
           57,49,100,102,45,56,51,49,101,49,53,57,51,97,99,50,57]
   yes

Generating version 5 UUIDs
--------------------------

Version 5 UUIDs are namespace-name based UUIDs using the SHA-1 hash
function. This predicate is only available on Prolog backends with
support for unbounded integer arithmetic. For example:

::

   | ?- uuid::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = '21f7f8de-8051-5b89-8680-0195ef798b6a'
   yes

To generate a UUID using a list of characters representation, use
instead the ``uuid/1`` parametric object:

::

   | ?- uuid(chars)::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = ['2','1',f,'7',f,'8',d,e,-,'8','0','5','1',-,'5',b,'8','9',-,
           '8','6','8','0',-,'0','1','9','5',e,f,'7','9','8',b,'6',a]
   yes

Similarly, to get a UUID using a list of character codes representation:

::

   | ?- uuid(codes)::uuid_v5('6ba7b810-9dad-11d1-80b4-00c04fd430c8', 'www.widgets.com', UUID).
   UUID = [50,49,102,55,102,56,100,101,45,56,48,53,49,45,53,98,56,57,45,
           56,54,56,48,45,48,49,57,53,101,102,55,57,56,98,54,97]
   yes

Generating version 7 UUIDs
--------------------------

Version 7 UUIDs are time-ordered using the Unix Epoch timestamp in
milliseconds (as specified in RFC 9562). By default, version 7 UUIDs are
generated as atoms. For example:

::

   | ?- uuid::uuid_v7(UUID).
   UUID = '018d5f3c-9b5a-7c4e-8f2a-1b3c4d5e6f70'
   yes

To generate a UUID using a list of characters representation, use
instead the ``uuid/1`` parametric object:

::

   | ?- uuid(chars)::uuid_v7(UUID).
   UUID = ['0','1','8',d,'5',f,'3',c,-,'9',b,'5',a,-,'7',c,'4',e,-,
           '8',f,'2',a,-,'1',b,'3',c,'4',d,'5',e,'6',f,'7','0']
   yes

Similar to get a UUID using a list of character codes representation:

::

   | ?- uuid(codes)::uuid_v7(UUID).
   UUID = [48,49,56,100,53,102,51,99,45,57,98,53,97,45,55,99,52,101,45,
           56,102,50,97,45,49,98,51,99,52,100,53,101,54,102,55,48]
   yes

When the backend only provides access to local time, use ``uuid_v7/2``
with the current local UTC offset to compute the UUID timestamp using
UTC:

::

   | ?- uuid::uuid_v7('+01:00', UUID).
   UUID = '018d5f3c-9b5a-7c4e-8f2a-1b3c4d5e6f70'
   yes

Generating the Nil and Max UUIDs
--------------------------------

Predicates are also provided that return the Nil and Max UUIDs:

::

   | ?- uuid::uuid_nil(UUID).
   UUID = '00000000-0000-0000-0000-000000000000'
   yes

   | ?- uuid::uuid_max(UUID).
   UUID = 'FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF'
   yes
