.. _library_memcached:

``memcached``
=============

Portable Memcached client implementing the text (ASCII) protocol. This
library uses the ``sockets`` library and supports all backend Prolog
systems supported by that library: ECLiPSe, GNU Prolog, SICStus Prolog,
SWI-Prolog, and Trealla Prolog.

API documentation
-----------------

Open the
`../../apis/library_index.html#memcached <../../apis/library_index.html#memcached>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(memcached(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(memcached(tester)).

Note: Tests require a running Memcached server on ``localhost`` port
``11211`` (the default). To install and start Memcached:

On macOS:

::

   $ brew install memcached
   $ memcached -d

On Linux (Debian/Ubuntu):

::

   $ sudo apt-get install memcached
   $ sudo systemctl start memcached

On Linux (RHEL/Fedora):

::

   $ sudo dnf install memcached
   $ sudo systemctl start memcached

Protocol Version
----------------

This library implements the Memcached text (ASCII) protocol as
documented in the official specification:

https://github.com/memcached/memcached/blob/master/doc/protocol.txt

The text protocol is the standard protocol supported by all Memcached
clients and servers. All commands use simple text lines terminated by
``\r\n``.

Features
--------

- Full storage command support: ``set``, ``add``, ``replace``,
  ``append``, ``prepend``, ``cas``
- Retrieval commands: ``get``, ``gets`` (with CAS token), multi-get,
  ``gat``, ``gats``
- Key deletion with ``delete``
- Atomic increment/decrement with ``incr``/``decr``
- Item expiration management with ``touch``, ``gat``, ``gats``
- Server management: ``flush_all``, ``version``, ``stats``
- Graceful disconnect with ``quit``
- Proper handling of all protocol response codes

Usage
-----

Connecting to a Memcached server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Basic connection to localhost on the default port (11211):

::

   ?- memcached::connect(localhost, Connection).

Connection to a specific host and port:

::

   ?- memcached::connect('cache.example.com', 11211, Connection).

Storing data
~~~~~~~~~~~~

Store a value unconditionally:

::

   ?- memcached::set(Connection, my_key, 'Hello, World!').

Store with flags and expiration time (in seconds):

::

   ?- memcached::set(Connection, my_key, 'Hello!', 0, 3600).

Store only if the key does not exist:

::

   ?- memcached::add(Connection, new_key, 'New value', 0, 60).

Store only if the key already exists:

::

   ?- memcached::replace(Connection, existing_key, 'Updated', 0, 60).

Append data to an existing value:

::

   ?- memcached::append(Connection, my_key, ' World!').

Prepend data to an existing value:

::

   ?- memcached::prepend(Connection, my_key, 'Hello ').

Retrieving data
~~~~~~~~~~~~~~~

Get a value by key:

::

   ?- memcached::get(Connection, my_key, Value).

Get a value with flags:

::

   ?- memcached::get(Connection, my_key, Value, Flags).

Get a value with CAS token (for optimistic locking):

::

   ?- memcached::gets(Connection, my_key, Value, CasUnique).

Get multiple keys at once:

::

   ?- memcached::mget(Connection, [key1, key2, key3], Items).

Check-and-Set (CAS)
~~~~~~~~~~~~~~~~~~~

CAS provides optimistic locking for cache updates:

::

   ?- memcached::gets(Connection, my_key, Value, CasUnique),
      % ... process value ...
      memcached::cas(Connection, my_key, 'New value', 0, 60, CasUnique).

If another client modifies the value between ``gets`` and ``cas``, the
``cas`` command will throw ``error(memcached_error(exists), _)``.

Deleting data
~~~~~~~~~~~~~

Delete an item by key:

::

   ?- memcached::delete(Connection, my_key).

Fails if the key does not exist.

Increment/Decrement
~~~~~~~~~~~~~~~~~~~

Atomically increment a numeric value:

::

   ?- memcached::set(Connection, counter, '100'),
      memcached::incr(Connection, counter, 5, NewValue).
   % NewValue = 105

Atomically decrement a numeric value:

::

   ?- memcached::decr(Connection, counter, 3, NewValue).
   % NewValue = 102

Note: The item must already exist and contain a decimal representation
of a 64-bit unsigned integer. Decrementing below 0 yields 0.

Touch and Get-and-Touch
~~~~~~~~~~~~~~~~~~~~~~~

Update expiration time without fetching:

::

   ?- memcached::touch(Connection, my_key, 3600).

Fetch and update expiration time in one operation:

::

   ?- memcached::gat(Connection, my_key, 3600, Value).

Fetch with CAS and update expiration:

::

   ?- memcached::gats(Connection, my_key, 3600, Value, CasUnique).

Server management
~~~~~~~~~~~~~~~~~

Invalidate all items:

::

   ?- memcached::flush_all(Connection).

Invalidate all items after a delay (in seconds):

::

   ?- memcached::flush_all(Connection, 30).

Get server version:

::

   ?- memcached::version(Connection, Version).

Get server statistics:

::

   ?- memcached::stats(Connection, Stats).

Get specific statistics:

::

   ?- memcached::stats(Connection, items, Stats).

Disconnecting
~~~~~~~~~~~~~

::

   ?- memcached::disconnect(Connection).

API Summary
-----------

Connection management
~~~~~~~~~~~~~~~~~~~~~

- ``connect(+Host, +Port, -Connection)`` - Connect to server
- ``connect(+Host, -Connection)`` - Connect to server on default port
- ``disconnect(+Connection)`` - Disconnect from server

Storage commands
~~~~~~~~~~~~~~~~

- ``set(+Connection, +Key, +Value, +Flags, +ExpTime)`` - Store
  unconditionally
- ``set(+Connection, +Key, +Value)`` - Store with default flags and no
  expiration
- ``add(+Connection, +Key, +Value, +Flags, +ExpTime)`` - Store only if
  new
- ``replace(+Connection, +Key, +Value, +Flags, +ExpTime)`` - Store only
  if exists
- ``append(+Connection, +Key, +Value)`` - Append to existing value
- ``prepend(+Connection, +Key, +Value)`` - Prepend to existing value
- ``cas(+Connection, +Key, +Value, +Flags, +ExpTime, +CasUnique)`` -
  Check-and-set

Retrieval commands
~~~~~~~~~~~~~~~~~~

- ``get(+Connection, +Key, -Value)`` - Get value
- ``get(+Connection, +Key, -Value, -Flags)`` - Get value and flags
- ``gets(+Connection, +Key, -Value, -CasUnique)`` - Get with CAS token
- ``gets(+Connection, +Key, -Value, -Flags, -CasUnique)`` - Get with
  flags and CAS
- ``mget(+Connection, +Keys, -Items)`` - Multi-get

Deletion
~~~~~~~~

- ``delete(+Connection, +Key)`` - Delete item

.. _incrementdecrement-1:

Increment/Decrement
~~~~~~~~~~~~~~~~~~~

- ``incr(+Connection, +Key, +Amount, -NewValue)`` - Increment
- ``decr(+Connection, +Key, +Amount, -NewValue)`` - Decrement

Touch
~~~~~

- ``touch(+Connection, +Key, +ExpTime)`` - Update expiration
- ``gat(+Connection, +Key, +ExpTime, -Value)`` - Get and touch
- ``gats(+Connection, +Key, +ExpTime, -Value, -CasUnique)`` -
  Get-and-touch with CAS

Server commands
~~~~~~~~~~~~~~~

- ``flush_all(+Connection)`` - Invalidate all items
- ``flush_all(+Connection, +Delay)`` - Invalidate all items after delay
- ``version(+Connection, -Version)`` - Get server version
- ``stats(+Connection, -Stats)`` - Get general statistics
- ``stats(+Connection, +Argument, -Stats)`` - Get specific statistics

Error Handling
--------------

Most predicates throw ``error(memcached_error(Reason), Context)`` on
failure. Common error reasons:

- ``connection_failed`` - Could not establish TCP connection
- ``not_stored`` - Storage condition not met (e.g. ``add`` on existing
  key)
- ``exists`` - CAS conflict (item was modified by another client)
- ``not_found`` - CAS on non-existent key
- ``server_error(Message)`` - Server returned an error

Retrieval predicates (``get``, ``gets``, ``gat``, ``gats``) fail (rather
than throw) when the requested key is not found. The ``delete``,
``incr``, ``decr``, and ``touch`` predicates also fail when the key is
not found.

Key Format
----------

- Keys are atoms up to 250 characters
- Keys must not contain control characters or whitespace
- Keys are case-sensitive

Expiration Times
----------------

- ``0`` means the item never expires (may still be evicted by LRU)
- Values up to 2592000 (30 days) are treated as relative seconds
- Values over 2592000 are treated as absolute Unix timestamps
- Negative values cause immediate expiration
