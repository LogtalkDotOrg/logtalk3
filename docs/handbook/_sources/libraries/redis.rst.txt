.. _library_redis:

``redis``
=========

Redis client library. Supports ECLiPSe, GNU Prolog, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and XVM.

For general information on Redis, including a list of the available
commands, visit:

::

   https://redis.io

API documentation
-----------------

Open the
`../../apis/library_index.html#redis <../../apis/library_index.html#redis>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(redis(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(redis(tester)).

The tests assume a localhost Redis server running on the default port
(6379) if the ``REDIS_HOST`` and ``REDIS_PORT`` environment variables
are not defined. If the server is not detected, the tests are skipped.

The unit tests were originally written by Sean Charles for his GNU
Prolog Redis client library:

::

   https://github.com/emacstheviking/gnuprolog-redisclient

The Logtalk version is a straight-forward port of the original tests
using the ``test/1`` dialect of ``lgtunit``.

Supported Redis Features
------------------------

This library provides wrapper predicates for commonly used Redis
operations across multiple data types. For Redis commands not covered by
a wrapper, use the generic ``redis::send/3`` predicate with the command
as a compound term.

Connection Management
~~~~~~~~~~~~~~~~~~~~~

- ``connect/1`` - Connect to localhost on default port (6379)
- ``connect/3`` - Connect to specified host and port
- ``disconnect/1`` - Disconnect from Redis server
- ``send/3`` - Send any Redis command and receive reply

String Operations
~~~~~~~~~~~~~~~~~

- ``get/3`` - Get the value of a key
- ``set/4`` - Set the value of a key
- ``append/4`` - Append a value to a key
- ``getrange/5`` - Get substring of string stored at key
- ``setrange/5`` - Overwrite part of a string at key starting at offset
- ``strlen/3`` - Get the length of the value stored at key
- ``mget/3`` - Get values of multiple keys
- ``mset/3`` - Set multiple key-value pairs atomically
- ``incr/3`` - Increment integer value of key by one
- ``decr/3`` - Decrement integer value of key by one
- ``incrby/4`` - Increment integer value of key by amount
- ``decrby/4`` - Decrement integer value of key by amount

Key Operations
~~~~~~~~~~~~~~

- ``del/3`` - Delete a key
- ``exists/3`` - Check if a key exists
- ``keys/3`` - Find all keys matching a pattern
- ``ttl/3`` - Get time to live for a key in seconds
- ``expire/4`` - Set a timeout on a key in seconds
- ``persist/3`` - Remove the expiration from a key
- ``rename/4`` - Rename a key
- ``type/3`` - Get the type of value stored at key

Hash Operations
~~~~~~~~~~~~~~~

Hashes are maps between string fields and string values, ideal for
representing objects.

- ``hset/5`` - Set field in a hash
- ``hget/4`` - Get value of field in a hash
- ``hgetall/3`` - Get all fields and values in a hash
- ``hdel/4`` - Delete field from a hash
- ``hexists/4`` - Check if field exists in a hash
- ``hkeys/3`` - Get all field names in a hash
- ``hvals/3`` - Get all values in a hash
- ``hlen/3`` - Get number of fields in a hash

List Operations
~~~~~~~~~~~~~~~

Redis lists are ordered collections of strings, sorted by insertion
order.

- ``lpush/4`` - Prepend value to a list
- ``rpush/4`` - Append value to a list
- ``lpop/3`` - Remove and return first element of list
- ``rpop/3`` - Remove and return last element of list
- ``lrange/5`` - Get range of elements from list
- ``llen/3`` - Get length of list
- ``lrem/5`` - Remove elements from list
- ``ltrim/5`` - Trim list to specified range

Set Operations
~~~~~~~~~~~~~~

Redis sets are unordered collections of unique strings.

- ``sadd/4`` - Add member to a set
- ``srem/4`` - Remove member from a set
- ``smembers/3`` - Get all members in a set
- ``sismember/4`` - Check if value is member of set
- ``scard/3`` - Get number of members in a set

Sorted Set Operations
~~~~~~~~~~~~~~~~~~~~~

Sorted sets are collections of unique strings (members) ordered by an
associated score. Members are unique, but scores may repeat.

- ``zadd/5`` - Add member with score to sorted set
- ``zrem/4`` - Remove member from sorted set
- ``zrange/5`` - Get range of members from sorted set by index
- ``zrank/4`` - Get rank (index) of member in sorted set
- ``zcard/3`` - Get number of members in sorted set
- ``zscore/4`` - Get score of member in sorted set

Usage Examples
~~~~~~~~~~~~~~

::

   % Connect and perform basic string operations
   ?- redis::connect(Connection),
      redis::set(Connection, mykey, 'Hello World', Status),
      redis::get(Connection, mykey, Value),
      redis::disconnect(Connection).
   Status = 'OK',
   Value = 'Hello World'.

   % Working with hashes
   ?- redis::connect(Connection),
      redis::hset(Connection, user:1000, name, 'John Doe', _),
      redis::hset(Connection, user:1000, email, 'john@example.com', _),
      redis::hgetall(Connection, user:1000, Fields),
      redis::disconnect(Connection).
   Fields = [bulk(name), bulk('John Doe'), bulk(email), bulk('john@example.com')].

   % Using lists as queues
   ?- redis::connect(Connection),
      redis::rpush(Connection, queue, task1, _),
      redis::rpush(Connection, queue, task2, _),
      redis::lpop(Connection, queue, Task),
      redis::disconnect(Connection).
   Task = task1.

   % Batch operations with mget/mset
   ?- redis::connect(Connection),
      redis::mset(Connection, [key1, val1, key2, val2], Status),
      redis::mget(Connection, [key1, key2], Values),
      redis::disconnect(Connection).
   Status = 'OK',
   Values = [bulk(val1), bulk(val2)].

   % Key expiration
   ?- redis::connect(Connection),
      redis::set(Connection, session:xyz, 'user_data', _),
      redis::expire(Connection, session:xyz, 3600, _), % Expire in 1 hour
      redis::ttl(Connection, session:xyz, TTL),
      redis::disconnect(Connection).
   TTL = 3600.

   % Using send/3 for commands without wrappers
   ?- redis::connect(Connection),
      redis::send(Connection, info(server), Reply),
      redis::disconnect(Connection).

Credits
-------

This library is inspired by the Sean Charles GNU Prolog Redis client
library.

Known issues
------------

Recent versions of macOS seem to disable the mapping of ``localhost`` to
``127.0.0.1``. This issue may prevent running this library unit tests
and the ``redis::connect/1`` predicate from working. This can be fixed
either by editing the ``/etc/hosts`` file or by using in alternative the
predicate ``redis::connect/3`` with ``'127.0.0.1'`` as the first
argument.
