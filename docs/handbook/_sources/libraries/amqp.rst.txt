.. _library_amqp:

``amqp``
========

Portable AMQP 0-9-1 (Advanced Message Queuing Protocol) client
implementation. This library uses the ``sockets`` library and supports
all backend Prolog systems supported by that library: ECLiPSe, GNU
Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Protocol Version
----------------

This library implements the AMQP 0-9-1 protocol specification:

https://www.rabbitmq.com/amqp-0-9-1-reference.html

AMQP 0-9-1 is a binary wire-level protocol for message-oriented
middleware. It is the de facto standard for message broker
interoperability and is supported by RabbitMQ, Apache Qpid, Apache
ActiveMQ, and other brokers.

Features
--------

- Full AMQP 0-9-1 protocol support with binary frame encoding/decoding
- Connection management with heartbeat negotiation
- Automatic reconnection with configurable retry attempts and delays
- Connection pooling with automatic connection management
- Multiple concurrent channels over a single connection
- Exchange operations: declare, delete, bind, unbind
- Queue operations: declare, delete, bind, unbind, purge
- Basic messaging: publish, consume, get, ack, nack, reject
- Quality of service (QoS) with prefetch settings
- Transaction support: tx.select, tx.commit, tx.rollback
- Publisher confirms (RabbitMQ extension)
- SASL PLAIN authentication
- Custom message properties and headers
- Content properties: content-type, delivery-mode, priority, etc.

API documentation
-----------------

Open the
`../../apis/library_index.html#amqp <../../apis/library_index.html#amqp>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(amqp(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(amqp(tester)).

Note: Integration tests require a running AMQP 0-9-1 server (e.g.,
RabbitMQ). RabbitMQ AMQP listens on port 5672 by default with
guest/guest credentials.

Usage
-----

Connecting to an AMQP server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Basic connection to RabbitMQ with default settings:

::

   ?- amqp::connect(localhost, 5672, Connection, []).

Connection with custom credentials and virtual host:

::

   ?- amqp::connect(localhost, 5672, Connection, [
       username('myuser'),
       password('mypassword'),
       virtual_host('/myvhost'),
       heartbeat(30)
   ]).

Connection with automatic reconnection enabled:

::

   ?- amqp::connect(localhost, 5672, Connection, [
       reconnect(true),
       reconnect_attempts(5),
       reconnect_delay(2)
   ]).

This will attempt to connect up to 5 times with a 2 second delay between
attempts. If all attempts fail, an ``amqp_error(reconnect_failed)``
error is thrown. The reconnection options are:

- ``reconnect(Boolean)`` - Enable automatic reconnection (default:
  ``false``)
- ``reconnect_attempts(N)`` - Maximum number of connection attempts
  (default: ``3``)
- ``reconnect_delay(Seconds)`` - Delay between attempts in seconds
  (default: ``1``)

Opening a channel
~~~~~~~~~~~~~~~~~

AMQP operations require a channel. Open one on the connection:

::

   ?- amqp::channel_open(Connection, 1, Channel).

You can open multiple channels (with different numbers) on a single
connection.

Declaring exchanges and queues
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Declare a direct exchange:

::

   ?- amqp::exchange_declare(Channel, 'my.exchange', [
       type(direct),
       durable(true)
   ]).

Declare a queue:

::

   ?- amqp::queue_declare(Channel, 'my.queue', [
       durable(true),
       auto_delete(false)
   ]).

Declare a queue with server-generated name:

::

   ?- amqp::queue_declare(Channel, Queue, [exclusive(true)]).
   % Queue will be unified with the generated name

Bind a queue to an exchange:

::

   ?- amqp::queue_bind(Channel, 'my.queue', 'my.exchange', [
       routing_key('my.routing.key')
   ]).

Publishing messages
~~~~~~~~~~~~~~~~~~~

Publish a simple message:

::

   ?- amqp::basic_publish(Channel, 'my.exchange', 'Hello, World!', [
       routing_key('my.routing.key')
   ]).

Publish with properties:

::

   ?- amqp::basic_publish(Channel, 'my.exchange', '{"data": "json"}', [
       routing_key('my.routing.key'),
       content_type('application/json'),
       delivery_mode(2),  % persistent
       correlation_id('abc123'),
       reply_to('reply.queue')
   ]).

Publish with custom headers:

::

   ?- amqp::basic_publish(Channel, 'my.exchange', 'Message', [
       routing_key('my.key'),
       headers([
           'x-custom-header'-longstr('value'),
           'x-priority'-int(5)
       ])
   ]).

Consuming messages
~~~~~~~~~~~~~~~~~~

Start a consumer:

::

   ?- amqp::basic_consume(Channel, 'my.queue', [
       consumer_tag('my-consumer'),
       no_ack(false)
   ]).

Receive messages:

::

   ?- amqp::receive(Channel, Message, [timeout(5000)]).

Extract message data:

::

   ?- amqp::message_body(Message, Body),
      amqp::message_delivery_tag(Message, DeliveryTag),
      amqp::message_property(Message, content_type, ContentType).

Acknowledge a message:

::

   ?- amqp::basic_ack(Channel, DeliveryTag, []).

Reject a message:

::

   ?- amqp::basic_reject(Channel, DeliveryTag, [requeue(true)]).

Synchronous get
~~~~~~~~~~~~~~~

Get a single message synchronously:

::

   ?- amqp::basic_get(Channel, 'my.queue', [no_ack(false)]).

Quality of Service
~~~~~~~~~~~~~~~~~~

Set prefetch count to limit unacknowledged messages:

::

   ?- amqp::basic_qos(Channel, [prefetch_count(10)]).

Transactions
~~~~~~~~~~~~

Enable transactions on a channel:

::

   ?- amqp::tx_select(Channel).

Publish messages within a transaction:

::

   ?- amqp::basic_publish(Channel, 'exchange', 'Msg1', [routing_key('key')]),
      amqp::basic_publish(Channel, 'exchange', 'Msg2', [routing_key('key')]),
      amqp::tx_commit(Channel).

Rollback a transaction:

::

   ?- amqp::tx_rollback(Channel).

Publisher Confirms (RabbitMQ extension)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Enable publisher confirms:

::

   ?- amqp::confirm_select(Channel).

Closing connections
~~~~~~~~~~~~~~~~~~~

Close a channel:

::

   ?- amqp::channel_close(Channel).

Close the connection:

::

   ?- amqp::close(Connection).

Connection Pooling
------------------

The library provides connection pooling through the ``amqp_pool``
category. To create a connection pool, define an object that imports
this category:

Defining a pool
~~~~~~~~~~~~~~~

::

   :- object(my_pool,
       imports(amqp_pool)).
   :- end_object.

Initializing the pool
~~~~~~~~~~~~~~~~~~~~~

Initialize the pool with configuration options:

::

   ?- my_pool::initialize([
       host(localhost),
       port(5672),
       min_size(2),
       max_size(10),
       connection_options([
           username('guest'),
           password('guest'),
           virtual_host('/')
       ])
   ]).

Pool configuration options:

- ``host(Host)`` - AMQP server hostname (default: ``localhost``)
- ``port(Port)`` - AMQP server port (default: ``5672``)
- ``min_size(N)`` - Minimum connections to maintain (default: ``1``)
- ``max_size(N)`` - Maximum connections allowed (default: ``10``)
- ``connection_options(Options)`` - Options passed to
  ``amqp::connect/4`` (default: ``[]``)

Acquiring and releasing connections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Manually acquire and release connections:

::

   ?- my_pool::acquire(Connection),
      amqp::channel_open(Connection, 1, Channel),
      % ... use the channel ...
      amqp::channel_close(Channel),
      my_pool::release(Connection).

Using with_connection/1
~~~~~~~~~~~~~~~~~~~~~~~

Use ``with_connection/1`` for automatic connection management:

::

   ?- my_pool::with_connection(do_work).

   do_work(Connection) :-
       amqp::channel_open(Connection, 1, Channel),
       amqp::basic_publish(Channel, '', 'Hello!', [routing_key('my.queue')]),
       amqp::channel_close(Channel).

The connection is automatically released even if the goal fails or
throws an exception.

Pool statistics
~~~~~~~~~~~~~~~

Get pool statistics:

::

   ?- my_pool::stats(stats(Available, InUse, Total, MinSize, MaxSize)).

Resizing the pool
~~~~~~~~~~~~~~~~~

Resize the pool at runtime:

::

   ?- my_pool::resize(5, 20).

Destroying the pool
~~~~~~~~~~~~~~~~~~~

Close all connections and clear pool state:

::

   ?- my_pool::destroy.

Creating pools dynamically
~~~~~~~~~~~~~~~~~~~~~~~~~~

Pools can also be created at runtime using ``create_object/4``:

::

   ?- create_object(dynamic_pool, [imports(amqp_pool)], [], []),
      dynamic_pool::initialize([host(localhost), port(5672)]).

Binary Frame Encoding
---------------------

AMQP 0-9-1 is a binary protocol. The library provides low-level encoding
and decoding predicates for working with raw frames:

Frame structure
~~~~~~~~~~~~~~~

An AMQP frame consists of:

- Type (1 byte): 1=method, 2=header, 3=body, 8=heartbeat
- Channel (2 bytes): Channel number (0 for connection frames)
- Size (4 bytes): Payload size
- Payload (Size bytes): Frame-specific data
- Frame end (1 byte): 0xCE marker

Encoding/decoding frames
~~~~~~~~~~~~~~~~~~~~~~~~

Encode a frame to bytes:

::

   ?- amqp::encode_frame(Frame, Bytes).

Decode bytes to a frame:

::

   ?- amqp::decode_frame(Bytes, Frame).

Data Types
----------

The library handles AMQP data types automatically:

- **octet**: 8-bit unsigned integer
- **short**: 16-bit unsigned integer (big-endian)
- **long**: 32-bit unsigned integer (big-endian)
- **longlong**: 64-bit unsigned integer (big-endian)
- **shortstr**: Short string (length <= 255)
- **longstr**: Long string
- **table**: Field table (key-value pairs)
- **array**: Field array

Field values in tables use type tags:

- ``bool(true/false)`` - Boolean
- ``byte(V)`` - Signed 8-bit
- ``short(V)`` - Signed 16-bit
- ``int(V)`` - Signed 32-bit
- ``long(V)`` - Signed 64-bit
- ``float(V)`` - 32-bit float
- ``double(V)`` - 64-bit float
- ``longstr(V)`` - Long string
- ``table(Pairs)`` - Nested table
- ``array(Values)`` - Array
- ``timestamp(V)`` - Timestamp
- ``void`` - No value

Error Handling
--------------

The library throws structured errors:

- ``amqp_error(connection_failed)`` - TCP connection failed
- ``amqp_error(auth_failed)`` - Authentication failed
- ``amqp_error(protocol_error(Msg))`` - Protocol violation
- ``amqp_error(channel_error(Msg))`` - Channel-level error
- ``amqp_error(exchange_error(Msg))`` - Exchange operation failed
- ``amqp_error(queue_error(Msg))`` - Queue operation failed
- ``amqp_error(basic_error(Msg))`` - Basic operation failed
- ``amqp_error(tx_error(Msg))`` - Transaction error

Comparison with STOMP
---------------------

AMQP 0-9-1 and STOMP are both messaging protocols, but differ
significantly:

============== =============== ================
Feature        AMQP 0-9-1      STOMP
============== =============== ================
Protocol type  Binary          Text
Complexity     High            Low
Exchange types Multiple        Broker-dependent
Routing        Flexible        Simple
Transactions   Native          Native
QoS            Native prefetch Limited
Performance    Higher          Lower
============== =============== ================

Use AMQP when you need:

- Fine-grained routing control
- High performance
- Advanced message patterns
- Exchange-based routing

Use STOMP when you need:

- Simple text-based protocol
- Easy debugging
- Protocol simplicity

Future Work
-----------

Planned enhancements for phase 2:

- Logtalk protocols for messaging patterns (request/reply, pub/sub,
  etc.)
- Categories for common message transformations
- Async message handlers

Known Limitations
-----------------

- SSL/TLS connections not yet supported (use stunnel or similar)
- Heartbeat sending must be done manually via ``send_heartbeat/1``
- Float/double encoding uses simplified representation
- No support for AMQP 1.0 (different protocol)
