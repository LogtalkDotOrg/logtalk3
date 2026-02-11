.. _library_stomp:

``stomp``
=========

Portable STOMP 1.2 (Simple Text Orientated Messaging Protocol) client
implementation. This library uses the ``sockets`` library and supports
all backend Prolog systems supported by that library: ECLiPSe, GNU
Prolog, SICStus Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Protocol Version
----------------

This library implements the STOMP 1.2 protocol specification:

https://stomp.github.io/stomp-specification-1.2.html

STOMP 1.2 is backwards compatible with STOMP 1.1 with minor differences
in frame line endings and message acknowledgment.

Features
--------

- Full STOMP 1.2 protocol support
- Heartbeat negotiation and automatic heartbeat handling
- Multiple concurrent subscriptions with unique IDs
- All acknowledgment modes: ``auto``, ``client``, ``client-individual``
- Transaction support: ``BEGIN``, ``COMMIT``, ``ABORT``
- Graceful disconnect with receipt confirmation
- Proper frame encoding/decoding with header escaping
- User-defined headers for messages

API documentation
-----------------

Open the
`../../apis/library_index.html#stomp <../../apis/library_index.html#stomp>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(stomp(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(stomp(tester)).

Note: Integration tests require a running STOMP server (e.g., RabbitMQ
with the STOMP plugin, ActiveMQ, or Apache Apollo). They are usually run
using RabbitMQ with the STOMP plugin. As RabbitMQ uses ``/`` as its
default virtual host, the tests assume this and use the ``host('/')``
option when calling the ``connect/4`` predicate (otherwise the
connection would fail with a
``"Virtual host 'localhost' access denied"`` error message). To enable
the RabbitMQ's STOMP plugin:

::

   $ rabbitmq-plugins enable rabbitmq_stomp

RabbitMQ STOMP listens on port 61613 by default.

Usage
-----

Connecting to a STOMP server
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Basic connection assuming RabbitMQ, which uses ``host('/')`` as shown:

::

   ?- stomp::connect(localhost, 61613, Connection, [host('/')]).

Connection with authentication:

::

   ?- stomp::connect(localhost, 61613, Connection, [
          login('user'),
          passcode('password')
      ]).

Connection with heartbeat:

::

   ?- stomp::connect(localhost, 61613, Connection, [
          heartbeat(10000, 10000)
      ]).

Connection with virtual host:

::

   ?- stomp::connect(localhost, 61613, Connection, [
          host('/my-vhost')
      ]).

Sending messages
~~~~~~~~~~~~~~~~

Send a simple text message:

::

   ?- stomp::send(Connection, '/queue/test', 'Hello, World!', []).

Send a message with content type:

::

   ?- stomp::send(Connection, '/queue/test', '{"key":"value"}', [
          content_type('application/json')
      ]).

Send with custom headers:

::

   ?- stomp::send(Connection, '/queue/test', 'Message', [
          header('priority', '9'),
          header('persistent', 'true')
      ]).

Send within a transaction:

::

   ?- stomp::begin_transaction(Connection, 'tx-001', []),
      stomp::send(Connection, '/queue/test', 'Message 1', [transaction('tx-001')]),
      stomp::send(Connection, '/queue/test', 'Message 2', [transaction('tx-001')]),
      stomp::commit_transaction(Connection, 'tx-001', []).

Subscribing to destinations
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Subscribe with auto acknowledgment (default):

::

   ?- stomp::subscribe(Connection, '/queue/test', 'sub-0', []).

Subscribe with client acknowledgment:

::

   ?- stomp::subscribe(Connection, '/queue/test', 'sub-0', [
          ack(client)
      ]).

Subscribe with client-individual acknowledgment:

::

   ?- stomp::subscribe(Connection, '/topic/events', 'sub-1', [
          ack(client_individual)
      ]).

Receiving messages
~~~~~~~~~~~~~~~~~~

Poll for a message (non-blocking):

::

   ?- stomp::receive(Connection, Frame, [timeout(0)]).

Wait for a message with timeout (milliseconds):

::

   ?- stomp::receive(Connection, Frame, [timeout(5000)]).

Process received MESSAGE frame:

::

   ?- stomp::receive(Connection, Frame, []),
      stomp::frame_command(Frame, 'MESSAGE'),
      stomp::frame_header(Frame, 'destination', Destination),
      stomp::frame_header(Frame, 'message-id', MessageId),
      stomp::frame_body(Frame, Body).

Acknowledging messages
~~~~~~~~~~~~~~~~~~~~~~

For ``client`` or ``client-individual`` acknowledgment modes:

::

   ?- stomp::receive(Connection, Frame, []),
      stomp::frame_header(Frame, 'ack', AckId),
      stomp::ack(Connection, AckId, []).

Negative acknowledgment:

::

   ?- stomp::nack(Connection, AckId, []).

Acknowledgment within a transaction:

::

   ?- stomp::ack(Connection, AckId, [transaction('tx-001')]).

Unsubscribing
~~~~~~~~~~~~~

::

   ?- stomp::unsubscribe(Connection, 'sub-0', []).

Transactions
~~~~~~~~~~~~

Begin a transaction:

::

   ?- stomp::begin_transaction(Connection, 'tx-001', []).

Commit a transaction:

::

   ?- stomp::commit_transaction(Connection, 'tx-001', []).

Abort a transaction:

::

   ?- stomp::abort_transaction(Connection, 'tx-001', []).

Disconnecting
~~~~~~~~~~~~~

Graceful disconnect with receipt confirmation:

::

   ?- stomp::disconnect(Connection, []).

Heartbeat handling
~~~~~~~~~~~~~~~~~~

Send a heartbeat (if needed manually):

::

   ?- stomp::send_heartbeat(Connection).

Check connection health:

::

   ?- stomp::connection_alive(Connection).

Working with frames
~~~~~~~~~~~~~~~~~~~

Get frame command:

::

   ?- stomp::frame_command(Frame, Command).

Get frame header value:

::

   ?- stomp::frame_header(Frame, HeaderName, Value).

Get frame body:

::

   ?- stomp::frame_body(Frame, Body).

Get all frame headers:

::

   ?- stomp::frame_headers(Frame, Headers).

API Summary
-----------

Connection management
~~~~~~~~~~~~~~~~~~~~~

- ``connect(+Host, +Port, -Connection, +Options)`` - Connect to server
- ``disconnect(+Connection, +Options)`` - Disconnect from server
- ``connection_alive(+Connection)`` - Check if connection is alive

Messaging
~~~~~~~~~

- ``send(+Connection, +Destination, +Body, +Options)`` - Send message
- ``subscribe(+Connection, +Destination, +Id, +Options)`` - Subscribe
- ``unsubscribe(+Connection, +Id, +Options)`` - Unsubscribe
- ``receive(+Connection, -Frame, +Options)`` - Receive frame

Acknowledgment
~~~~~~~~~~~~~~

- ``ack(+Connection, +Id, +Options)`` - Acknowledge message
- ``nack(+Connection, +Id, +Options)`` - Negative acknowledge

.. _transactions-1:

Transactions
~~~~~~~~~~~~

- ``begin_transaction(+Connection, +TransactionId, +Options)`` - Begin
- ``commit_transaction(+Connection, +TransactionId, +Options)`` - Commit
- ``abort_transaction(+Connection, +TransactionId, +Options)`` - Abort

Heartbeat
~~~~~~~~~

- ``send_heartbeat(+Connection)`` - Send heartbeat EOL

Frame inspection
~~~~~~~~~~~~~~~~

- ``frame_command(+Frame, -Command)`` - Get frame command
- ``frame_header(+Frame, +Name, -Value)`` - Get header value
- ``frame_headers(+Frame, -Headers)`` - Get all headers
- ``frame_body(+Frame, -Body)`` - Get frame body

Connection Options
------------------

- ``login(Login)`` - Username for authentication
- ``passcode(Passcode)`` - Password for authentication
- ``host(VirtualHost)`` - Virtual host name (default: from Host
  parameter)
- ``heartbeat(ClientMs, ServerMs)`` - Heartbeat timing in milliseconds

Send Options
------------

- ``content_type(MimeType)`` - MIME type of the body
- ``content_length(Length)`` - Byte length of body (auto-calculated if
  omitted)
- ``transaction(TransactionId)`` - Include in transaction
- ``receipt(ReceiptId)`` - Request receipt from server
- ``header(Name, Value)`` - Add custom header

Subscribe Options
-----------------

- ``ack(Mode)`` - Acknowledgment mode: ``auto``, ``client``, or
  ``client_individual``

Receive Options
---------------

- ``timeout(Milliseconds)`` - Wait timeout; 0 for non-blocking, -1 for
  infinite

Error Handling
--------------

Most predicates throw ``error(stomp_error(Reason), Context)`` on
failure. Common error reasons:

- ``connection_failed`` - Could not establish TCP connection
- ``protocol_error(Message)`` - Server sent ERROR frame
- ``not_connected`` - Connection was closed
- ``invalid_frame`` - Malformed frame received
- ``timeout`` - Operation timed out
