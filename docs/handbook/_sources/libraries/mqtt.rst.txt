.. _library_mqtt:

``mqtt``
========

Portable MQTT 5.0 client implementation. This initial version provides
the client-side MQTT transport and packet handling using objects that
implement the ``http_transport_protocol`` protocol. It supports MQTT and
MQTT-over-TLS connections, performs the MQTT 5 CONNECT/CONNACK exchange,
provides packet encoding and decoding predicates, and implements
synchronous publish, subscribe, unsubscribe, receive, and ping
operations.

API documentation
-----------------

Open the
`../../apis/library_index.html#mqtt <../../apis/library_index.html#mqtt>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(mqtt(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(mqtt(tester)).

Protocol Version
----------------

This library targets the MQTT 5.0 specification:

https://docs.oasis-open.org/mqtt/mqtt/v5.0/mqtt-v5.0.html

Transport Selection
-------------------

The ``connect/3`` predicate accepts ``mqtt://Host[:Port]`` and
``mqtts://Host[:Port]`` addresses. The default port is ``1883`` for
``mqtt`` and ``8883`` for ``mqtts``.

The ``transport(default)`` option follows the HTTP libraries design:

- ``mqtt`` uses ``http_socket_transport``.
- ``mqtts`` uses ``http_process_transport``.
- ``mqtts`` adds ``connection_transport(tls)`` to connection options
  unless a connection transport is already specified explicitly.

Applications can pass any object implementing
``http_transport_protocol`` using the ``transport/1`` option.

Current Status
--------------

This library currently provides a client-only MQTT 5 API. The
``connect/3`` and ``connect/4`` predicates open a transport connection
and wait for a successful CONNACK packet before returning. A non-success
CONNACK reason code closes the transport connection and throws an error.

The ``publish/4``, ``subscribe/4``, and ``unsubscribe/4`` predicates are
synchronous:

- QoS 0 ``publish/4`` writes the PUBLISH packet and returns.
- QoS 1 ``publish/4`` waits for the matching PUBACK packet.
- QoS 2 ``publish/4`` waits for PUBREC, sends PUBREL, and waits for
  PUBCOMP.
- ``subscribe/4`` waits for the matching SUBACK packet and returns its
  reason codes.
- ``unsubscribe/4`` waits for the matching UNSUBACK packet and returns
  its reason codes.

The ``send_pingreq/1`` predicate sends a PINGREQ packet. The ``ping/2``
predicate sends PINGREQ and waits synchronously for PINGRESP. The
``receive/3`` predicate reads and decodes the next MQTT packet.

The ``disconnect/2`` predicate sends a normal DISCONNECT packet and then
closes the transport connection.

The public ``encode_packet/2`` and ``decode_packet/2`` predicates
support the current normalized packet terms for CONNECT, CONNACK,
PUBLISH, PUBACK, PUBREC, PUBREL, PUBCOMP, SUBSCRIBE, SUBACK,
UNSUBSCRIBE, UNSUBACK, PINGREQ, PINGRESP, DISCONNECT, and AUTH packets.

Limitations
-----------

The implementation is intentionally synchronous and client-only. It does
not run a background receive loop, does not maintain an incoming message
queue, and does not automatically send keep-alive pings. Applications
should call ``receive/3``, ``send_pingreq/1``, or ``ping/2`` explicitly
as needed.

MQTT 5 property encoding and decoding supports all MQTT 5 property
identifiers using normalized property terms in packet ``properties/1``
fields. This includes user properties, authentication method and data,
topic aliases, subscription identifiers, response topics, correlation
data, payload format indicators, message expiry, session expiry, and
receive maximum values. Property order is preserved, and repeated user
properties are supported.

The CONNECT packet currently supports client identifier, clean start,
keep alive, and properties. Will messages, username/password
authentication, enhanced authentication flow, topic alias state
management, property context validation, duplicate single-use property
validation, and session state management are not yet implemented.

QoS acknowledgement handling checks the expected packet type and packet
identifier. It does not maintain persistent in-flight state across calls
or recover QoS exchanges after a connection loss.

Packet payloads and binary fields are represented as lists of byte
values. MQTT UTF-8 strings are represented as atoms and are checked for
null characters; full Unicode well-formedness checks beyond the backend
atom/code conversion are not currently implemented.
