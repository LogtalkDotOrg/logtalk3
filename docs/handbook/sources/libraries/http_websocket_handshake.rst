.. _library_http_websocket_handshake:

``http_websocket_handshake``
============================

This library provides lower-level opening-handshake predicates in the
``http_websocket_handshake`` object that is used by other libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_websocket_handshake <../../apis/library_index.html#http_websocket_handshake>`__
link in a web browser.

Loading
-------

To load the full library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_websocket_handshake(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_websocket_handshake(tester)).

Current scope
-------------

The current implementation provides two predicates:

- ``websocket_opening_key/1`` for generating canonical base64-encoded
  opening keys.
- ``websocket_accept/2`` for computing canonical
  ``Sec-WebSocket-Accept`` values from valid opening keys.

Current workflow
----------------

Use the ``websocket_opening_key/1`` and ``websocket_accept/2``
predicates only when you need the raw opening-handshake values without
opening a connection.
