.. _library_http_digest:

``http_digest``
===============

This library provides HTTP Digest authentication helpers on top of the
normalized HTTP stack.

The library provides these public entities:

- ``http_digest_verifier_protocol`` server-side verifier protocol for
  looking up stored Digest HA1 values
- ``http_digest`` core Digest parsing, generation, request
  authorization, request protection, challenge building, and
  ``Authentication-Info`` response decoration
- ``http_server_digest_handler(_, _, _, _)`` portable handler wrapper
  that applies Digest protection and response decoration around another
  object implementing ``http_handler_protocol``
- ``http_router_digest_auth(_, _, _)`` router companion category that
  protects routes declaring ``digest_auth/1`` metadata and decorates
  successful protected responses with ``Authentication-Info``
- ``http_client_digest_session`` stateful client helper that preserves
  cookies and retries once when the server replies with a ``401`` Digest
  challenge

The library design keeps the Digest core object stateless and
deterministic. State is kept only where it is operationally useful: in
verifier objects supplied by applications and in the optional
client-side session helper used for cookie storage.

The server-side entities are portable across the backends supported by
the normalized HTTP library. The ``http_client_digest_session`` helper
additionally depends on the socket-backed HTTP client stack and is
therefore available on the same backends supported by the
``http_client`` and ``http_socket`` libraries.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_digest <../../apis/library_index.html#http_digest>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_digest(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_digest(tester)).

Usage overview
--------------

Use ``http_digest`` directly when you need explicit control over Digest
challenge parsing and request verification:

::

   | ?- http_digest::unauthorized_response(Challenge, Response, [realm('private'), nonce_secret('secret')]).

Wrap a normal handler with ``http_server_digest_handler(_, _, _, _)``
when you want a portable middleware-style integration point for Digest
verification:

::

   | ?- Handler = http_server_digest_handler(verifier, app_handler, [realm('private'), nonce_secret('secret')], []).

Use ``http_router_digest_auth(_, _, _)`` in router objects importing
``http_router`` when you want per-route protection driven by normal
route metadata:

::

   authorize_routed_request(Request, Action) :-
       ^^authorize_digest_auth_request(Request, Action).

   response_middleware(digest_authentication_info, add_digest_authentication_info).

   route_metadata(show_secret, [digest_auth([])]).

Use ``http_client_digest_session`` when you need cookie persistence plus
automatic retry after a ``401`` Digest challenge:

::

   | ?- http_client_digest_session::open(Session, 'Mufasa', 'Circle Of Life'),
        http_client_digest_session::get(Session, 'http://127.0.0.1:8080/protected', Response, []),
        http_client_digest_session::close(Session).

The current client helper is reactive: it sends the initial request
without an ``Authorization`` header and retries automatically only after
receiving a Digest challenge. This keeps the helper small and avoids
maintaining speculative challenge caches inside the client session
state.

For ``add_authentication_info/4``, the ``nextnonce`` option accepts
three forms:

- ``nextnonce(false)`` omits the ``nextnonce`` field
- ``nextnonce(true)`` generates a fresh nonce using ``nonce_secret/1``
- ``nextnonce(Nonce)`` emits the explicit nonce atom verbatim

Current scope
-------------

- deterministic Digest header parsing and generation for challenges,
  authorizations, and ``Authentication-Info``
- server-side request protection through explicit verifier objects
- middleware-style handler wrapping for server integration
- route-level protection and successful-response ``Authentication-Info``
  decoration through ``http_router_digest_auth(_, _, _)`` plus the
  ``authorize_routed_request/2`` and ``response_middleware/2`` router
  hooks
- client-side Digest retry on top of the existing ``http://``
  socket-backed transport and cookie-jar support
- MD5, SHA-256, and SHA-512-256 based Digest computations through the
  ``hashes`` and ``hmac`` libraries; when no algorithm option is
  provided, the default challenge algorithm remains ``sha256``

Non-implemented features
------------------------

- HTTPS transport support in the client helper
- proactive client-side challenge caching across requests
- shared or persistent nonce replay stores
- ``auth-int`` request-body hashing support
