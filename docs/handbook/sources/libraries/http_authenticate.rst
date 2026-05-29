.. _library_http_authenticate:

``http_authenticate``
=====================

This library provides HTTP Basic authentication helpers on top of the
normalized HTTP stack.

The library currently provides these public entities:

- ``http_authenticate_verifier_protocol`` server-side verifier protocol
  for checking plaintext credentials
- ``http_authenticate`` core Basic parsing, generation, request
  protection, and challenge helpers
- ``http_htpasswd_verifier(_)`` portable Apache ``.htpasswd`` subset
  verifier supporting ``{SHA}`` entries
- ``http_server_basic_handler(_, _, _)`` portable handler wrapper that
  applies Basic protection around another object implementing
  ``http_handler_protocol``
- ``http_router_basic_auth(_, _)`` router companion category that
  protects routes declaring ``basic_auth/1`` metadata

API documentation
-----------------

Open the
`../../apis/library_index.html#http_authenticate <../../apis/library_index.html#http_authenticate>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_authenticate(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_authenticate(tester)).

Usage overview
--------------

Use ``http_authenticate`` directly when you need explicit control over
Basic challenge parsing and request verification:

::

   | ?- http_authenticate::protect_request(Request, verifier, Action, [realm('private')]).

Wrap a normal handler with ``http_server_basic_handler(_, _, _)`` when
you want a portable middleware-style integration point for Basic
verification:

::

   | ?- Handler = http_server_basic_handler(verifier, app_handler, [realm('private')]).

Use ``http_router_basic_auth(_, _)`` in router objects importing
``http_router`` when you want per-route protection driven by normal
route metadata:

::

   authorize_routed_request(Request, Action) :-
       ^^authorize_basic_auth_request(Request, Action).

   route_metadata(show_secret, [basic_auth([])]).

Current scope
-------------

- deterministic Basic header parsing and generation for
  ``Authorization`` and ``WWW-Authenticate``
- server-side request protection through explicit verifier objects with
  malformed or missing client credentials mapped to ``401 Unauthorized``
  responses while verifier and configuration errors are rethrown
- middleware-style handler wrapping for generic
  ``http_handler_protocol`` objects
- route-level protection through ``http_router_basic_auth(_, _)`` and
  the ``authorize_routed_request/2`` router hook
- portable Apache ``.htpasswd`` subset verification for ``{SHA}``
  entries using expanded file paths and rejecting malformed decoded
  SHA-1 digests

Option validation
-----------------

- ``protect_request/4`` and ``unauthorized_response/3-4`` validate
  option values at the call boundary using the shared ``options``
  category support
- ``unauthorized_response/4`` only accepts overlay options for
  ``status/1``, ``headers/1``, ``body/1``, and ``properties/1``
- unauthorized-response ``status/1`` values remain limited to ``401``,
  but the reason phrase follows the shared HTTP ``text`` rules instead
  of requiring an atom representation
- invalid response customization ``headers/1`` and ``body/1`` values are
  rejected before any normalized HTTP response term is constructed

Non-implemented features
------------------------

- client-side session helpers
- automatic role or scope authorization metadata
- portable verification of ``$apr1$``, bcrypt (``$2a$``, ``$2b$``,
  ``$2y$``), or traditional ``crypt()`` ``.htpasswd`` entries
