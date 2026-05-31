.. _library_rest:

``rest``
========

The ``rest`` library provides a small REST authoring layer on top of the
existing ``http_router`` library. It is implemented as a category so
that REST application objects can continue to implement the
``http_handler_protocol`` protocol directly while reusing the existing
router dispatch, request annotation, middleware, and OpenAPI derivation
logic.

This library reuses the ``http`` and ``http_router``. It derives router
hooks from higher-level endpoint descriptors, provides small request and
response helpers, and normalizes simple action result terms into HTTP
responses.

Layering
--------

``rest`` is the highest-level authoring layer in the current HTTP stack:

- Start with `http <../http/NOTES.md>`__ for normalized HTTP messages
  and with `http_router <../http_router/NOTES.md>`__ for declarative
  routing.
- Use ``rest`` when you want to describe endpoints with ``endpoint/5``,
  keep the inherited ``handle/2`` entry point, and return small result
  terms instead of building every response manually.
- Stay with `http_router <../http_router/NOTES.md>`__ when you need
  direct control over ``route/4``, custom handler predicates, or router
  hooks without the higher-level endpoint layer.
- Pair REST applications with `open_api <../open_api/NOTES.md>`__ when
  you want derived OpenAPI documents or opt-in request and response
  contract validation.

API documentation
-----------------

Open the
`../../apis/library_index.html#rest <../../apis/library_index.html#rest>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(rest(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(rest(tester)).

Current scope
-------------

The current slice provides one imported routing predicate through the
category inheritance from ``http_router`` plus request and response
helper predicates:

- ``handle/2``
- ``path_parameter/3``
- ``query_parameter/3``
- ``request_header/3``
- ``request_body/2``
- ``json_body/2``
- ``form_body/2``
- ``text_body/2``
- ``binary_body/2``
- ``json_response/4-5``
- ``created_response/4``
- ``no_content_response/2``
- ``problem_response/6``

The category also exposes optional OpenAPI contract validation hooks:

- ``open_api_validate_request/1``
- ``open_api_request_validation_error_response/4``
- ``open_api_validate_response/1``
- ``open_api_response_validation_error_response/5``

Importing REST application objects are expected to define endpoint
descriptors using:

- ``endpoint(Id, Method, Path, Action, Options)``

Endpoint identifiers must be unique within the importing object. When
the object also exposes OpenAPI operations through ``http_router``,
those same identifiers are reused as operation identifiers.

The ``Action`` argument is the name of a declared local predicate with
arity 2, typically a protected predicate. The action predicate receives
the annotated request and returns one of the currently supported result
terms:

- ``ok(JSON)``
- ``created(Location, JSON)``
- ``no_content``
- ``json(Status, JSON)``
- ``json(Status, Headers, JSON)``
- ``problem(Status, Type, Title, Detail)``
- ``response(Response)`` or a normalized ``response/5`` term directly

The category derives the ``http_router`` hooks from those endpoint
descriptors:

- ``route/4``
- ``route_metadata/2``
- ``route_produces/2``

Endpoint options are passed through as route metadata except for the
special ``produces(MediaTypes)`` option, which is translated into
``route_produces/2``. This keeps endpoint metadata available to action
predicates and response middleware and also allows router-level OpenAPI
derivation to keep working.

The current helper predicates read the normalized HTTP request terms
provided by ``http``:

::

   request(Method, Target, Version, Headers, Body, Properties)

They rely on router annotations such as ``route/1`` and
``path_params/1`` plus the normalized derived properties produced by the
``http`` library such as ``query_pairs/1``. The ``path_parameter/3`` and
``query_parameter/3`` helpers are deterministic lookups. The decoded
body helpers return ``400 Bad Request`` problem responses when the
current action expects a JSON, form, text, or binary body and the
normalized request body is missing or has a different decoded shape.

Action result normalization currently builds normalized HTTP response
terms using these rules:

- successful JSON responses use the negotiated router media type when
  the request carries a ``response_media_type/1`` annotation and
  otherwise fall back to ``application/json``
- problem responses always use ``application/problem+json``

When ``open_api_validate_request/1`` succeeds for a matched endpoint,
the category validates the normalized request against the derived
OpenAPI operation descriptor before calling the endpoint action. When
``open_api_validate_response/1`` succeeds for a matched endpoint, the
category validates the normalized response after action result
normalization.

By default, request validation failures preserve three classes of client
error: unsupported request media types return
``415 Unsupported Media Type``, schema-invalid request bodies return
``422 Unprocessable Content``, and other request validation failures
return ``400 Bad Request``. Response validation failures still return a
``500 Internal Server Error`` ``application/problem+json`` response.
Importing objects can override those defaults using the corresponding
validation error response hooks. Validation is skipped for the synthetic
router requests used internally for OpenAPI request and response
inference.

OpenAPI integration
-------------------

When an importing object also implements the
``open_api_provider_protocol`` protocol, the inherited ``http_router``
OpenAPI derivation continues to work. Endpoint options such as
``summary/1``, ``description/1``, ``tags/1``, ``deprecated/1``,
``security/1``, ``parameters/1``, ``request_body/1``, and
``responses/1`` are exposed through ``route_metadata/2``, while
``produces(MediaTypes)`` drives ``route_produces/2`` and therefore
response media type inference.

Current limitations
-------------------

- request and response OpenAPI contract validation is currently opt-in
  per endpoint using the validation hooks
- the current slice does not yet include authentication, authorization,
  pagination, filtering, or rate-limiting helpers
- result normalization currently focuses on JSON and problem responses;
  it does not yet provide specialized helpers for redirects, streaming,
  or multipart payloads
- action failures are currently mapped to generic
  ``500 Internal Server Error`` problem responses unless the action
  throws a ``problem/4`` term explicitly
