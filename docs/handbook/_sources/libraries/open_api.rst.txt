.. _library_open_api:

``open_api``
============

The ``open_api`` library provides a first-pass implementation for
deriving, parsing, generating, and structurally validating OpenAPI 3.1.0
documents. It builds on top of the ``json``, ``json_schema``, and
``application`` libraries.

This initial implementation focuses on deriving OpenAPI documents from
objects implementing the ``open_api_provider_protocol`` protocol using a
small, portable descriptor vocabulary. This provides the following
benefits:

- Earlier validation: malformed descriptors can be rejected by functor
  shape and argument checks before any JSON normalization.
- Better portability: provider objects depend on a Logtalk-level
  vocabulary, not on OpenAPI object field spelling.
- Better reuse: the same descriptors could later feed both OpenAPI
  generation and a future authentication or HTTP integration layer.
- Cleaner provider code: provider objects avoid raw camelCase OpenAPI
  keys and nested curly terms for common security cases.

Layering
--------

``open_api`` is a companion library for the HTTP stack rather than a
transport or dispatch layer:

- Use it standalone when you only need to parse, generate, or
  structurally validate OpenAPI documents.
- Pair it with the ``http_core`` library when you need request or
  response contract validation against normalized HTTP terms or wire
  sources.
- Pair it with ``http_router`` library when router objects expose
  ``open_api_provider_protocol`` metadata derived from ``route/4``,
  ``route_metadata/2``, and ``route_produces/2``.
- Pair it with ``rest`` library when you want the same validation and
  document derivation over higher-level endpoint descriptors built on
  top of ``http_router``.
- Use it directly with custom provider objects when your API description
  source is not a router or REST object.

API documentation
-----------------

Open the
`../../apis/library_index.html#open_api <../../apis/library_index.html#open_api>`__
link in a web browser.

Loading
-------

To load this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(open_api(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(open_api(tester)).

Current scope
-------------

- Derive OpenAPI 3.1.0 documents from provider objects implementing the
  ``open_api_provider_protocol`` protocol
- Construct reusable JSON descriptor terms using helper predicates for
  ``media/2``, ``request_body/3``, and ``response/3``, validating
  JSON-compatible media types, response status keys, and shallow
  schema-term shape, including boolean schemas
- Resolve provider operations by ``operationId`` using ``operation/3``
- Validate normalized HTTP request and response terms, or objects
  implementing the ``http_request_protocol`` and
  ``http_response_protocol`` protocols, against provider operation
  descriptors
- Parse HTTP request and response wire sources through the ``http_core``
  library before validating them against provider operation descriptors
- Parse and generate OpenAPI documents using the repository-standard
  JSON term representation
- Perform structural validation of derived or parsed documents using an
  inline phase-1 JSON Schema
- Reject provider descriptor sets that reuse the same ``operationId``
  atom for more than one operation
- Derive top-level OpenAPI ``security`` defaults from provider
  ``security/1``
- Derive reusable security schemes under ``components.securitySchemes``
  and reject top-level or operation ``security/1`` references to
  undeclared scheme names or undeclared OAuth flow scopes when locally
  available
- Reject malformed reusable security scheme descriptors that omit fixed
  fields required by their ``type``, and reject malformed declared OAuth
  flow objects that omit their required URLs or ``scopes``
- Reject provider-side invalid ``apiKey.in`` values and malformed
  URL-valued security fields such as OAuth token or authorization
  endpoints
- Validate provider server descriptor URLs and reject malformed values
- Accept wildcard response status descriptors such as ``'2XX'``
- Reject provider path parameter descriptors that are not declared in
  the operation path template or that do not use ``Required = true``
- Reject parsed or derived documents whose inline path parameters
  violate the OpenAPI path-parameter invariants
- Accept webhook-only OpenAPI 3.1.0 documents during structural
  validation

Current limitations
-------------------

- Validation is intentionally partial and does not attempt to implement
  the full OpenAPI 3.1 meta-schema. Only selected type-specific
  fixed-field checks, such as those for Security Scheme Objects and
  OAuth Flow Objects, are provided
- The library derives only ``components.schemas`` and
  ``components.securitySchemes`` plus the root ``security`` field
- The library integrates with the ``http_core`` library for request and
  response parsing and for protocol-based access to normalized HTTP
  messages, but it limits itself to contract validation rather than
  transport concerns
- The supported ``security_scheme/2`` descriptor vocabulary is limited
  to the forms summarized below
- Scope validation cross-checks against locally declared OAuth flow
  scopes when they are present in a reusable security scheme. Full
  OpenID Connect discovery-based scope resolution is out of scope
- Query and cookie parameter validation relies on typed request
  properties such as ``query_pairs/1`` and ``cookies/1``; raw
  query-string parsing is intentionally out of scope
- Request and response body validation supports ``json/1``, ``text/1``,
  ``binary/1``, and ``form/1`` payload terms; multipart payload
  validation remains out of scope
- The JSON helper predicates validate media types and response status
  keys eagerly, but schema validation at the helper boundary is
  intentionally shallow and only checks for boolean schemas, JSON object
  terms, or ``schema_ref/1`` references

Provider descriptors
--------------------

API metadata
~~~~~~~~~~~~

The ``api_info/1`` predicate returns an
``info(Title, Version, Summary, Properties)`` descriptor for the
top-level OpenAPI ``info`` object. Currently recognized metadata
properties are limited to ``description(Description)``.

Provider objects that also implement ``application_protocol`` can
contribute missing OpenAPI metadata:

- ``description/1`` maps to ``info.description``
- ``license/1`` maps to ``info.license.name``
- ``homepage/1`` maps to the top-level ``externalDocs.url``

Server descriptors
~~~~~~~~~~~~~~~~~~

The ``servers/1`` predicate returns a list of
``server(URL, Description)`` descriptors used to build the top-level
OpenAPI ``servers`` array. ``URL`` must be a valid OpenAPI Server Object
URL string, allowing absolute URLs, relative references, and templated
variables such as ``https://{username}.example.com:{port}/{basePath}``.

Top-level security defaults
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``security/1`` predicate returns a list of top-level OpenAPI
security requirement alternatives. The returned list uses the same
``Requirements`` representation accepted by operation
``security(Requirements)`` properties: each requirement alternative is a
list of ``Scheme-Scopes`` pairs and ``[]`` denotes an empty security
requirement object. Scheme names must match declarations exposed by
``security_scheme/2``.

Operation descriptors
~~~~~~~~~~~~~~~~~~~~~

The ``operations/1`` predicate returns a list of
``operation(Id, Method, Path, Summary, Parameters, RequestBody, Responses, Properties)``
descriptors. ``Id`` is used as the OpenAPI ``operationId`` and must be
unique across the returned descriptor list.

Nested descriptors are
``parameter(Name, In, Description, Required, Schema)``,
``request_body(Description, Required, MediaTypes)``,
``response(Status, Description, MediaTypes)``, and
``media(MediaType, Schema)``. ``Status`` may be an integer HTTP status
code, ``default``, or a wildcard range atom such as ``'2XX'``. Path
parameters must match a template expression in ``Path`` and must use
``Required = true``.

The current implementation keeps the operation property vocabulary
narrow and supports only:

- ``description(Description)``
- ``tags(Tags)``
- ``deprecated(Boolean)``
- ``security(Requirements)`` where ``Requirements`` is a list of
  requirement alternatives represented as lists of ``Scheme-Scopes``
  pairs and ``[]`` denotes an empty security requirement object

Scheme names referenced from ``security(Requirements)`` must match names
exposed by ``security_scheme/2``, and operation-level security overrides
any top-level ``security/1`` declaration.

Reusable schemas
~~~~~~~~~~~~~~~~

The ``schema/2`` predicate maps reusable schema names to JSON Schema
curly terms compatible with the ``json_schema`` library. References to
reusable schemas are expressed using ``schema_ref(Name)`` terms.

Reusable security schemes
~~~~~~~~~~~~~~~~~~~~~~~~~

The ``security_scheme/2`` predicate maps reusable security scheme names
to portable descriptor terms. This library accepts those terms and
normalizes them to OpenAPI Security Scheme Objects when deriving the
final document. The scheme names must match names referenced from
operation ``security(Requirements)`` properties. The descriptor
vocabulary keeps provider objects at the Logtalk domain level instead of
exposing OpenAPI JSON surface spelling directly:

- provider code does not need raw OpenAPI field names such as
  ``openIdConnectUrl`` or ``clientCredentials``
- validation can happen at the level of Logtalk terms before document
  generation
- the same provider facts are easier to reuse in a future portable HTTP
  or authentication library because they describe security intent rather
  than emitted OpenAPI object structure

Supported descriptor shapes are:

- ``api_key(In, Name)``
- ``api_key(In, Name, Options)``
- ``http(Scheme)``
- ``http(Scheme, Options)``
- ``mutual_tls``
- ``mutual_tls(Options)``
- ``oauth2(Flows)``
- ``oauth2(Flows, Options)``
- ``openid_connect(URL)``
- ``openid_connect(URL, Options)``

Supported scheme options are:

- ``description(Description)`` for all scheme kinds
- ``bearer_format(Format)`` for ``http/2``
- ``flows(Flows)`` for ``openid_connect/2`` when locally declared scopes
  are needed without relying on OpenID Connect discovery; these local
  flow declarations are used only for provider-side security-reference
  validation and are not emitted into the final OpenAPI document

Supported OAuth flow descriptors are:

- ``implicit(AuthorizationURL, Scopes)``
- ``implicit(AuthorizationURL, Scopes, [refresh_url(URL)])``
- ``password(TokenURL, Scopes)``
- ``password(TokenURL, Scopes, [refresh_url(URL)])``
- ``client_credentials(TokenURL, Scopes)``
- ``client_credentials(TokenURL, Scopes, [refresh_url(URL)])``
- ``authorization_code(AuthorizationURL, TokenURL, Scopes)``
- ``authorization_code(AuthorizationURL, TokenURL, Scopes, [refresh_url(URL)])``

Scope descriptors are expressed as a list of
``scope(Name, Description)`` terms.

Use descriptor terms such as:

::

   security_scheme(
     user_oauth,
     oauth2([
       client_credentials(
         'https://auth.example.com/oauth/token',
         [scope(read_users, 'Read user data')]
       )
     ])
   ).

   security_scheme(api_key, api_key(header, 'X-API-Key')).

   security_scheme(user_bearer, http(bearer, [bearer_format('JWT')])).

   security_scheme(user_oidc, openid_connect('https://example.com/.well-known/openid-configuration')).

An OpenID Connect scheme can also declare local scopes for the existing
security reference validation path:

::

   security_scheme(
     user_oidc,
     openid_connect(
       'https://example.com/.well-known/openid-configuration',
       [flows([
         authorization_code(
           'https://example.com/oauth/authorize',
           'https://example.com/oauth/token',
           [
             scope(read_users, 'Read user data')
           ]
         )
       ])]
     )
   ).

Those local flow declarations are used only for provider-side security
reference validation. They are not emitted into the derived OpenAPI
document, as OpenID Connect Security Scheme Objects do not define a
``flows`` field.

Provider-side validation checks, among other things:

- duplicate scheme names
- invalid descriptor functors or arities
- required fixed fields after normalization
- valid ``apiKey`` locations
- malformed OAuth flow or scope descriptors
- malformed URL-valued fields such as ``tokenUrl``,
  ``authorizationUrl``, ``refreshUrl``, and ``openIdConnectUrl``

Usage
-----

Load the library and derive a document from a provider object:

::

   | ?- logtalk_load(open_api(loader)),
        open_api::document(sample_open_api_provider, Document).

Parse and generate OpenAPI documents in JSON term form:

::

   | ?- open_api::parse(atom('{"openapi":"3.1.0","info":{"title":"Demo","version":"1.0.0"},"paths":{}}'), Document).

   | ?- open_api::generate(atom(JSON), {openapi-'3.1.0', info-{title-'Demo', version-'1.0.0'}, paths-{}}).

Validate a document and inspect any structural errors:

::

   | ?- open_api::validate_document(Document, Errors).

Resolve an operation descriptor and validate normalized requests,
responses, or HTTP wire sources against it:

::

   | ?- open_api::operation(sample_open_api_provider, update_user, Operation).

   | ?- open_api::validate_request(
         sample_open_api_provider,
         update_user,
         request(
             put,
             origin('/users/11111111-1111-1111-1111-111111111111'),
             http(1, 1),
             [],
             content('application/json', json({name-'Alice Example', active-true})),
             [query_pairs([verbose-true])]
         ),
         Errors
      ).

   | ?- open_api::validate_response(
           sample_open_api_provider,
           get_user,
           response(
               http(1, 1),
               status(404, 'Not Found'),
               [],
               content('application/json', json({code-not_found, message-'User not found'})),
               []
           )
        ).

   | ?- open_api::validate_http_request(
           sample_open_api_provider,
           update_user,
           atom('PUT /users/11111111-1111-1111-1111-111111111111?verbose=true HTTP/1.1\r\ncontent-type: application/json\r\n\r\n{"name":"Alice Example","active":true}')
        ).

Construct standard JSON request and response descriptors when building
provider operations programmatically:

::

   | ?- open_api::json_request_body_descriptor(
           'Message payload',
           true,
           {type-object, properties-{title-{type-string}}, required-[title], additionalProperties- @false},
           RequestBody
        ).

   | ?- open_api::problem_response_descriptor(default, 'Problem response', Response).
