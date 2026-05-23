________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`open_api`
=========

The `open_api` library provides a first-pass implementation for deriving,
parsing, generating, and structurally validating OpenAPI 3.1.0 documents.
It builds on top of the `json`, `json_schema`, and `application` libraries.

This initial implementation focuses on deriving OpenAPI documents from
objects implementing the `open_api_provider_protocol` protocol using a small,
portable descriptor vocabulary.


API documentation
-----------------

Open the [../../apis/library_index.html#open_api](../../apis/library_index.html#open_api)
link in a web browser.


Loading
-------

To load this library, load the `loader.lgt` file:

	| ?- logtalk_load(open_api(loader)).


Current scope
-------------

- Derive OpenAPI 3.1.0 documents from provider objects exposing
  `api_info/1`, `servers/1`, `security/1`, `operations/1`, `schema/2`, and
  `security_scheme/2`
- Resolve provider operations by `operationId` using `operation/3`
- Validate normalized HTTP request and response terms, or objects
  implementing the `http_request_protocol` and
  `http_response_protocol` protocols, against provider operation
  descriptors
- Parse HTTP request and response wire sources through the `http`
  library before validating them against provider operation
  descriptors
- Parse and generate OpenAPI documents using the repository-standard JSON
  term representation
- Perform structural validation of derived or parsed documents using an
  inline phase-1 JSON Schema
- Reject provider descriptor sets that reuse the same `operationId` atom for
  more than one operation
- Derive top-level OpenAPI `security` defaults from provider `security/1`
- Derive reusable security schemes under `components.securitySchemes` and
  reject top-level or operation `security/1` references to undeclared scheme
  names or undeclared OAuth flow scopes when locally available
- Accept portable `security_scheme/2` descriptors using `api_key/2-3`,
  `http/1-2`, `mutual_tls/0-1`, `oauth2/1-2`, and `openid_connect/1-2`
  while still normalizing legacy raw OpenAPI Security Scheme Object curly
  terms during transition
- Reject malformed reusable security scheme descriptors that omit fixed fields
  required by their `type`, and reject malformed declared OAuth flow objects
  that omit their required URLs or `scopes`
- Reject provider-side invalid `apiKey.in` values and malformed URL-valued
  security fields such as OAuth token or authorization endpoints
- Accept provider `server(URL, Description)` descriptors using absolute,
  relative, or templated OpenAPI Server Object URLs, while still rejecting
  malformed server URL strings
- Accept wildcard response status descriptors such as `'2XX'`
- Reject provider path parameter descriptors that are not declared in the
  operation path template or that do not use `Required = true`
- Reject parsed or derived documents whose inline path parameters violate the
  OpenAPI path-parameter invariants
- Accept webhook-only OpenAPI 3.1.0 documents during structural validation

The current implementation keeps the operation property vocabulary narrow and
supports only the property terms already used in the draft provider examples:

- `description(Atom)`
- `tags(List)`
- `deprecated(Boolean)`
- `security(Requirements)` where `Requirements` is a list of security
  requirement alternatives, each represented as a list of `Scheme-Scopes`
  pairs, and `[]` denotes an empty security requirement object

Provider objects that also implement `application_protocol` can contribute
missing OpenAPI metadata:

- `description/1` maps to `info.description`
- `license/1` maps to `info.license.name`
- `homepage/1` maps to the top-level `externalDocs.url`


Current limitations
-------------------

- Validation is intentionally partial and does not attempt to implement the
  full OpenAPI 3.1 meta-schema, but it now includes type-specific fixed-field
  checks for Security Scheme Objects and OAuth Flow Objects
- The library currently derives only `components.schemas` and
  `components.securitySchemes` plus the root `security` field
- The library now integrates with the `http` core for request and
  response parsing and for protocol-based access to normalized HTTP
  messages, but it still limits itself to contract validation rather
  than transport concerns
- The supported `security_scheme/2` descriptor vocabulary is summarized in
  [SECURITY_DESCRIPTOR_DSL.md](SECURITY_DESCRIPTOR_DSL.md). Legacy raw OpenAPI
  Security Scheme Object curly terms are still accepted during transition
- Scope validation currently cross-checks against locally declared OAuth flow
  scopes when they are present in a reusable security scheme. Full OpenID
  Connect discovery-based scope resolution remains out of scope
- Query and cookie parameter validation currently relies on typed request
  properties such as `query_pairs/1` and `cookies/1`; raw query-string
  parsing is intentionally out of scope for this first pass
- Request and response body validation currently supports `json/1`,
  `text/1`, `binary/1`, and `form/1` payload terms; multipart payload
  validation remains out of scope


Usage
-----

Load the library and derive a document from a provider object:

	| ?- logtalk_load(open_api(loader)),
	     open_api::document(sample_open_api_provider, Document).

Parse and generate OpenAPI documents in JSON term form:

	| ?- open_api::parse(atom('{"openapi":"3.1.0","info":{"title":"Demo","version":"1.0.0"},"paths":{}}'), Document).

	| ?- open_api::generate(atom(JSON), {openapi-'3.1.0', info-{title-'Demo', version-'1.0.0'}, paths-{}}).

Validate a document and inspect any structural errors:

	| ?- open_api::validate_document(Document, Errors).

Resolve an operation descriptor and validate normalized requests,
responses, or HTTP wire sources against it:

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
