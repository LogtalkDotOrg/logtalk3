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


`http_oauth`
============

This library implements OAuth protected-resource support for normalized HTTP:

- RFC 6750 Bearer Authorization and challenge parsing/generation
- staged token authentication and scope authorization
- resource-aware verifier and pluggable scope-checker protocols
- RFC 9728 protected-resource metadata documents and public endpoint handling
- RFC 7662 token introspection with Basic, Bearer, or custom-header client authentication
- JWT access-token verification using the `jwt` library
- generic handler and `http_router` integration


API documentation
-----------------

Open the [../../apis/library_index.html#http-oauth](../../apis/library_index.html#http-oauth)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_oauth(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_oauth(tester)).


Usage
-----

### Protected-resource and verifier contract

Every authentication call requires a canonical
`protected_resource(Identifier)` option. Use the externally visible HTTPS URL
that identifies the protected resource and use the same atom consistently in
metadata, token audiences, and protection options.

A verifier is any object implementing `http_oauth_verifier_protocol`. Its
`verify/3` predicate receives the raw token and expected protected-resource
identifier. It must fail for an invalid, inactive, expired, or wrong-audience
token and return normalized information on success:

	:- object(my_token_verifier,
		implements(http_oauth_verifier_protocol)).

		verify(Token, ProtectedResource, oauth_token_info([
			source(my_authorization_server),
			scopes(Scopes),
			audience_validation(exact(ProtectedResource)),
			claims(Claims)
		])) :-
			% Verify Token and bind Scopes and Claims here.
			...

	:- end_object.

The library adds the resulting `oauth_token_info/1` and `oauth_scopes/1`
properties to the normalized request. The raw Bearer token is deliberately not
retained. Handlers can query these properties using `token_info/2`, `scopes/2`,
or the `http_core::property/2` predicate.

### Protecting a request with a static scope policy

Use `protect_request/4` when the required scopes are known before routing or
dispatch. The default `http_oauth_exact_scope_checker` requires every listed
scope and compares scope atoms exactly:

	http_oauth::protect_request(Request0, my_token_verifier, Action, [
		protected_resource('https://api.example.com/orders'),
		required_scopes([orders_read])
	]),
	( Action = continue(Request) ->
		orders_handler::handle(Request, Response)
	; Action = respond(Response)
	).

Missing credentials produce a 401 response, malformed authorization data a
400 response, invalid tokens a 401 response, and insufficient scopes a 403
response. Optional `realm/1`, `resource_metadata/1`, `headers/1`, `body/1`,
and `properties/1` options customize generated error responses. Supply a
`scope_checker/1` object implementing `http_oauth_scope_checker_protocol` for
policies other than exact all-scopes matching.

### Separating authentication from authorization

Use the staged predicates when routing, operation selection, or request-body
inspection determines the required scopes. Authenticate first, resolve the
operation, and then authorize the annotated request:

	http_oauth::authenticate_request(Request0, my_token_verifier, Authentication, [
		protected_resource('https://api.example.com/orders')
	]),
	( Authentication = continue(AuthenticatedRequest) ->
		required_operation_scopes(AuthenticatedRequest, RequiredScopes),
		http_oauth::authorize_request(AuthenticatedRequest, Action, [
			required_scopes(RequiredScopes)
		])
	; Action = Authentication
	).

This form is useful for streaming servers because scope authorization can be
completed before response headers are committed. Pass any response-decoration
options needed by each stage to that stage.

### Wrapping an HTTP handler

The parametric `http_server_core_oauth_handler/3` object applies a static
policy around any object implementing `http_handler_protocol`:

	http_server_core_oauth_handler(
		my_token_verifier,
		orders_handler,
		[
			protected_resource('https://api.example.com/orders'),
			required_scopes([orders_read])
		]
	)::handle(Request, Response).

The wrapped handler receives the annotated request only after authentication
and authorization succeed. Otherwise, the wrapper returns the generated OAuth
response without calling the handler.

### Applying route-specific policies

Objects importing `http_router` can also import the parametric
`http_router_oauth/2` category. Base options define the protected resource and
shared policy; an `oauth/1` route metadata property overrides options for an
individual route:

	:- object(api,
		implements(http_handler_protocol),
		imports([
			http_router,
			http_router_oauth(my_token_verifier, [
				protected_resource('https://api.example.com')
			])
		])).

		route(list_orders, get, '/orders', list_orders).
		route_metadata(list_orders, [oauth([required_scopes([orders_read])])]).

		authorize_routed_request(Request, Action) :-
			^^authorize_oauth_request(Request, Action).

Routes without `oauth/1` metadata remain public. Route options override base
options by option name and arity.

### Publishing protected-resource metadata

`http_oauth_metadata::well_known_url/2` derives the RFC 9728 endpoint from a
canonical HTTPS resource identifier. For example,
`https://api.example.com/orders` maps to
`https://api.example.com/.well-known/oauth-protected-resource/orders`.

Use `document/3-4` to build a JSON term or `response/3-4` to build a normalized
HTTP response:

	http_oauth_metadata::response(
		'https://api.example.com/orders',
		[
			authorization_servers(['https://identity.example.com']),
			scopes_supported([orders_read, orders_write]),
			resource_name('Orders API'),
			resource_documentation('https://api.example.com/docs/orders')
		],
		Response,
		[required_members([authorization_servers])]
	).

Identifiers and URL-valued descriptors must use HTTPS. Use
`extension(Name, Value)` for non-standard members; registered metadata names
cannot be supplied as extensions. The
`http_server_core_oauth_endpoint_handler/6` wrapper combines public metadata
publication with a protected application handler. It serves only `GET` at the
exact derived well-known path, returns 405 for other methods at that path, and
protects every other request.

### Verifying opaque tokens by introspection

Use the parametric `http_oauth_introspection_verifier/2` when access tokens are
validated by an RFC 7662 endpoint:

	Verifier = http_oauth_introspection_verifier(
		'https://identity.example.com/oauth/introspect',
		[authentication(basic(client_id, client_secret))]
	),
	http_oauth::protect_request(Request, Verifier, Action, [
		protected_resource('https://api.example.com/orders'),
		required_scopes([orders_read])
	]).

The introspection client also supports `authentication(bearer(Token))` and
`authentication(headers(Headers))`, plus `token_type_hint/1`, custom
`headers/1`, and underlying `http_options/1`. Introspection endpoints require
HTTPS. The `allow_insecure_localhost(true)` option exists only for local tests
and accepts loopback HTTP endpoints.

By default, the verifier requires the introspection `aud` value to contain the
protected-resource identifier. Use `audience_validation(trust_active)` only
when the authenticated introspection service guarantees that `active=true` is
already specific to the requested resource. The inbound Bearer token must
never be reused as client authentication or forwarded to downstream APIs.

### Verifying JWT access tokens locally

Use `http_oauth_jwt_verifier/2` for signed JWT access tokens. Its first
parameter is any symmetric key, public JWK, or JWK Set accepted by
`jwt::verify/4`; its second parameter is the corresponding JWT verification
option list:

	Verifier = http_oauth_jwt_verifier(PublicJWKSet, [
		allow_algorithms(['RS256']),
		claim_policy([
			claim(iss, expected('https://identity.example.com'))
		])
	]),
	http_oauth::protect_request(Request, Verifier, Action, [
		protected_resource('https://api.example.com/orders')
	]).

The verifier requires an `aud` claim matching the protected resource. A
space-delimited `scope` claim is normalized to a list of atoms; if it is
absent, the granted scope list is empty. Signature, registered-claim, and time
validation behavior is configured using the `jwt` library options.

### Parsing headers and building OAuth responses

Use `authorization/2` and `challenge/2` with normalized HTTP messages, or the
`parse_authorization/2`, `generate_authorization/2`, `parse_challenge/2`, and
`generate_challenge/2` predicates with individual header values. Bearer terms
use the forms `bearer_authorization(Token)` and
`bearer_challenge(Fields)`.

The `unauthorized_response/3` and `forbidden_response/3` predicates construct
new normalized responses. Their arity-four variants decorate an existing
response while preserving its HTTP version and any body, headers, and
properties not overridden by options. These helpers are useful when an
application needs to add OAuth challenges to its own error representation.

### MCP over HTTP

OAuth protection applies to MCP HTTP transports. It does not apply to MCP
stdio transports, and an MCP session identifier never substitutes for
per-request Bearer authentication. Every protected HTTP request must therefore
carry credentials accepted by the configured verifier.

For MCP protected-resource metadata, use
`required_members([authorization_servers])`. The
`http_server_core_oauth_endpoint_handler/6` wrapper is the simplest way to
serve that metadata publicly at the exact RFC 9728 well-known path while
protecting the MCP handler at all other paths. Use staged authentication and
authorization when the MCP method determines the required scopes, so an
insufficient-scope response can be returned before starting a streamed MCP
response.


Limitations
-----------

- OAuth protection applies only to HTTP transports, not MCP stdio transports.
- Bearer tokens are accepted only in the HTTP `Authorization` header.
- The library does not implement authorization-server discovery, token
	acquisition, token refresh, or token revocation.
- Token introspection is synchronous and does not provide built-in caching.
- JWT verification is limited to the algorithms and capabilities supported by
	the `jwt` library and the selected backend.
- Protected-resource metadata and token introspection require HTTPS, except
	for the explicit introspection loopback override intended for local tests.
- The default scope policy requires exact matching of all required scopes;
	alternative policies require a custom scope-checker object.
