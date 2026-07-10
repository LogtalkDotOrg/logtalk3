________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>  
SPDX-License-Identifier: Apache-2.0

Licensed under the Apache License, Version 2.0 (the License);
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________


`http_router`
=============

The `http_router` library provides the first declarative routing layer on top of
the `http_core` library. It is implemented as a category so that router objects
can import it and implement the `http_handler_protocol` protocol directly while
reusing common method dispatch and path-template matching logic.


Layering
--------

`http_router` sits between the normalized `http_core` message layer and higher-level
API authoring helpers:

- Use `http_server` directly when you only need normalized
  messages, parsers, generators, or low-level handlers.
- Use `http_router` when you want the handler object to keep exposing `handle/2`
  while route matching, metadata annotation, middleware, and content
  negotiation are derived from `route/4` declarations.
- Add `rest` when `route/4` is still too low-level and you
  prefer endpoint descriptors plus small normalized result terms.
- Pair router objects with `open_api` when you want the
  route declarations and metadata to derive OpenAPI operations automatically.
- Add companion libraries such as `http_parameters`, `http_cors`, `http_htmx`,
  `http_session`, `http_static_files`, and `http_directory_listing` when you need
  parameter extraction, middleware helpers, sessions, or static-file routing
  on the same dispatch layer.


API documentation
-----------------

Open the [../../apis/library_index.html#http_router](../../apis/library_index.html#http_router)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_router(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_router(tester)).


Current scope
-------------

The initial routing solution provides one public predicate through the imported
category:

- `handle/2` routes a normalized request using `route/4` clauses defined by the
  importing object.

The remaining router behavior is organized in the sections below.


Route declarations
------------------

Importing router objects must define route descriptors using:

- `route(Id, Method, PathTemplate, Handler)`

The `Method` argument is a normalized lowercase HTTP method atom accepted by
`http_core`, including `query` for RFC 10008 QUERY routes. The `Handler`
argument is the name of a declared local predicate with arity 2, typically a
protected predicate.

When automatic `OPTIONS` or `405 Method Not Allowed` responses are generated,
QUERY routes are included in the `Allow` header like other route methods.


Path templates
--------------

Path-template atoms support literal segments, anonymous `*` wildcard segments,
plain `{name}` placeholders, and typed placeholders such as `{id:integer}` and
`{score:number}`. Example:

	route(show_user, get, '/users/{id}', show_user).


Request annotations
-------------------

The router category calls the matched handler after annotating the request with
`route(Id)`, `path_params(Pairs)`, and any `route_metadata/2` properties. When
`route_produces/2` is defined and the request `Accept` header can be satisfied,
it also adds the negotiated `response_media_type(MediaType)` property. On the
normal routing path it scrubs stale internal synthetic properties such as
`open_api_probe/1`, `automatic_options/1`, `effective_methods/1`, and
`response_media_type/1` before handler execution.


Route metadata
--------------

Importing router objects can optionally declare additional route-specific
metadata using:

- `route_metadata(Id, Metadata)`

When defined, `Metadata` must be a list of compound terms. The router removes
any existing request properties with the same functors, prepends the metadata to
the matched request, and then adds the standard `route/1` and `path_params/1`
annotations. This keeps route metadata available to both route handlers and
response middleware and allows metadata descriptors such as `summary/1`,
`description/1`, `tags/1`, or other application-specific terms.


Route authorization
-------------------

Importing router objects can optionally authorize or decorate routed requests
after route matching and metadata annotation but before route handler dispatch
using:

- `authorize_routed_request(Request, Action)`

The hook must return either `continue(Request)` or `respond(Response)`.
Short-circuited responses still flow through response middleware and keep the
routed request annotations such as `route/1`, `path_params/1`, and any route
metadata properties. This hook is the main place to validate or decorate a
routed request after route matching and metadata annotation but before the route
handler is dispatched.


Content negotiation
-------------------

Importing router objects can optionally declare route response media types
using:

- `route_produces(Id, MediaTypes)`

When defined, `MediaTypes` must be a non-empty list of media type atoms. The
router negotiates the request `Accept` header against that list, annotates the
matched request with `response_media_type(MediaType)`, and returns a generic
`406 Not Acceptable` response when no produced media type matches.

On the normal routing path, any stale incoming `response_media_type/1`
annotation is scrubbed before route dispatch and replaced only when the
matched route successfully negotiates one of its declared `route_produces/2`
media types.


Middleware chaining
-------------------

Importing router objects can also optionally define ordered middleware
descriptors using:

- `middleware(Id, Handler)`

The `Handler` argument is the name of a declared local predicate with arity 2
that receives the current request and returns either `continue(Request)` or
`respond(Response)`. Middleware runs before route matching, so it can rewrite
requests before dispatch or short-circuit processing with an immediate response.
Ordered middleware descriptors are evaluated in declaration order.


Response middleware
-------------------

Importing router objects can also optionally define ordered response middleware
descriptors using:

- `response_middleware(Id, Handler)`

The `Handler` argument is the name of a declared local predicate with arity 3
that receives the current request, the current response, and returns the
transformed response. Response middleware runs after route dispatch or
short-circuit processing, so it can decorate or rewrite any generated response.
It is the final place where a response can be adjusted before it is returned to
the caller.


HEAD fallback
-------------

`HEAD` requests match exact `head` routes first and otherwise fall back to a
matching `get` route.


OPTIONS handling
----------------

`OPTIONS` requests match explicit `options` routes first. When no explicit
`options` route exists for a matched path, the router returns an automatic
`200 OK` response with the derived `Allow` header and an empty body. The
synthetic request used for this path is annotated with
`automatic_options(true)` and `effective_methods(Methods)`. When the router
can identify exactly one matching non-`options` route template, it also
annotates that synthetic request with `route(Id)`, `path_params(Pairs)`, and
that route `route_metadata/2` properties before response middleware runs. When
multiple non-`options` routes match the same path, the synthetic request omits
`route/1`, keeps `path_params/1` only when all matches produce the same value,
and preserves only metadata properties that are identical across all matched
routes. Automatic `OPTIONS` can be customized using
`route_automatic_options_response/3` and still flows through response
middleware.


Custom bad-request responses
----------------------------

Importing router objects can optionally define:

- `route_bad_request_response(Request, Errors, Response)`

Dedicated route-handler exceptions matching
`error(http_parameter_validation(Errors), Context)` with a non-empty `Errors`
list are translated into `400 Bad Request` responses before response middleware
runs. The router uses the optional `route_bad_request_response/3` hook when it
is defined and otherwise falls back to a default plain-text response. The
routed request annotations remain available to the hook and to any later
response middleware.


Custom error responses
----------------------

Importing router objects can optionally customize other routing errors by
defining:

- `route_not_found_response(Request, Response)`
- `route_method_not_allowed_response(Request, AllowedMethods, Response)`

When a path matches but the request method does not, the router returns a
`405 Method Not Allowed` response with an `Allow` header derived from the
matching route descriptors. A `get` route implicitly contributes both `GET` and
`HEAD` to that header. Automatic router support for `OPTIONS` also contributes
`OPTIONS` to the header for matched paths.

The `AllowedMethods` argument passed to the `405` hook is the effective method
list as lowercase atoms and already includes implicit `head` support for `get`
routes and automatic `options` support. The `Request` passed to the `405` hook
and to response middleware is annotated with `matched_path(true)` and
`effective_methods(AllowedMethods)`.


Custom negotiation failures
---------------------------

Importing router objects can optionally define:

- `route_not_acceptable_response(Request, ProducedMediaTypes, Response)`

The `ProducedMediaTypes` argument passed to the `406` hook is the normalized
list declared by `route_produces/2` for the matched route.


OpenAPI derivation
------------------

When a router object also implements the `open_api_provider_protocol`
protocol, the imported category can automatically derive `operations/1` from
`route/4`, `route_metadata/2`, and `route_produces/2`. Recognized
route-metadata terms for OpenAPI derivation are:

- `summary(Summary)`
- `description(Description)`
- `tags(Tags)`
- `deprecated(Boolean)`
- `security(Requirements)`
- `parameters(Parameters)`
- `request_body(RequestBody)`
- `responses(Responses)`

Path-template placeholders automatically derive default OpenAPI path parameter
descriptors using a string schema. Metadata `parameters/1` can override those
defaults or add extra query, header, or cookie parameters. When `responses/1`
is not provided, the router first attempts to infer a successful response by
probing the route handler with a synthetic annotated request and then falls
back to a default successful response. When available, `route_produces/2`
guides both response probing and the derived OpenAPI media types.

When `request_body/1` metadata is not provided, the router also attempts to
infer a request body descriptor by probing the route handler with a synthetic
annotated request whose body is left open for the handler to constrain. The
inferred request and response schemas are emitted inline and are derived from
the observed payload terms:

- `json/1` payloads infer JSON Schema types recursively from the observed JSON
  values.
- `text/1` and `binary/1` payloads infer a string schema.
- `form/1` payloads infer an object schema from the observed key-value pairs.

Explicit `request_body/1` or `responses/1` metadata always takes precedence
over inferred descriptors.

The imported category also exposes top-level OpenAPI provider predicates. The
default `api_info/1` descriptor is derived from the importing object identifier,
`servers/1` defaults to `[]`, and importing objects can override or extend the
provider surface with these hooks:

- `open_api_info(Info)`
- `open_api_servers(Servers)`
- `open_api_security(Security)`
- `open_api_schema(Name, Schema)`
- `open_api_security_scheme(Name, SecurityScheme)`


Current limitations
-------------------

- OpenAPI inference is best-effort. Even with the optional
  `route_open_api_request_body_example/2` and
  `route_open_api_response_example/2` hooks, explicit metadata remains the
  right choice for reusable component schemas and for routes whose contracts
  should be stated directly instead of inferred.
