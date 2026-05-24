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
the `http` core library. It is implemented as a category so that router objects
can implement the `http_handler_protocol` protocol directly while reusing common
method dispatch and path-template matching logic.


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

The initial routing slice provides one public predicate through the imported
category:

- `handle/2` routes a normalized request using `route/4` clauses defined by the
  importing object.

Importing router objects are expected to define route descriptors using:

- `route(Id, Method, PathTemplate, Handler)`

The `Handler` argument is the name of a declared local predicate with arity 2,
typically a protected predicate. The router category calls that predicate after
annotating the matched request with:

- `route(Id)`
- `path_params(Pairs)`

Importing router objects can also optionally declare additional route-specific
metadata using:

- `route_metadata(Id, Metadata)`

When defined, `Metadata` must be a list of compound terms. The router removes
any existing request properties with the same functors, prepends the metadata to
the matched request, and then adds the standard `route/1` and `path_params/1`
annotations. This keeps route metadata available to both route handlers and
response middleware and allows metadata descriptors such as `summary/1`,
`description/1`, `tags/1`, or other application-specific terms.

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

Importing router objects can also optionally declare route response media types
using:

- `route_produces(Id, MediaTypes)`

When defined, `MediaTypes` must be a non-empty list of media type atoms. The
router negotiates the request `Accept` header against that list, annotates the
matched request with `response_media_type(MediaType)`, and returns a generic
`406 Not Acceptable` response when no produced media type matches.

Importing router objects can also optionally define ordered middleware
descriptors using:

- `middleware(Id, Handler)`

The `Handler` argument is the name of a declared local predicate with arity 2
that receives the current request and returns either `continue(Request)` or
`respond(Response)`. Middleware runs before route matching, so it can rewrite
requests before dispatch or short-circuit processing with an immediate response.

Importing router objects can also optionally define ordered response middleware
descriptors using:

- `response_middleware(Id, Handler)`

The `Handler` argument is the name of a declared local predicate with arity 3
that receives the current request, the current response, and returns the
transformed response. Response middleware runs after route dispatch or
short-circuit processing, so it can decorate or rewrite any generated response.

Path templates are atoms using literal segments and optional `{name}`
placeholders. Example:

	route(show_user, get, '/users/{id}', show_user).

`HEAD` requests match exact `head` routes first and otherwise fall back to a
matching `get` route.

When a path matches but the request method does not, the router returns a
`405 Method Not Allowed` response with an `Allow` header derived from the
matching route descriptors. A `get` route implicitly contributes both `GET` and
`HEAD` to that header. Automatic router support for `OPTIONS` also contributes
`OPTIONS` to the header for matched paths.

`OPTIONS` requests match explicit `options` routes first. When no explicit
`options` route exists for a matched path, the router returns an automatic
`200 OK` response with the derived `Allow` header and an empty body.

Importing router objects can optionally customize error handling by defining:

- `route_not_found_response(Request, Response)`
- `route_method_not_allowed_response(Request, AllowedMethods, Response)`
- `route_not_acceptable_response(Request, ProducedMediaTypes, Response)`

The `AllowedMethods` argument passed to the `405` hook is the effective method
list as lowercase atoms and already includes implicit `head` support for `get`
routes and automatic `options` support.

The `ProducedMediaTypes` argument passed to the `406` hook is the normalized
list declared by `route_produces/2` for the matched route.


Current limitations
-------------------

- The current slice supports path-template matching, pre-routing middleware
  chaining, response post-processing middleware, route-specific metadata
  annotations, automatic OpenAPI provider derivation including best-effort
  request and successful response schema inference, method dispatch,
  route-level content negotiation,
  customizable `404`/`405`/`406` responses, and automatic `OPTIONS`
  responses.
- Path templates support literal segments, anonymous `*` wildcard segments,
  plain `{name}` placeholders, and typed placeholders such as
  `{id:integer}` and `{score:number}`.
- OpenAPI inference is still best-effort, but it now accepts optional
  `route_open_api_request_body_example/2` and
  `route_open_api_response_example/2` hooks before falling back to handler
  probing. Explicit metadata remains the right choice for reusable component
  schemas and for routes whose contracts should be stated directly instead of
  inferred.
