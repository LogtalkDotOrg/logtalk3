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


`http_cors`
===========

The `http_cors` library provides a transport-neutral CORS layer on top of the
normalized `http_core` library. It classifies normalized requests as CORS or
preflight requests, builds direct preflight responses from CORS policies, and
decorates normalized responses with the relevant `Access-Control-*` headers.

This library reuses the `http_core`, `http_server`, and `http_router`. It uses
normalized request and response terms and can be called directly from plain
handlers or from router hooks and response middleware.


API documentation
-----------------

Open the [../../apis/library_index.html#http_cors](../../apis/library_index.html#http_cors)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

    | ?- logtalk_load(http_cors(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

    | ?- logtalk_load(http_cors(tester)).


Current scope
-------------

The current implementation provides one transport-neutral public object with
five public predicates:

- `request_origin/2`
- `is_cors_request/1`
- `is_preflight_request/1`
- `preflight_response/3`
- `add_response_headers/4`

The option DSL follows the `options` library conventions and supports:

- `allowed_origins(any)`
- `allowed_origins(Origins)`
- `allowed_methods(any)`
- `allowed_methods(Methods)`
- `allowed_headers(any)`
- `allowed_headers(requested)`
- `allowed_headers(Headers)`
- `expose_headers(any)`
- `expose_headers(Headers)`
- `allow_credentials(Boolean)`
- `max_age(none)`
- `max_age(Seconds)`

The default policy is:

- `allowed_origins([])`
- `allowed_methods([get])`
- `allowed_headers(requested)`
- `expose_headers([])`
- `allow_credentials(false)`
- `max_age(none)`

This library consumes the normalized HTTP terms provided by the `http_core`
library:

    request(Method, Target, Version, Headers, Body, Properties)
    response(Version, Status, Headers, Body, Properties)

It reads normalized request headers such as `origin`,
`access_control_request_method`, and `access_control_request_headers` and emits
normalized response headers such as `access_control_allow_origin`,
`access_control_allow_methods`, `access_control_allow_headers`,
`access_control_expose_headers`, and `vary`.

When policy depends on a specific requesting origin, generated responses add or
preserve the appropriate `Vary` metadata. Existing `Vary: *` headers are treated
as absorbing and remain `*` when CORS headers are merged into a response.

Wildcard header policies can be expressed explicitly using `allowed_headers(any)`
and `expose_headers(any)`. For compatibility, exact `['*']` option values are
normalized to the same wildcard behavior. On non-credentialed requests these
policies emit `*` when that has the intended CORS wildcard meaning. On
credentialed requests, and for `Authorization` preflight requests, the library
materializes explicit header-name lists instead because browsers otherwise treat
`*` as a literal header name.

Similarly, `allowed_origins(any)` emits `*` for non-credentialed requests but
reflects the concrete request origin for credentialed requests and adds `Vary:
Origin` so caches keep those responses partitioned correctly.

Wildcard origin policies can also be expressed using pattern atoms inside
`allowed_origins(Origins)`, for example `allowed_origins(['https://*.example.com'])`.
These patterns match exactly one leftmost hostname label, require exact scheme
matching, and require exact port matching when the pattern includes an explicit
port. Matching pattern policies always emit the concrete request origin and add
or preserve `Vary: Origin` because the response depends on the requesting
origin.

Wildcard method policies can be expressed using `allowed_methods(any)`. For
compatibility, exact `['*']` values are normalized to the same wildcard
behavior. Method wildcards expand from the request `effective_methods(Methods)`
property after removing `options`, so they are intended for routed or otherwise
annotated requests that provide that metadata. The generated
`Access-Control-Allow-Methods` header still contains an explicit method list.


Route-level overrides
---------------------

When present, the request property `cors(Options)` overrides the caller-supplied
default options. This makes it possible to define router-wide defaults and then
refine them per route using `route_metadata/2`.

Example route metadata:

    route_metadata(show_page, [
        cors([
            allowed_origins(['https://app.example.com']),
            expose_headers([x_trace_id]),
            allow_credentials(true)
        ])
    ]).


Router integration
------------------

For actual routed responses, use router response middleware:

    response_middleware(cors, add_cors_headers).

    add_cors_headers(Request, Response0, Response) :-
        Defaults = [
            allowed_origins([]),
            allowed_methods([get]),
            allowed_headers(requested),
            expose_headers([]),
            allow_credentials(false),
            max_age(none)
        ],
        http_cors::add_response_headers(Request, Response0, Response, Defaults).

For automatic router `OPTIONS` responses, use
`route_automatic_options_response/3` when the router object wants to build a
direct preflight response:

    route_automatic_options_response(Request, _EffectiveMethods, Response) :-
        Defaults = [
            allowed_origins([]),
            allowed_methods([get, post]),
            allowed_headers(requested),
            expose_headers([]),
            allow_credentials(false),
            max_age(none)
        ],
        http_cors::preflight_response(Request, Response, Defaults).

The current `http_router` implementation annotates synthetic automatic
`OPTIONS` requests with `automatic_options(true)`, `effective_methods(Methods)`,
and route metadata when the match is unique or the metadata is shared across
same-path matches. This allows `http_cors` integrations to reuse those
annotations directly.


Plain handler integration
-------------------------

For plain handlers using `http_server`, the recommended pattern is:

1. detect preflight requests with `is_preflight_request/1`
2. build the direct preflight response with `preflight_response/3`
3. otherwise build the application response and pass it through
   `add_response_headers/4`

When `add_response_headers/4` denies a request, it does not add permission
headers but it may still preserve or add cache-relevant `Vary` metadata.
Use `preflight_response/3` when an explicit `403 Forbidden` preflight response
is required.


Current limitations
-------------------

- Origin matching is exact and string-based
- Dynamic callback policies are not supported
