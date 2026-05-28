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


`http_parameters`
=================

The `http_parameters` library provides typed query, form, path, header,
and cookie parameter extraction helpers for normalized HTTP requests
together with an optional router companion category that reuses the same
declarations for OpenAPI metadata generation.

Currently this library provides:

- object `http_parameters`
- category `http_router_parameters`


API documentation
-----------------

Open the [../../apis/library_index.html#http_parameters](../../apis/library_index.html#http_parameters)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_parameters(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_parameters(tester)).


Declarations
------------

Parameter declarations use the shape:

	parameter(Name, Source, Type, Options)

The currently supported source atoms are:

- `query`
- `form`
- `path`
- `header`
- `cookie`

Header declarations match normalized request headers case-insensitively.
Cookie declarations read from the normalized `cookies(Pairs)` request
property already exposed by the `http` library.
Reserved OpenAPI header names such as `accept`, `content_type`, and
`authorization` remain available for runtime extraction but are omitted
from generated OpenAPI parameter descriptors.

The currently supported scalar types are:

- `string`
- `text`
- `atom`
- `integer`
- `number`
- `boolean`

List-valued declarations use `list(ScalarType)`.

The currently supported options are:

- `optional`
- `default(Value)`
- `description(Description)`
- `schema(Schema)`
- `enum(Values)`
- `minimum(Value)`
- `maximum(Value)`
- `pattern(Pattern)`

The `enum/1`, `minimum/1`, and `maximum/1` options are enforced at
runtime after type coercion. For list-valued declarations they apply to
each extracted item. Defaults are also checked against these constraints
when declarations are normalized.

The `minimum/1` and `maximum/1` options are only valid for `integer` and
`number` declarations. The `pattern/1` option is only valid for `string`,
`text`, and `atom` declarations and is currently used only to enrich the
generated OpenAPI schema; it is not enforced at runtime.

Duplicate declaration pairs with the same `(Name, Source)` are rejected.

Path declarations must remain scalar and cannot use `default/1`.
When the library is used through `http_router_parameters`, any declared
`path` parameter must match a placeholder in the route path template. Typed
route placeholders remain authoritative for runtime type compatibility:

- `{name:integer}` requires a declared runtime type of `integer`
- `{name:number}` requires a declared runtime type of `number`
- `{name}` allows declared runtime types `string`, `text`, or `atom`

Descriptions and custom `schema/1` values can still refine the generated
OpenAPI descriptor for a matching path parameter, but they cannot rename
the placeholder or change its runtime type.


Error handling
--------------

Client-input validation failures throw exceptions using the shape:

	error(http_parameter_validation(Errors), Context)

The `Errors` list currently contains these structured terms:

- `missing_parameter(Source, Name)`
- `duplicate_parameter(Source, Name)`
- `invalid_parameter_value(Source, Name, Value, Reason)`

The `Reason` term is currently one of:

- `expected(Type)`
- `incompatible_type(Type)`
- `not_in_enum(Values)`
- `below_minimum(Minimum)`
- `above_maximum(Maximum)`
- `invalid_list_item(Index, ItemReason)`

Even though the public API currently fails fast, the list-shaped contract
keeps error detection and reporting decoupled and leaves room for future
accumulated validation without changing router integration.


Router integration
------------------

The `http_router_parameters` category extends `http_router` and adds:

- `route_parameters/2`
- `route_parameter_declarations/2`
- `route_parameter_request_body_description/2`
- `route_parameter_extra_metadata/2`

The category generates OpenAPI parameter descriptors for `query`, `path`,
`header`, and `cookie` declarations, `application/x-www-form-urlencoded`
request bodies for `form` declarations, and a default `400 Bad Request`
response descriptor. When a declaration uses `enum/1`, `minimum/1`,
`maximum/1`, or `pattern/1`, the generated parameter or form-property
schema reflects those constraints when the schema is generated by the
library.

When `route_parameter_extra_metadata/2` also defines `responses/1`, an
explicit `response(400, ...)` replaces the generated validation response.
A `response(default, ...)` remains additive and does not suppress the
generated `400 Bad Request` descriptor.
