---
jupyter:
  jupytext:
    text_representation:
      extension: .md
      format_name: markdown
      format_version: '1.3'
      jupytext_version: 1.16.7
  kernelspec:
    display_name: Logtalk
    language: logtalk
    name: logtalk_kernel
---

<!--
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
-->

# products_api

This folder contains a small but complete REST API example built using
the HTTP libraries. It illustrates how `http_parameters`, `http_router`,
`http_server`, and `http_client` fit together:

- `http_parameters`, through the `http_router_parameters` category,
  declares and validates typed query, path, and form parameters for each
  route.
- `http_server` starts a local HTTP listener backed by the router object.
- `http_client` drives that listener with real HTTP requests.

The API is a tiny in-memory product catalog with three routes:

| Method | Path              | Purpose                              |
|--------|-------------------|--------------------------------------|
| GET    | `/products`       | List/search/paginate products        |
| GET    | `/products/{id}`  | Fetch a single product by identifier |
| POST   | `/products`       | Create a new product from form data  |

## Files

- `http_products_store.lgt` — an in-memory product catalog: plain facts plus a
  few filtering and pagination helper predicates.
- `http_products_api.lgt` — the router object. It implements
  `http_handler_protocol` and imports the `http_router_parameters`
  category, declaring `parameter/4` descriptors per route that are reused
  both for extraction (`route_parameters/2`) and for automatic
  type/range/enum validation.
- `http_products_demo.lgt` — starts a local server with `http_server`, issues a
  handful of `http_client` requests against it (including one that fails
  parameter validation on purpose), prints the results, and stops the
  server; it's only loaded with backends supporting threads.
- `tests.lgt` — `lgtunit` tests. Most tests dispatch synthetic request
  terms directly to `http_products_api::handle/2`. One test performs a full
  round trip through real `http_server`/`http_client` sockets; it is
  skipped on backends without threading support.
- `tester.lgt` — loads the example, its dependencies, and runs `tests.lgt`.
- `loader.lgt` — loads the example and its dependencies.

## Loading and running the demo

Print Logtalk, Prolog backend, and kernel versions (if running as a notebook):

```logtalk
%versions
```

Load the example, which contains an `initialization/1` directive that
runs it:

```logtalk
logtalk_load(http_products_api(loader)).
```

When using a backend with threads support, the `http_products_demo.lgt`
file is loaded. The following goal starts a local server, sends it a few
requests using `http_client`, prints each response, and shuts the server
down:

```logtalk
http_products_demo::run.
```

## Testing

```logtalk
logtalk_load(http_products_api(tester)).
```

## Notes

- Query parameters `page`, `per_page`, `status`, and `category` are all
  declared with `http_parameters` options (`default/1`, `minimum/1`,
  `maximum/1`, `enum/1`, `list/1`) so that out-of-range or invalid values
  are rejected with a `400 Bad Request` response before the route handler
  ever runs — see `route_parameter_declarations/2` in `http_products_api.lgt`.
- The `GET /products/{id}` route uses a typed path placeholder,
  `{id:integer}`; a non-numeric `id` segment simply does not match the
  route (`404 Not Found`) rather than failing parameter validation
  (`400 Bad Request`).
- Response bodies use `content('application/json', json({...}))`, the
  repository-standard JSON term representation also used by
  `http_parameters` and `open_api`; `http_core` encodes and decodes it
  automatically for the `application/json` media type, so the same
  handler code works whether it is dispatched directly (as in the unit
  tests) or over a real socket (as in the demo).
