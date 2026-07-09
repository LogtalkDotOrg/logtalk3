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

# http_routes

This example illustrates the `http_router`, `http_server`, and
`http_client` libraries by defining a small HTTP server with three
routes and a client that calls each of them:

- `routes` is an object that imports the `http_router` category and
  implements the `http_handler_protocol` protocol. It declares three
  routes using `route/4` facts:

  - `GET /hello` — a plain static route that returns a fixed text
    response.
  - `GET /users/{id:integer}` — a route using a typed path-template
    placeholder; the router extracts and types the path parameter and
    annotates the request with a `path_params/1` property before
    calling the handler.
  - `POST /echo` — a route that reads the request body and echoes it
    back, prefixed with `"Echo: "`.

- `routes_demo` is an object that starts a local, loopback HTTP
  server serving the `routes` object using the `http_server` facade,
  calls each of the three routes using the `http_client` convenience
  predicates (`get/3` and `post/4`), prints the normalized responses,
  and stops the server.

This example requires a backend Prolog compiler that supports threads,
as `http_server::start/4` and `http_server::stop/1` run the server
concurrently with the client calls.

## Loading

```logtalk
| ?- logtalk_load(http_routes(loader)).
```

## Running

```logtalk
| ?- routes_demo::run.
GET /hello -> 200 OK
  Hello from the Logtalk http_router example!
GET /users/42 -> 200 OK
  User #42
POST /echo -> 200 OK
  Echo: Hello, router!
true.
```
