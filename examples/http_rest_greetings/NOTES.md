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
distributed under the License is distributed on an AS IS BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
________________________________________________________________________
-->

# http_rest_greetings

This example shows a small HTTP server and HTTP client built on top of the
`rest` library.

The example illustrates six basic steps:

1. Define one object that imports `rest` and implements both
   `http_handler_protocol` and `open_api_provider_protocol`.
2. Declare `POST`, `GET`, and `DELETE` endpoints using `endpoint/5`.
3. Use `json_body/2` and `path_parameter/3` inside endpoint actions.
4. Enable automatic OpenAPI request and response validation for selected
   endpoints.
5. Publish the derived OpenAPI document at `/openapi.json`.
6. Let a small HTTP client fetch that document and then exercise the REST
   operations.

Load the example with:

```logtalk
logtalk_load(http_rest_greetings(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_rest_greetings_demo::run(Result).
```

The returned `Result` term contains the fetched OpenAPI document plus the
responses returned by the create, lookup, and delete operations.

When backend threads are not available, run the server and client in
separate sessions. In the first session start the server for four client
connections:

```logtalk
greetings_rest_server::serve(8080, 4).
```

Then, in a second session, fetch the published document and call the API:

```logtalk
greetings_rest_client::fetch_document(8080, Document).
greetings_rest_client::create_greeting(8080, 'Ada', CreateResponse).
greetings_rest_client::get_greeting(8080, 'Ada', LookupResponse).
greetings_rest_client::delete_greeting(8080, 'Ada', DeleteResponse).
```

The example also supports direct in-memory handler queries without starting a
listener. For example:

```logtalk
Request = request(post, origin('/greetings'), http(1, 1), [], content('application/json', json({name-'Ada'})), []),
greetings_rest_api(8080)::handle(Request, Response).
```

This returns a `201 Created` response with a `Location` header and a JSON
greeting body. The same object also publishes an OpenAPI document directly:

```logtalk
Request = request(get, origin('/openapi.json'), http(1, 1), [], empty, []),
greetings_rest_api(8080)::handle(Request, Response).
```

Study the [http_rest_greetings.lgt](http_rest_greetings.lgt) source file
together with these sample queries. The example is intentionally small so
the endpoint descriptors, request helpers, automatic validation, OpenAPI
publication, and client workflow all stay visible at once.
