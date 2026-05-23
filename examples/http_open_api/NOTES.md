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

# http_open_api

This example shows a small HTTP server and HTTP client sharing an OpenAPI
contract.

The example illustrates five basic steps:

1. Define a provider object that implements `open_api_provider_protocol`.
2. Publish the generated document at `/openapi.json`.
3. Declare both a request-body `POST /greetings` operation and a
  path-parameter `GET /greetings/{name}` operation.
4. Validate server-side requests and responses against the operation
  descriptors.
5. Let the client fetch the published document before calling the API.

Load the example with:

```logtalk
logtalk_load(http_open_api(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_open_api_demo::run(Result).
```

The returned `Result` term contains the fetched OpenAPI document plus the
HTTP responses returned by both greeting operations.

The direct client can also be used directly:

```logtalk
greetings_client::run(8080, 'Ada', Result).
```

That direct client assumes some server is already running. The example now
also includes an open-ended server alternative and a managed client that
starts and stops that server explicitly in the same process:

```logtalk
managed_greetings_client::run('Ada', Result).
```

When backend threads are not available, run the server and client in
separate sessions. For example, in the first session start the server:

```logtalk
greetings_server::serve(8080, 3).
```

Then, in a second session, fetch the published contract and call the API:

```logtalk
greetings_client::fetch_document(8080, Document).
greetings_client::create_greeting(8080, 'Ada', CreateResponse).
greetings_client::get_greeting(8080, 'Ada', LookupResponse).
```

When backend threads are available and you want the server to keep serving
connections until an explicit stop request, use the open-ended alternative:

```logtalk
open_ended_greetings_server::serve(8080, demo_control).
```

Then later, in the same Logtalk process, stop it explicitly with:

```logtalk
open_ended_greetings_server::stop(demo_control).
```

Study the [http_open_api.lgt](http_open_api.lgt) source file together with
these sample queries. The example is intentionally small so the contract,
handler, and client flow fit on one screen while still showing both body
and path-parameter operations plus bounded and open-ended server lifecycles.
