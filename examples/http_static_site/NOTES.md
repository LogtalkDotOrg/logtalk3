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

# http_static_site

This example shows how to combine the `http_static_files` and
`http_directory_listing` libraries in one small HTTP server.

The example illustrates four basic steps:

1. Prepare a document root for the files and directories you want to expose.
2. Delegate ordinary request paths to `http_static_files::serve/5`.
3. Delegate a separate browsing prefix to `http_directory_listing::serve/5`.
4. Exercise both handlers through ordinary `http_client` GET requests.

Load the example with:

```logtalk
logtalk_load(http_static_site(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_static_site_demo::run(Result).
```

The returned `Result` term contains three normalized responses: the home page
served from `/`, the plain-text guide file served from `/docs/guide.txt`, and
the HTML directory listing served from `/browse/docs/`.

When backend threads are not available, run the bounded server and the client
workflow in separate sessions. In the first session start the example server:

```logtalk
static_site_server::serve(8080, 3).
```

Then, in a second session, fetch the same three resources:

```logtalk
static_site_client::run(8080, Result).
```

To explore the example with a web browser instead of the direct client, start
the server with a larger connection count so it can handle multiple manual
requests:

```logtalk
static_site_server::serve(8080, 20).
```

Then open these URLs in a browser:

- `http://127.0.0.1:8080/` for the home page served by `http_static_files`
- `http://127.0.0.1:8080/docs/guide.txt` for a plain-text file response
- `http://127.0.0.1:8080/browse/docs/` for the HTML directory listing served by `http_directory_listing`

From the `/browse/docs/` listing page, clicking `guide.txt` works by
forwarding `/browse/...` file requests to `http_static_files` while
keeping `/browse/.../` requests on `http_directory_listing`.

The server stops automatically after the requested number of accepted
connections, so restart it if you want to keep browsing after that limit is
reached.

The example server above creates its own temporary sample document root. To use
your own document root instead, call the alternative server predicate:

```logtalk
static_site_server::serve(8080, '/path/to/docroot', 3).
```

Study the [http_static_site.lgt](http_static_site.lgt) source file together
with these sample queries. The example is intentionally small so the two helper
libraries stay visible: the handler strips a `/browse` prefix before calling
`http_directory_listing::serve/5`, strips the leading slash from ordinary file
requests before calling `http_static_files::serve/5`, and lets the helpers take
care of method handling, sandboxing, and response construction.
