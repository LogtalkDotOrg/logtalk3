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

# http_static_site_basic

This example shows how to protect a small static site with HTTP Basic
authentication while still combining the `http_static_files` and
`http_directory_listing` libraries in one small HTTP server.

The example illustrates five basic steps:

1. Prepare a document root for the files and directories you want to expose.
2. Prepare a matching `.htpasswd` file outside the served document root.
3. Delegate ordinary request paths to `http_static_files::serve/5`.
4. Delegate a separate browsing prefix to `http_directory_listing::serve/5` with custom presentation options.
5. Wrap the static-site handler with `http_server_basic_handler(_, _, _)` so the whole site requires valid credentials.

Load the example with:

```logtalk
logtalk_load(http_static_site_basic(loader)).
```

The bundled sample credentials are:

- user name: `Mufasa`
- password: `Circle Of Life`
- realm: `static-site`

When backend threads are available, run the complete self-contained demo:

```logtalk
http_static_site_basic_demo::run(Result).
```

The returned `Result` term contains four normalized responses: the initial
`401 Unauthorized` challenge produced by a request without credentials, the
home page served from `/`, the plain-text guide file served from
`/docs/guide.txt`, and the HTML directory listing served from `/browse/docs/`.

The directory-listing route demonstrates the same `http_directory_listing`
customization hooks as the non-authenticated example:

1. `columns([name, type, modified])` hides the size column.
2. `type_display(media)` shows MIME-based file types such as `text/plain`.
3. `theme(ocean)` adds stable CSS hook classes to the generated HTML.
4. `stylesheets(['/assets/listing.css'])` links a stylesheet served by the
   same protected site.

When backend threads are not available, run the bounded server and the client
workflow in separate sessions. In the first session start the example server:

```logtalk
static_site_basic_server::serve(8080, 4).
```

Then, in a second session, fetch the challenge and the same three protected
resources with the bundled credentials:

```logtalk
static_site_basic_client::run(8080, Result).
```

To explore the example with a web browser instead of the direct client, start
the server with a larger connection count so it can handle multiple manual
requests:

```logtalk
static_site_basic_server::serve(8080, 20).
```

Then open these URLs in a browser:

- `http://127.0.0.1:8080/` for the protected home page
- `http://127.0.0.1:8080/docs/guide.txt` for a protected plain-text file
- `http://127.0.0.1:8080/browse/docs/` for the protected HTML directory listing
- `http://127.0.0.1:8080/assets/listing.css` for the stylesheet used by the customized directory listing

The browser will prompt for the sample user name and password the first time it
receives the `401 Unauthorized` response. After a successful sign-in, the same
credentials are reused automatically by the browser when it requests linked
resources such as `listing.css` or `guide.txt` from the directory listing page.

From the `/browse/docs/` listing page, clicking `guide.txt` works by
forwarding `/browse/...` file requests to `http_static_files` while keeping
`/browse/.../` requests on `http_directory_listing`.

The password file used by the example is generated outside the served document
root on purpose. This keeps the example self-contained without demonstrating an
unsafe layout where static-file serving could expose the credential file.

The `.htpasswd` entry is generated from the plain-text password instead of
being kept as a magic constant. Apache `{SHA}` entries store the Base64
encoding of the raw 20-byte SHA-1 digest of the password bytes. In other
words, the example computes:

1. the password bytes using `atom_codes/2`
2. the raw SHA-1 digest bytes using `sha1::digest/2`
3. the Base64 text using `base64::generate/2`
4. the final line by prefixing the user name and the `{SHA}` marker

For the bundled password `Circle Of Life`, this pipeline produces the stored
digest text `HDWE96v093gThQ8bU2xY5rEgegA=`. The complete `.htpasswd` line then
becomes:

```text
Mufasa:{SHA}HDWE96v093gThQ8bU2xY5rEgegA=
```

The example uses `sha1::digest/2` instead of `sha1::hash/2` because Apache
expects Base64 over the raw digest bytes, not over the hexadecimal text form.

The server stops automatically after the requested number of accepted
connections, so restart it if you want to keep browsing after that limit is
reached.

The example server above creates both its own temporary sample document root and
its own temporary password file. To use your own document root and password file
instead, call the alternative server predicate:

```logtalk
static_site_basic_server::serve(8080, '/path/to/docroot', '/path/to/.htpasswd', 3).
```

Study the [http_static_site_basic.lgt](http_static_site_basic.lgt) source file
together with these sample queries. The example is intentionally small so the
two static-site libraries and the Basic-auth wrapper stay visible: the handler
strips a `/browse` prefix before calling `http_directory_listing::serve/5`,
strips the leading slash from ordinary file requests before calling
`http_static_files::serve/5`, keeps the password file outside the served tree,
and lets `http_server_basic_handler(_, _, _)` take care of request protection,
challenge generation, and `401 Unauthorized` responses.
