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

# http_static_site_digest

This example shows how to protect a small static site with HTTP Digest
authentication while still combining the `http_static_files` and
`http_directory_listing` libraries in one small HTTP server.

The example illustrates five basic steps:

1. Prepare a document root for the files and directories you want to expose.
2. Provide a verifier object implementing `http_digest_verifier_protocol` by deriving `HA1` values from the sample credentials.
3. Delegate ordinary request paths to `http_static_files::serve/5`.
4. Delegate a separate browsing prefix to `http_directory_listing::serve/5` with custom presentation options.
5. Wrap the static-site handler with `http_server_core_digest_handler(_, _, _, _)` so the whole site requires valid Digest credentials.

Load the example with:

```logtalk
logtalk_load(http_static_site_digest(loader)).
```

The bundled sample settings are:

- user name: `Mufasa`
- password: `Circle Of Life`
- realm: `static-site`
- challenge algorithm: `sha256`

The verifier does not hide a precomputed `HA1` constant. HTTP Digest uses:

```text
HA1 = H("Username:Realm:Password")
```

where `H` is the selected Digest algorithm. In this example the verifier:

1. builds the material atom `Username:Realm:Password` using `atomic_list_concat/3`
2. converts that atom to bytes using `atom_codes/2`
3. hashes those bytes using `md5::hash/2`, `sha256::hash/2`, or `sha512_256::hash/2`
4. returns the resulting lowercase hexadecimal digest text as the `HA1` value

For the bundled settings, the material string is:

```text
Mufasa:static-site:Circle Of Life
```

Using the bundled `sha256` challenge algorithm, that produces:

```text
763b3b4e93dcb059118ec297b15b0d5cb9f988f6a9265a092da5e127265649b0
```

Unlike the Basic-authenticated example, Digest does not use Apache-style
`{SHA}` entries or Base64 over raw digest bytes. The verifier returns the
hexadecimal hash text expected by the Digest library for the selected
algorithm.

When backend threads are available and the backend supports the Digest client
session object, run the complete self-contained demo:

```logtalk
http_static_site_digest_demo::run(Result).
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

When backend threads are not available but the backend still supports the
Digest client session object, run the bounded server and the client workflow
in separate sessions. In the first session start the example server:

```logtalk
static_site_digest_server::serve(8080, 7).
```

Then, in a second session, fetch the challenge and the same three protected
resources with the bundled credentials:

```logtalk
static_site_digest_client::run(8080, Result).
```

The bundled `http_client_digest_session` object is reactive: each protected
request is sent once without credentials, retried only after the server returns
the Digest challenge, and then succeeds with the computed `Authorization`
header. Because of that extra round trip, the bounded server above needs a
larger connection count than the Basic-authenticated example.

Depending on your browser, manual HTTP Digest authentication may or may not be
available. The direct example client is therefore the reference workflow for
this example. If your browser supports HTTP Digest, start the server with a
larger connection count such as:

```logtalk
static_site_digest_server::serve(8080, 20).
```

Then open these URLs in a browser:

- `http://127.0.0.1:8080/` for the protected home page
- `http://127.0.0.1:8080/docs/guide.txt` for a protected plain-text file
- `http://127.0.0.1:8080/browse/docs/` for the protected HTML directory listing
- `http://127.0.0.1:8080/assets/listing.css` for the stylesheet used by the customized directory listing

From the `/browse/docs/` listing page, clicking `guide.txt` works by
forwarding `/browse/...` file requests to `http_static_files` while keeping
`/browse/.../` requests on `http_directory_listing`.

The server stops automatically after the requested number of accepted
connections, so restart it if you want to keep browsing after that limit is
reached.

The example server above creates its own temporary sample document root. To use
your own document root instead, call the alternative server predicate:

```logtalk
static_site_digest_server::serve(8080, '/path/to/docroot', 7).
```

Study the [http_static_site_digest.lgt](http_static_site_digest.lgt) source
file together with these sample queries. The example is intentionally small
so the two static-site libraries, the Digest verifier object, and the
Digest-authentication wrapper stay visible: the handler strips a `/browse`
prefix before calling `http_directory_listing::serve/5`, strips the leading
slash from ordinary file requests before calling `http_static_files::serve/5`,
keeps the Digest verifier logic in its own object, and lets
`http_server_core_digest_handler(_, _, _, _)` take care of request protection,
challenge generation, and `Authentication-Info` response decoration.
