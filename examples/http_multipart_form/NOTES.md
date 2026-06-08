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

# http_multipart_form

This example shows a small HTTP server and HTTP client using the
`http_multipart` library for plain HTML form handling.

The example illustrates six basic steps:

1. Define one handler object that imports `http_router` and exposes a simple
   `GET /` form route plus a `POST /submit` form-processing route.
2. Render an ordinary HTML page containing a form with
   `enctype="multipart/form-data"`.
3. Let the server receive the normalized multipart request body.
4. Extract the submitted `name` and `email` fields using
   `http_multipart::fields/2`.
5. Return a plain HTML confirmation page showing the submitted values.
6. Exercise the whole flow through a small direct HTTP client and a
   self-contained demo object.

Load the example with:

```logtalk
logtalk_load(http_multipart_form(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_multipart_form_demo::run(Result).
```

The returned `Result` term contains two normalized responses:

1. the form page served from `/`
2. the confirmation page returned by the multipart form submission to `/submit`

When backend threads are not available, run the server and client in separate
sessions. In the first session start the server for two client connections:

```logtalk
multipart_form_server::serve(8080, 2).
```

Then, in a second session, fetch the form and submit it:

```logtalk
multipart_form_client::fetch_form(8080, FormResponse).
multipart_form_client::submit_form(8080, 'Ada Lovelace', 'ada@example.com', SubmitResponse).
multipart_form_client::run(8080, 'Ada Lovelace', 'ada@example.com', Result).
```

The example also supports direct in-memory handler queries without starting a
listener. For example, fetch the form page directly:

```logtalk
Request = request(get, origin('/'), http(1, 1), [], empty, []),
multipart_form_http_handler::handle(Request, Response).
```

This returns a `200 OK` response whose body contains the plain HTML form.

You can also build a normalized multipart request body explicitly and submit it
directly to the handler:

```logtalk
http_multipart::form_data_body([
    field(name, 'Ada Lovelace', []),
    field(email, 'ada@example.com', [])
], Body),
Request = request(post, origin('/submit'), http(1, 1), [], Body, []),
multipart_form_http_handler::handle(Request, Response).
```

This returns a `200 OK` response with a confirmation page showing the submitted
name and email values.

To explore the example with a web browser, start the server:

```logtalk
multipart_form_server::serve(8080, 20).
```

Then open `http://127.0.0.1:8080/` in a browser, fill in the form, and submit
it.

Study the [http_multipart_form.lgt](http_multipart_form.lgt) source file
together with these sample queries. The example is intentionally small so the
plain form page, normalized multipart request body, field extraction, and
client workflow all stay visible at once.
