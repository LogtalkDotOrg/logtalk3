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

# http_cookies_counter

This example shows how a server can preserve small pieces of state in the
client by sending a `Set-Cookie` header and reading the corresponding
`Cookie` header in a later request.

The important point is that the server controls the cookie contents and their
meaning. The client side in this example now uses the explicit `http_session`
and `http_cookie_jar` library objects so the round-trip is realistic while the
stored cookie state remains inspectable.

The example illustrates four basic steps:

1. Read normalized cookie pairs from the incoming request.
2. Compute the next visit count without keeping any mutable server state.
3. Generate a `Set-Cookie` header value with `http_cookies(atom)::generate_set_cookie/4`.
4. Replay the stored cookie automatically on the next request using one explicit `http_session` and inspect the current jar contents with `http_cookie_jar`.

Load the example with:

```logtalk
logtalk_load(http_cookies_counter(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_cookies_counter_demo::run(Result).
```

The returned `Result` term contains the first response, the cookie pairs that
the session jar stores after that response, and the second response that
reuses the server-provided cookie automatically.

When backend threads are not available, run the server and client in separate
sessions. In the first session start the server for two client requests:

```logtalk
cookie_counter_server::serve(8080, 2).
```

Then, in a second session, run the client workflow:

```logtalk
cookie_counter_client::run(8080, Result).
```

To inspect the cookie round-trip step by step instead of using `run/2`, call:

```logtalk
http_session::open(Session),
cookie_counter_client::visit(8080, Session, FirstResponse, CookiePairs),
cookie_counter_client::visit(8080, Session, SecondResponse, _),
http_session::close(Session).
```

Study the [http_cookies_counter.lgt](http_cookies_counter.lgt) source file
together with these sample queries. The example is intentionally small so the
cookie storage story stays visible: the server derives the next value from the
cookie it previously asked the client to store instead of keeping the counter
locally, while the client delegates persistence and replay to the dedicated
session and cookie-jar library.
