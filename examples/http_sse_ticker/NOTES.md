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

# http_sse_ticker

This example shows the basics of using the high-level `http_sse` library to
accept a Server-Sent Events connection, push a short sequence of named
events, and end the stream. It uses the `transport(http_socket_transport)`
option explicitly because the local listener is opened through the default
HTTP server socket transport.

Server-Sent Events only flow from server to client over a plain HTTP
response, so, unlike the `http_websocket_echo` example, there is no
opening handshake to negotiate and nothing for the server to read back
from the connection; the whole exchange is the server pushing events and
the client reading them.

The example illustrates four basic steps:

1. Accept the connection with `http_sse::accept/4`, advertising a
   reconnection time with the `retry/1` option.
2. Open the client connection with `http_sse::open/3`.
3. Push named events from the server with `http_sse::send_event/3`.
4. Read events on the client with `http_sse::receive/2` until it returns
   `end_of_file`, which `http_sse` reports once the server ends the stream.

Load the example with:

```logtalk
logtalk_load(http_sse_ticker(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_sse_ticker_demo::run(Result).
```

The returned `Result` term contains both the server-side and client-side
views of the same sequence of ticks.

When backend threads are not available, run the server and client in
separate sessions. In the first session start the server:

```logtalk
http_sse_ticker_server::serve(8080, Session).
```

Then, in a second session, connect the client and collect the ticks:

```logtalk
http_sse_ticker_client::run(8080, Session).
```

Study the [http_sse_ticker.lgt](http_sse_ticker.lgt) source file together
with these sample queries. The example is intentionally small so the
high-level SSE accept/open/send/receive flow can be understood without
dropping down to the lower-level request, response, or connection
plumbing libraries.
