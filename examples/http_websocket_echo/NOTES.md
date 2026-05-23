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

# http_websocket_echo

This example shows the basics of using the HTTP and WebSocket libraries to
upgrade an HTTP connection and exchange WebSocket messages.

The example illustrates four basic steps:

1. Accept the HTTP Upgrade handshake with `http_server::accept_websocket/3`.
2. Open a client connection with `http_client::open_websocket/4`.
3. Extract the upgraded connection streams with `http_socket::connection_streams/3`.
4. Read and write normalized WebSocket messages with `http_websocket_messages`.

Load the example with:

```logtalk
logtalk_load(http_websocket_echo(loader)).
```

When backend threads are available, run the complete self-contained demo:

```logtalk
http_websocket_echo_demo::run(Result).
```

The returned `Result` term contains both the server-side and client-side
views of the same WebSocket exchange.

When backend threads are not available, run the server and client in
separate sessions. In the first session start the server:

```logtalk
websocket_echo_server::serve(8080, Session).
```

Then, in a second session, connect the client and exchange one message:

```logtalk
websocket_echo_client::run(8080, hello, Session).
```

Study the [http_websocket_echo.lgt](http_websocket_echo.lgt) source file
together with these sample queries. The example is intentionally small so
the handshake, upgraded connection handling, and message exchange logic can
be understood without extra protocol machinery.
