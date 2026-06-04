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

This example shows the basics of using the high-level `http_websocket` library
to accept a WebSocket connection, exchange messages, and close the session.

The example illustrates three basic steps:

1. Accept the connection with `http_websocket::accept/4`.
2. Open the client connection with `http_websocket::open/3`.
3. Exchange normalized messages with `http_websocket::send/2` and `http_websocket::receive/2`.

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
the high-level WebSocket open/send/receive flow can be understood without
dropping down to the lower-level handshake, stream, frame, or session
plumbing libraries.
