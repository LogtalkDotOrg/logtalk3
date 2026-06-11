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


`http_websocket_handshake`
==========================

This library provides lower-level opening-handshake predicates in the
`http_websocket_handshake` object that is used by other libraries.
Requires a backend supporting unbound integer arithmetic.

API documentation
-----------------

Open the [../../apis/library_index.html#http_websocket_handshake](../../apis/library_index.html#http_websocket_handshake)
link in a web browser.


Loading
-------

To load the full library, load the `loader.lgt` file:

	| ?- logtalk_load(http_websocket_handshake(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_websocket_handshake(tester)).


Current scope
-------------

The current implementation provides two predicates:

- `websocket_opening_key/1` for generating canonical base64-encoded opening keys.
- `websocket_accept/2` for computing canonical `Sec-WebSocket-Accept` values from valid opening keys.


Current workflow
----------------

Use the `websocket_opening_key/1` and `websocket_accept/2` predicates only
when you need the raw opening-handshake values without opening a connection.
