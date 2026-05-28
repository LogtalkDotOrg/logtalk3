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


`http_session`
==============

This library provides explicit client-side and server-side HTTP session
support on top of the normalized HTTP stack.

The library provides these public entities:

- `http_cookie_jar`
	explicit in-memory cookie storage and request matching for client-side use
- `http_client_session`
	reusable HTTP client sessions that automatically replay and store cookies
- `http_server_session`
	server-side session manager over normalized request and response terms
- `http_server_session_handler(_, _)`
	portable handler wrapper that applies server-session begin and finish logic
- `http_router_server_session(_)`
	router middleware helpers for request annotation and response finalization

The design keeps the current `http_client` API stateless. Automatic cookie
persistence is scoped to explicit client-session handles, while server-side
session state stays in an explicit in-memory manager keyed by opaque cookie
identifiers.

This library can be used with backend Prolog systems that supports the
`sockets` library: ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog,
Trealla Prolog, and XVM.


API documentation
-----------------

Open the [../../apis/library_index.html#http_session](../../apis/library_index.html#http_session)
link in a web browser.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(http_session(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(http_session(tester)).


Usage overview
--------------

Use `http_client` for one-shot stateless calls:

	| ?- http_client::get('http://127.0.0.1:8080/resource', Response, []).

Use `http_client_session` when you need cookie persistence across requests:

	| ?- http_client_session::open(Session),
	     http_client_session::get(Session, 'http://127.0.0.1:8080/visits', First, []),
	     http_client_session::get(Session, 'http://127.0.0.1:8080/visits', Second, []),
	     http_client_session::close(Session).

Sessions can also reopen a saved cookie jar directly:

	| ?- http_client_session::open(Session, [cookies_file('cookies.state')]),
	     http_client_session::get(Session, 'http://127.0.0.1:8080/visits', Response, []),
	     http_client_session::close(Session).

Use `http_server_session` when you need explicit server-side session state over
normalized request and response terms:

	| ?- http_server_session::open(Manager),
	     http::request(get, origin('/visits'), http(1, 1), [], empty, [], Request0),
	     http_server_session::begin(Manager, Request0, Request),
	     http_server_session::current(Request, Session),
	     http_server_session::set(Session, visits, 1),
	     http::response(http(1, 1), status(200, 'OK'), [], empty, [], Response0),
	     http_server_session::finish(Request, Response0, Response),
	     http_server_session::close(Manager).

You can also work directly with the cookie jar:

	| ?- http_cookie_jar::open(Jar),
	     http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-'/'])]),
	     http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies).

Cookie jars can also be explicitly saved and restored:

	| ?- http_cookie_jar::open(Jar),
	     http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-'/'])]),
	     http_cookie_jar::save(Jar, 'cookies.state'),
	     http_cookie_jar::close(Jar),
	     http_cookie_jar::open(RestoredJar, [cookies_file('cookies.state')]).

The `cookies_file/1` session option is a convenience for opening a fresh owned
cookie jar from disk. It is mutually exclusive with the `cookie_jar/1` session
option.

Current scope:

- in-memory cookie storage with explicit save and load support
- automatic cookie replay for explicit client-session handles
- in-memory server-side session storage keyed by opaque cookie identifiers
- direct server-session request begin/finish operations plus plain-handler and router adapters
- absolute `http://` URLs via the existing `http_client` facade
- core handling for host-only and domain cookies, default path computation,
  secure filtering, `Max-Age`, and normalized `Expires` HTTP-date values

Out of scope for this first slice:

- HTTPS transport support in the client facade
- public suffix enforcement
- SameSite policies
- persistent or distributed server-session stores
- session-owned connection-pool management
