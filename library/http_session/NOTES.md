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

This library adds explicit reusable client state on top of the stateless
`http_client` library.

The initial implementation provides two public objects:

- `http_cookie_jar`
  explicit in-memory cookie storage and request matching
- `http_session`
  reusable HTTP sessions that automatically replay and store cookies

The design keeps the current `http_client` API stateless. Automatic cookie
persistence is scoped to explicit session handles instead of using hidden
process-global state.

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

Use `http_session` when you need cookie persistence across requests:

	| ?- http_session::open(Session),
	     http_session::get(Session, 'http://127.0.0.1:8080/visits', First, []),
	     http_session::get(Session, 'http://127.0.0.1:8080/visits', Second, []),
	     http_session::close(Session).

Sessions can also reopen a saved cookie jar directly:

	| ?- http_session::open(Session, [cookies_file('cookies.state')]),
	     http_session::get(Session, 'http://127.0.0.1:8080/visits', Response, []),
	     http_session::close(Session).

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
- automatic cookie replay for explicit session handles
- absolute `http://` URLs via the existing `http_client` facade
- core handling for host-only and domain cookies, default path computation,
  secure filtering, `Max-Age`, and normalized `Expires` HTTP-date values

Out of scope for this first slice:

- HTTPS transport support in the client facade
- public suffix enforcement
- SameSite policies
- session-owned connection-pool management
