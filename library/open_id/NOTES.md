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


`open_id`
=========

The `open_id` library provides a portable OpenID Connect client for the
Authorization Code + PKCE flow. It supports provider discovery, authorization
URL construction, authorization-code token exchange, JWKS retrieval, and
ID-token verification for `RS256` and `ES256`.

The library uses `http_client(http_socket_process)` for HTTP requests. HTTPS
requests are made using `http_socket_process` TLS connections, which require
the `openssl` command to be available unless overridden with the
`openssl_executable/1` option.

This library can be used with backend Prolog systems that supports unbound
integer arithmetic and the `sockets` library: ECLiPSe, SICStus Prolog,
SWI-Prolog, Trealla Prolog, and XVM.


Loading
-------

To load the library, load the `loader.lgt` file:

	| ?- logtalk_load(open_id(loader)).


Testing
-------

To test this library, load the `tester.lgt` file:

	| ?- logtalk_load(open_id(tester)).


Basic usage
-----------

	| ?- open_id::discover('https://issuer.example', Provider, []),
	     open_id::authorization_url(
	         Provider,
	         authorization_request([
	             client_id('client-id'),
	             redirect_uri('https://client.example/callback'),
	             scope([openid, profile])
	         ]),
	         URL,
	         Session,
	         []
	     ).

After redirecting the user to `URL` and receiving an authorization code:

	| ?- open_id::exchange_code(Provider, Code, Session, Tokens, []),
	     open_id::jwks(Provider, JWKSet, []),
	     Tokens = tokens(Properties),
	     member(id_token(IDToken), Properties),
	     open_id::verify_id_token(IDToken, Provider, JWKSet, Claims,
	         [client_id('client-id')]).


Current scope
-------------

- OpenID Provider discovery.
- Authorization Code + PKCE (`S256`) URL construction.
- Authorization-code token exchange for public clients and
  `client_secret_post/1`.
- JWKS retrieval and key selection.
- ID-token JWT verification for `RS256` and `ES256` using OpenSSL.
- Claim validation for issuer, audience, authorized party, nonce, expiration,
  not-before, and issued-at tolerance.


Current limitations
-------------------

- Refresh-token handling, UserInfo, logout, device authorization, dynamic client
  registration, browser launching, and callback server ownership are outside
  the first release scope.
- JWKS caching is left to applications.
- ID-token verification requires the `openssl` command.

