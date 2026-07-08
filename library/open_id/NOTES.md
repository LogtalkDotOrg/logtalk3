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
URL construction, authorization response parsing and state validation,
authorization-code and refresh-token exchanges, UserInfo requests, RP-initiated
logout URL construction, JWKS retrieval and caching, and ID-token verification
for `RS256` and `ES256`.

The library uses `http_client(http_process_transport)` for HTTP requests. HTTPS
requests are made using `http_process_transport` TLS connections, which require
the `openssl` command to be available unless overridden with the
`openssl_executable/1` option.

This library can be used with backend Prolog systems that support unbound
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

To run the optional live tests against Google's public OpenID Connect
discovery and JWKS endpoints, load the `tester_live.lgt` file:

	| ?- logtalk_load(open_id(tester_live)).

The Google discovery and JWKS live tests do not require credentials. The
optional Google ID-token verification live test requires a fresh Google-issued
ID token and its expected audience:

	$ export LOGTALK_OPEN_ID_GOOGLE_ID_TOKEN="..."
	$ export LOGTALK_OPEN_ID_GOOGLE_AUDIENCE="..."

If the ID token includes a nonce claim, also define the
`LOGTALK_OPEN_ID_GOOGLE_NONCE` environment variable.

The live tests also include an end-to-end Authorization Code + PKCE flow
against the community-run `https://oidctest.wsweet.org/` OpenID Connect test
provider, using its documented test account `dwho`/`dwho` and pre-registered
client `private`/`tardis`. This provider has no availability guarantees.


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

After redirecting the user to `URL` and receiving the callback URL:

	| ?- open_id::authorization_code(CallbackURL, Session, Code, []),
	     open_id::exchange_code(
	         Provider,
	         Code,
	         Session,
	         Tokens,
	         [client_authentication(client_secret_basic('client-secret'))]
	     ),
	     Tokens = tokens(Properties),
	     member(id_token(IDToken), Properties),
	     open_id::verify_id_token(IDToken, Provider, Claims,
	         [expected_audience('client-id')]).

To call the UserInfo endpoint with the resulting access token:

	| ?- Tokens = tokens(Properties),
	     member(access_token(AccessToken), Properties),
	     open_id::userinfo(Provider, AccessToken, UserInfo, []).

To use a refresh token:

	| ?- open_id::refresh_token(
	         Provider,
	         RefreshToken,
	         RefreshedTokens,
	         [
	             client_id('client-id'),
	             client_authentication(client_secret_basic('client-secret'))
	         ]
	     ).

To build an RP-initiated logout URL:

	| ?- open_id::logout_url(
	         Provider,
	         logout_request([
	             id_token_hint(IDToken),
	             post_logout_redirect_uri('https://client.example/logout/callback'),
	             state('logout-state')
	         ]),
	         LogoutURL,
	         []
	     ).


Current scope
-------------

- OpenID Provider discovery.
- Authorization Code + PKCE (`S256`) URL construction.
- Authorization response parsing and state validation.
- Authorization-code token exchange for public clients,
  `client_secret_post/1`, and `client_secret_basic/1`.
- Refresh-token exchange and UserInfo retrieval.
- RP-initiated logout URL construction.
- JWKS retrieval, caching, and refresh-on-unknown-`kid` key rotation.
- ID-token JWT verification for `RS256` and `ES256` using OpenSSL.
- Claim validation for issuer, subject, audience, authorized party, nonce,
  expiration, not-before, and issued-at tolerance.


Current limitations
-------------------

- Device authorization, dynamic client registration, browser launching, and
  callback server ownership are outside the current scope.
- ID-token verification requires the `openssl` command.
