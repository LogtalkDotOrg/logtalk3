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


`smtp`
======

Portable SMTP/ESMTP client using the `sockets` library for plaintext
connections and the `process` library with OpenSSL for implicit TLS and
STARTTLS connections. Requires a backend supporting sockets.


API documentation
-----------------

Open the [../../apis/library_index.html#smtp](../../apis/library_index.html#smtp)
link in a web browser.


Loading
-------

To load all entities in this library, load the `loader.lgt` file:

	| ?- logtalk_load(smtp(loader)).


Testing
-------

To test this library predicates, load the `tester.lgt` file:

	| ?- logtalk_load(smtp(tester)).

The default tests use a loopback mock SMTP server and do not deliver email.


Usage
-----

The primary API is the stateless `send/5` predicate. It opens a connection,
negotiates the SMTP session, sends one message, issues `QUIT`, and closes all
socket or process resources before returning:

	| ?- Message = smtp_message(
	         'alice@example.com',
	         ['bob@example.com'],
	         ['Subject'-'Hello'],
	         'Olá, mundo!'
	     ),
	     smtp::send('smtp.example.com', 587, Message, Result, [
	         security(starttls),
	         auth('alice@example.com'-'secret')
	     ]).

For applications sending several messages in one session, use `connect/4`,
connection-based `send/4`, and `disconnect/1`. The connection handle is opaque
and caller-owned; the library does not store global session state.


Messages and results
--------------------

Messages use the term:

	smtp_message(EnvelopeFrom, Recipients, Headers, Body)

`Recipients` is a mailbox atom or a non-empty list of mailbox atoms. `Headers`
is a list of `Name-Value` pairs. Bodies are text represented by an atom, a
`chars(Chars)` term, or a `codes(Codes)` term.

All bodies are encoded as UTF-8 and transferred using MIME Base64 encoding.
Input CRLF, bare CR, and bare LF line endings are normalized to CRLF before
encoding, and Base64 output is folded at 76 characters per line. The library
adds the following headers when they are not supplied by the caller:

- `MIME-Version: 1.0`
- `Content-Type: text/plain; charset=UTF-8`
- `Content-Transfer-Encoding: base64`

Caller-supplied MIME headers are preserved when compatible. Header names are
matched case-insensitively. Conflicting or duplicate MIME headers are rejected
before opening a connection. A caller-supplied `Content-Type` may select a
different media type, such as `text/html`, but must declare the UTF-8 charset.

Results use the term:

	smtp_result(FinalResponse, AcceptedRecipients, RejectedRecipients)

Responses use `smtp_response(Code, Lines)`. Rejected recipients are represented
by `Recipient-smtp_response(Code, Lines)`. `FinalResponse` is `not_sent` when no
recipient was accepted or when `require_all_recipients(true)` prevents sending.


Options
-------

Connection options:

- `security(plain)` (default), `security(tls)`, or `security(starttls)`
- `helo(Name)`
- `auth(User-Password)`
- `allow_insecure_auth(Boolean)` (default `false`)
- `openssl_executable(Executable)` (default `openssl`)
- `server_name(default)`, `server_name(none)`, or `server_name(Name)`
- `verify_peer(Boolean)` (default `true`)
- `ca_file(File)`
- `openssl_arguments(Arguments)`

Transaction options:

- `require_all_recipients(Boolean)` (default `false`)
- `header(Name, Value)`, which may be repeated

Port numbers never select a security mode automatically.


Security
--------

Implicit TLS and STARTTLS use `openssl s_client`. STARTTLS uses OpenSSL's
`-starttls smtp` support: OpenSSL consumes the initial plaintext greeting,
EHLO, and STARTTLS exchange, after which this library sends the required EHLO
over the encrypted connection.

Peer and hostname verification are enabled by default. Authentication supports
the advertised PLAIN and LOGIN mechanisms, preferring PLAIN. Authentication on
a plaintext connection is rejected unless `allow_insecure_auth(true)` is
explicitly specified.


Limitations
-----------

The current version supports UTF-8 MIME text bodies. Header names and values,
including subjects and display names, remain restricted to ASCII; RFC 2047
encoded words are not generated. MIME multipart bodies, attachments, SMTPUTF8
envelopes, quoted-printable transfer encoding, 8BITMIME, PIPELINING, CHUNKING,
DSN, automatic retries, connection pooling, and operation timeouts are not
implemented. Automatic retry after `DATA` is deliberately omitted because loss
of the final reply makes delivery status ambiguous and retrying can duplicate
mail.
