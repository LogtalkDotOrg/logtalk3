.. _library_smtp:

``smtp``
========

Portable SMTP/ESMTP client using the ``sockets`` library for plaintext
connections and the ``process`` library with OpenSSL for implicit TLS
and STARTTLS connections. Requires a backend supporting sockets.

Requirements
------------

The ``openssl`` (version 3.0.0 or later) command must be available on
the system ``PATH``.

On macOS, this command can be installed using e.g. Homebrew:

::

   $ brew install openssl

Or using MacPorts:

::

   $ sudo port install openssl

On Ubuntu, this command can be installed using:

::

   $ sudo apt install openssl

On RedHat distributions (8.x and later):

::

   $ sudo dnf install openssl

For older RedHat distributions:

::

   $ sudo yum install openssl

On Windows, the ``openssl`` command can be installed using e.g.
Chocolatey:

::

   > choco install openssl

API documentation
-----------------

Open the
`../../apis/library_index.html#smtp <../../apis/library_index.html#smtp>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(smtp(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(smtp(tester)).

The default tests use a loopback mock SMTP server and do not deliver
email.

Usage
-----

The primary API is the stateless ``send/5`` predicate. It opens a
connection, negotiates the SMTP session, sends one message, issues
``QUIT``, and closes all socket or process resources before returning:

::

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

For applications sending several messages in one session, use
``connect/4``, connection-based ``send/4``, and ``disconnect/1``. The
connection handle is opaque and caller-owned; the library does not store
global session state.

Messages and results
--------------------

Messages use the term:

::

   smtp_message(EnvelopeFrom, Recipients, Headers, Body)

``Recipients`` is a mailbox atom or a non-empty list of mailbox atoms.
``Headers`` is a list of ``Name-Value`` pairs. Bodies are text
represented by an atom, a ``chars(Chars)`` term, or a ``codes(Codes)``
term.

All bodies are encoded as UTF-8 and transferred using MIME Base64
encoding. Input CRLF, bare CR, and bare LF line endings are normalized
to CRLF before encoding, and Base64 output is folded at 76 characters
per line. The library adds the following headers when they are not
supplied by the caller:

- ``MIME-Version: 1.0``
- ``Content-Type: text/plain; charset=UTF-8``
- ``Content-Transfer-Encoding: base64``

Caller-supplied MIME headers are preserved when compatible. Header names
are matched case-insensitively. Conflicting or duplicate MIME headers
are rejected before opening a connection. A caller-supplied
``Content-Type`` may select a different media type, such as
``text/html``, but must declare the UTF-8 charset.

Unicode ``Subject`` and ``Comments`` values are automatically serialized
as RFC 2047 UTF-8 Base64 encoded words. Encoded words are limited to 75
characters and long values are folded between encoded words without
splitting a Unicode character. ASCII values, including caller-supplied
encoded words, are preserved unchanged. Other header values remain
restricted to ASCII.

Results use the term:

::

   smtp_result(FinalResponse, AcceptedRecipients, RejectedRecipients)

Responses use ``smtp_response(Code, Lines)``. Rejected recipients are
represented by ``Recipient-smtp_response(Code, Lines)``.
``FinalResponse`` is ``not_sent`` when no recipient was accepted or when
``require_all_recipients(true)`` prevents sending.

Options
-------

Connection options:

- ``security(plain)`` (default), ``security(tls)``, or
  ``security(starttls)``
- ``helo(Name)``
- ``auth(User-Password)``
- ``allow_insecure_auth(Boolean)`` (default ``false``)
- ``openssl_executable(Executable)`` (default ``openssl``)
- ``server_name(default)``, ``server_name(none)``, or
  ``server_name(Name)``
- ``verify_peer(Boolean)`` (default ``true``)
- ``ca_file(File)``
- ``openssl_arguments(Arguments)``

Transaction options:

- ``require_all_recipients(Boolean)`` (default ``false``)
- ``header(Name, Value)``, which may be repeated

Port numbers never select a security mode automatically.

Security
--------

Implicit TLS and STARTTLS use ``openssl s_client``. STARTTLS uses
OpenSSL's ``-starttls smtp`` support: OpenSSL consumes the initial
plaintext greeting, EHLO, and STARTTLS exchange, after which this
library sends the required EHLO over the encrypted connection. The
OpenSSL interactive command interpreter is disabled so that SMTP
commands such as ``RCPT``, ``RSET``, and ``QUIT`` are forwarded
unchanged.

Peer and hostname verification are enabled by default. Authentication
supports the advertised PLAIN and LOGIN mechanisms, preferring PLAIN.
Authentication on a plaintext connection is rejected unless
``allow_insecure_auth(true)`` is explicitly specified.

Limitations
-----------

The current version does not generate RFC 2047 encoded words for display
names, structured fields, or arbitrary extension fields. MIME multipart
bodies, attachments, SMTPUTF8 envelopes, quoted-printable transfer
encoding, 8BITMIME, PIPELINING, CHUNKING, DSN, automatic retries,
connection pooling, and operation timeouts are not implemented.
Automatic retry after ``DATA`` is deliberately omitted because loss of
the final reply makes delivery status ambiguous and retrying can
duplicate mail.
