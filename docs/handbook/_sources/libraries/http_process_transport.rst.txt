.. _library_http_process_transport:

``http_process_transport``
==========================

The ``http_process_transport`` library provides an alternative
implementation of the ``http_transport_protocol`` protocol. Based on the
``process`` library and external commands, it provides TLS support for
HTTPS and WSS connections. The current implementation uses
``openssl s_client`` to expose a reusable client-side byte stream
suitable for the existing ``http_client_core`` predicates. The current
server-side implementation defaults to ``ncat`` to own the public
listener socket and expose accepted connections to the parent object
using a loopback relay listener plus peer metadata parsed from helper
diagnostics.

This library requires backend support for the ``process`` and ``socket``
libraries. Sequential client-side transport, including reusable
connections, one-shot client exchanges, and connection pools, depends on
binary process pipes.

When using a backend that doesn't support threads, this library still
supports client-side reusable connections, sequential client pooling,
one-shot and reusable client exchanges, and serial single-connection
HTTPS and WSS serving. Worker-based serving modes and shutdown control
still require backend thread support.

Requirements
------------

The ``openssl`` and ``ncat`` commands must be available on the current
``PATH``.

On macOS, both commands can be installed using e.g. Homebrew:

::

   $ brew install openssl nmap

Or using MacPorts:

::

   $ sudo port install openssl nmap

On Ubuntu, both commands can be installed using:

::

   $ sudo apt install openssl ncat

On RedHat distributions (8.x and later):

::

   $ sudo dnf install openssl nmap-ncat

For older RedHat distributions:

::

   $ sudo yum install openssl nmap-ncat

On Windows, both commands can be installed using e.g. Chocolatey:

::

   > choco install openssl
   > choco install nmap

API documentation
-----------------

Open the
`../../apis/library_index.html#http_process_transport <../../apis/library_index.html#http_process_transport>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_process_transport(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_process_transport(tester)).

Current scope
-------------

- reusable client connections (``open_connection/4``,
  ``close_connection/1``)
- reusable client connection pools (``open_connection_pool/4``,
  ``close_connection_pool/1``)
- reusable-connection and one-shot exchanges (``exchange/3-4``,
  ``exchange_sequence/3-4``)
- helper-backed plain TCP listeners (``open_listener/4``,
  ``close_listener/1``)
- helper-backed TLS listeners (``open_listener/4`` with
  ``listener_transport(tls)``)
- server-side serving (``serve_once/3``, ``serve_websocket_once/5``,
  ``serve_listener/4-5``, ``serve_until_shutdown/4``,
  ``request_shutdown/1``)
- the same public connection and pool handle shapes used by
  ``http_socket_transport`` to keep downstream endpoint validation
  compatible

Current limitations
-------------------

- worker-based serving modes and shutdown control require backend thread
  support

- the ``openssl`` executable must be available on the ``PATH`` unless
  overridden with the ``openssl_executable/1`` option

- server-side listeners require ``ncat`` on the ``PATH`` unless
  overridden with the ``listener_helper_executable/1`` option to use
  ``socat`` instead

- the TLS listener implementation is limited to server-side stream
  exposure and handshake handling; advanced TLS options such as ALPN,
  client certificate verification, and protocol version tuning are not
  provided

Connection options accepted by ``open_connection/4``:

- ``type(binary)``
- ``type(text)``
- ``connection_transport(tcp)``
- ``connection_transport(tls)``
- ``openssl_executable(Executable)``
- ``server_name(Name)``
- ``openssl_arguments(Arguments)``

Notes on those options:

- ``type(binary)`` and ``type(text)`` match the shared
  ``http_socket_transport`` client-side option surface; the selected
  value is applied to the client connection streams after the helper
  process is started
- ``connection_transport(tcp)`` is the default and uses ``ncat`` as the
  helper process
- ``connection_transport(tls)`` uses ``openssl s_client`` for the client
  connection helper process
- the default ``server_name(default)`` value maps to the requested host
  except for colon-containing hosts, where the SNI option is omitted by
  default when ``connection_transport(tls)`` is selected
- ``openssl_arguments/1`` can be used to pass verification, CA, ALPN, or
  client certificate flags directly to ``openssl s_client`` when
  ``connection_transport(tls)`` is selected

Connection-pool options accepted by ``open_connection_pool/4``:

- ``min_size(N)``
- ``max_size(N)``
- ``connection_options(Options)``

Listener options accepted by ``open_listener/4``:

- ``backlog(N)``
- ``type(binary)``
- ``type(text)``
- ``listener_transport(tcp)``
- ``listener_transport(tls)``
- ``listener_helper_executable(Executable)``
- ``temporary_tls_credentials(Prefix)``
- ``tls_certificate_file(File)``
- ``tls_key_file(File)``

Notes on those listener options:

- ``type/1`` is accepted for drop-in compatibility with
  ``http_socket_transport`` but does not change the internal relay
  socket type used by the listener implementation
- ``listener_helper_executable/1`` is a compatibility option name for
  selecting the helper executable used for the public listener and
  loopback relay
- ``temporary_tls_credentials(Prefix)`` generates a temporary
  self-signed certificate and key owned by the listener when
  ``listener_transport(tls)`` is selected; both files are deleted by
  ``close_listener/1`` and by the ``serve_until_shutdown/4-5``
  finalization path
- ``tls_certificate_file/1`` and ``tls_key_file/1`` are required
  together when ``listener_transport(tls)`` is selected unless
  ``temporary_tls_credentials/1`` is used instead
- ``temporary_tls_credentials/1`` is mutually exclusive with
  ``tls_certificate_file/1`` and ``tls_key_file/1``

Helper predicate:

- ``temporary_tls_credentials_files(Prefix, CertificateFile, KeyFile)``
  computes the temporary certificate and key file paths for a given
  prefix without creating those files; this is useful for client-side
  trust configuration when the listener itself owns certificate creation
  via ``temporary_tls_credentials(Prefix)``

Serving options accepted by ``serve_listener/5``:

- ``shutdown(keep_open)``
- ``shutdown(close)``
- ``workers(serial)``
- ``workers(per_connection)``
- ``workers(pool(Size))``
- ``workers(pool(Size, rolling))``

Serving options accepted by ``serve_until_shutdown/4``:

- ``workers(serial)``
- ``workers(per_connection)``
- ``workers(pool(Size))``
- ``workers(pool(Size, rolling))``

Example:

::

   :- initialization((
       logtalk_load(http_process_transport(loader)),
       http_core::request(get, origin('/'), http(1,1), [host-host('example.com')], empty, [], Request),
       http_process_transport::exchange(
           'example.com',
           443,
           Request,
           Response
       ),
       writeq(Response), nl,
       halt
   )).

The one-shot exchange predicates add ``Connection: close`` automatically
when the request does not already specify connection handling, mirroring
the existing ``http_socket_transport`` behavior.

On the server side, a long-lived helper listener keeps ownership of the
public listener socket and relays each accepted connection into a local
loopback listener. Peer addresses are recovered from helper diagnostics,
allowing the parent object to keep returning ``ClientInfo`` terms while
still exposing real binary streams to ``http_server_core`` and the
existing WebSocket layers. The same listener shape is used for both
plain TCP and TLS listeners, which allows the higher-level HTTP and
WebSocket layers to serve HTTPS and WSS requests without depending on
backend-specific SSL predicates.

Live smoke tests
----------------

The normal ``tester.lgt`` driver only runs non-network tests. Opt-in
live smoke tests are provided in ``tests_live.lgt`` and can be run using
``tester_live.lgt``:

::

   | ?- logtalk_load(http_process_transport(tester_live)).

The live smoke tests cover two parts:

- external HTTPS client-side smoke tests against a configurable public
  endpoint
- local loopback server-side smoke tests for plain HTTP, HTTPS, and WSS
  using the ``http_process_transport`` listener path

The client-side live smoke tests default to the proven endpoint:

- host: ``httpbin.org``
- port: ``443``
- path: ``/``

The endpoint can be overridden using environment variables before
running the driver:

- ``LOGTALK_HTTP_SOCKET_PROCESS_LIVE_HOST``
- ``LOGTALK_HTTP_SOCKET_PROCESS_LIVE_PORT``
- ``LOGTALK_HTTP_SOCKET_PROCESS_LIVE_PATH``

These live tests exercise the one-shot exchange path, reusable
connections, sequential reuse on the same connection, and pooled reuse
while keeping the normal automated test suite free of external network
dependencies.

The server-side live smoke tests generate a temporary self-signed
certificate, open local loopback listeners, and then verify:

- plain ``serve_once/3``
- TLS ``serve_once/3``
- WSS ``serve_websocket_once/5`` plus a small frame round-trip on the
  upgraded streams
