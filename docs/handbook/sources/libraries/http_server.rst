.. _library_http_server:

``http_server``
===============

The ``http_server`` library provides the user-facing facade for starting
and stopping HTTP and HTTPS servers. It selects a default transport from
the ``scheme/1`` option while still allowing explicit transport
selection when an application needs lower-level control.

By default, ``scheme(http)`` uses ``http_socket_transport`` and
``scheme(https)`` uses ``http_process_transport`` (selecting a TLS
listener). Incompatible scheme, transport, and listener transport
combinations are rejected with a ``consistency_error/3`` exception.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_server <../../apis/library_index.html#http_server>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_server(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_server(tester)).

Bounded serving
---------------

To serve a given number of connections, use the ``serve/4`` predicate.
For example:

::

   | ?- http_server::serve(8080, my_handler, 10, [scheme(http)]).

To inspect or reuse a listener, open it explicitly and close it after
serving:

::

   | ?- http_server::open('127.0.0.1', Port, Server, []),
        http_server::serve(Server, my_handler, 1, ClientInfos),
        http_server::close(Server).

Open-ended serving
------------------

To serve any number of connections until shutdown, use the
``serve_until_shutdown/5`` predicate. For example:

::

   | ?- http_server::serve_until_shutdown('127.0.0.1', 8080, my_handler, control, []).
   ...

   | ?- http_server::request_shutdown(control).
   ...

When the Prolog backend supports threads, you can also use the
``start/4-5`` and ``stop/1`` predicates. For example:

::

   | ?- http_server::start(8080, my_handler, Server, []),
        http_server::stop(Server).

HTTPS
-----

For HTTPS, the server must have TLS credentials. The facade selects
``http_process_transport`` and a TLS listener when ``scheme(https)`` is
used, but the transport still needs either explicit certificate and key
files or a request to create temporary credentials.

Use ``tls_certificate_file/1`` and ``tls_key_file/1`` for real
deployments:

::

   | ?- http_server::serve(
           '127.0.0.1', 8443, my_handler, 10, _ClientInfos,
           [
               scheme(https),
               tls_certificate_file('/path/to/cert.pem'),
               tls_key_file('/path/to/key.pem')
           ]
        ).

Use ``temporary_tls_credentials/1`` for local examples and tests. Its
argument is a file name prefix used for temporary certificate and key
files. The temporary files are created by the process transport and
removed when the listener is closed:

::

   | ?- http_server::serve(
           '127.0.0.1', 8443, my_handler, 10, _ClientInfos,
           [scheme(https), temporary_tls_credentials('logtalk_http_server_')]
        ).

If ``scheme(https)`` is used without either
``temporary_tls_credentials/1`` or both ``tls_certificate_file/1`` and
``tls_key_file/1``, the process transport rejects the listener options.

Handling server requests
------------------------

The handler argument in the examples above, ``my_handler``, is the
object that implements ``http_handler_protocol`` and receives each
accepted HTTP request, building the corresponding HTTP response.

For ordinary HTTP requests, the handler is expected to provide a
``handle/2`` predicate compatible with the ``http_server_core``
request/response pipeline. For example, a minimal handler can
pattern-match the request and return a plain text response:

::

   :- object(my_handler,
       implements(http_handler_protocol)).

       :- uses(http_core, [
           response/6
       ]).

       handle(Request, Response) :-
           Request = request(_Method, _Target, Version, _Headers, _Body, _Properties),
           response(
               Version, status(200, 'OK'), [],
               content('text/plain', text('Hello!')), [], Response
           ).

   :- end_object.

When a server accepts a connection, the selected transport reads the
request, calls ``my_handler::handle(Request, Response)``, writes the
response back to the client, and then follows the selected serving
policy. Parametric objects can be used when the handler needs
configuration, e.g. ``static_site_handler(Root)``.

Options
-------

The predicates that open or start a listener accept the following facade
options:

- ``scheme(Scheme)`` controls the URL scheme served by the listener.
  Valid values: ``http`` or ``https``. Default: ``http``.
- ``transport(Transport)`` selects the transport object. Valid values:
  ``default`` or an object implementing ``http_transport_protocol``.
  Default: ``default``, which resolves to ``http_socket_transport`` for
  ``scheme(http)`` and to ``http_process_transport`` for
  ``scheme(https)``.
- ``listener_options(Options)`` passes additional listener options
  directly to the selected transport. Default: ``[]``.
- ``serve_options(Options)`` passes additional serving options directly
  to the selected transport. Default: ``[]``.
- ``control(Control)`` names the shutdown control used by ``start/4-5``
  and open-ended serving. Valid values: any non-variable term, or
  ``default`` to create a fresh internal control term. Default:
  ``default``.

For convenience, selected listener and serving options can also be
written directly in the facade option list instead of wrapping them in
``listener_options/1`` or ``serve_options/1``:

- Listener options: ``backlog/1``, ``type/1``, ``listener_transport/1``,
  ``listener_helper_executable/1``, ``temporary_tls_credentials/1``,
  ``tls_certificate_file/1``, and ``tls_key_file/1``.
- Serving options: ``workers/1`` and ``shutdown/1``.

The direct listener options accepted by the facade have the following
values: ``backlog/1`` takes a positive integer, ``type/1`` takes
``binary`` or ``text``, ``listener_transport/1`` takes ``tcp`` or
``tls``, ``listener_helper_executable/1`` takes ``ncat`` or ``socat``,
and the TLS credential options take file names or a temporary file name
prefix as atoms.

The direct serving options accepted by the facade are
``workers(serial)``, ``workers(per_connection)``,
``workers(pool(Size))``, ``workers(pool(Size, rolling))``,
``shutdown(keep_open)``, and ``shutdown(close)``.

These direct options are pass-through options. They are checked by the
facade for basic shape, but the selected transport still decides whether
a particular option is supported and what its transport-specific default
is. For example, ``listener_transport(tls)``,
``listener_helper_executable/1``, and the TLS credential options are
options for the process transport. With ``scheme(https)``, the facade
adds ``listener_transport(tls)`` automatically unless another listener
transport is explicitly specified.

When the process transport is selected, these listener defaults apply:
``backlog(5)``, ``type(binary)``, ``listener_transport(tcp)``, and
``listener_helper_executable(ncat)``. The facade changes the effective
listener transport to ``tls`` for ``scheme(https)``. The TLS credential
options have no default usable certificate:
``temporary_tls_credentials/1`` must be requested explicitly, or both
``tls_certificate_file/1`` and ``tls_key_file/1`` must be provided when
opening a TLS listener.

Common serving defaults are inherited from the transports:
``workers(serial)`` and ``shutdown(keep_open)``. The convenience
predicates that open and close a listener around a bounded request loop
add ``shutdown(close)`` while serving.
