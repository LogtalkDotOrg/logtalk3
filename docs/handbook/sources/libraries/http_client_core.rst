.. _library_http_client_core:

``http_client_core``
====================

The ``http_client_core`` library provides low-level stream primitives
that are transport-neutral and used by ``http_socket`` the library.

This library can be used with backend Prolog systems that support
unbound integer arithmetic and the ``sockets`` library: ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_client_core <../../apis/library_index.html#http_client_core>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_client_core(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_client_core(tester)).

Current scope
-------------

The ``http_client_core`` object provides the following stream-based
predicates:

- ``write_request/2``
- ``read_response/2``
- ``exchange/4``
- ``exchange_sequence/4``

Framing rules
-------------

The ``http_client_core`` object supports response framing for:

- status codes that never carry a body (``1xx``, ``204``, ``205``, and
  ``304``)
- fixed-length bodies via ``Content-Length``
- chunked bodies via ``Transfer-Encoding: chunked``, including trailers
- close-delimited bodies terminated by end-of-file

Request-aware exchanges performed by ``http_client_core::exchange/4``
and ``http_client_core::exchange_sequence/4`` also support ``HEAD``
responses. In that case the normalized response body is ``empty`` and
the response is annotated with the metadata properties
``body_omitted(head)`` and, when applicable,
``omitted_body_length(Length)``.

Close-delimited responses returned by
``http_client_core::read_response/2`` and
``http_client_core::exchange/4`` are annotated with the property
``body_framing(close_delimited)``. Sequential exchanges can only use a
close-delimited response as the final response in the sequence because
the connection is consumed while reading the body.

Current limitations
-------------------

- Only the transport coding sequence ``[chunked]`` is recognized when
  reading streamed response bodies in ``http_client_core``.
- Close-delimited response bodies can only be used as the final response
  in an ``http_client_core::exchange_sequence/4`` sequence.
- The ``http_client_core`` object assumes binary streams for both input
  and output.
