.. _library_open_ai:

``open_ai``
===========

The ``open_ai`` library provides a portable OpenAI-compatible public
non-admin API surface. It uses the existing HTTP, REST, and OpenAPI
libraries and defaults to the ``http_socket_process`` transport for
HTTPS and WSS support.

The library implements the OpenAI wire-format routing, client request
construction, OpenAPI document generation, and validation hooks.
Server-side model, file, eval, tool, realtime, and media behavior is
supplied by an application backend object implementing
``open_ai_backend_protocol``.

Requires a backend supporting sockets and unbound integer arithmetic due
to the library dependencies. Supported backends include ECLiPSe, SICStus
Prolog, SWI-Prolog, Trealla Prolog, and XVM.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(open_ai(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(open_ai(tester)).

Current scope
-------------

- Public non-admin endpoints from the pinned official OpenAI OpenAPI
  ``2.3.0`` document.
- Organization and project administration paths are excluded.
- The generated catalog currently covers 161 operations.
- Request and response schemas are intentionally permissive in this
  first catalog pass; endpoint coverage, routing, OpenAPI document
  generation, and client URL/header construction are the primary
  compatibility surface.

Basic client usage
------------------

::

   | ?- logtalk_load(open_ai(loader)).
   ...

   | ?- open_ai_client::request(
           createResponse,
           [],
           content('application/json', json({model-'gpt-4.1', input-'Hello'})),
           Response,
           [api_key('sk-...')]
        ).

Basic server usage
------------------

Define a backend object implementing ``open_ai_backend_protocol``:

::

   :- object(my_backend,
       implements(open_ai_backend_protocol)).

       handle_open_ai(OperationId, Request, Result) :-
           % inspect OperationId and Request, then return a REST result term
           Result = ok({operation-OperationId}).

   :- end_object.

Then dispatch normalized HTTP requests through:

::

   open_ai_server(my_backend)::handle(Request, Response).
