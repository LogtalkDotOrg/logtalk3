.. _library_http_multipart:

``http_multipart``
==================

This library provides a higher-level multipart helper layer on top of
the ``http`` core library. It does not replace the low-level multipart
wire support already implemented in ``http``; instead it wraps the
normalized multipart body and part terms, adds form-data field and file
helpers, and delegates raw parsing and generation to the existing
``http`` predicates.

API documentation
-----------------

Open the `../../docs/index.html <../../docs/index.html>`__ file in a web
browser and choose the libraries index and then the ``http_multipart``
library.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_multipart(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_multipart(tester)).

Current scope
-------------

The current slice provides:

- ``body/3`` for constructing validated normalized multipart body terms.
- ``part/4`` for constructing validated normalized multipart part terms.
- ``is_body/1`` and ``is_part/1`` for testing normalized multipart body
  and part terms.
- ``media_type/2`` and ``parts/2`` for inspecting multipart body terms.
- ``part_headers/2``, ``part_body/2``, and ``part_properties/2`` for
  inspecting multipart part terms.
- ``field/3`` and ``fields/2`` for inspecting textual form-data field
  parts and multipart/form-data bodies.
- ``file/5`` and ``files/2`` for inspecting form-data file parts and
  multipart/form-data bodies.
- ``field_part/3`` and ``file_part/5`` for constructing outgoing
  form-data field and file parts.
- ``form_data_body/2`` for constructing a ``multipart/form-data`` body
  from an ordered list of field and file descriptors.
- ``parse/4`` and ``generate/3`` as multipart-specific wrappers around
  the existing ``http::parse_body/4`` and ``http::generate_body/3``
  predicates.

Normalized terms
----------------

Multipart bodies use the normalized ``http`` body term:

::

   content(MediaType, multipart(Parts))

Multipart parts use the recursive normalized term:

::

   part(Headers, Body, Properties)

Current workflow
----------------

- Use ``form_data_body/2`` when you want to assemble a normalized
  ``multipart/form-data`` body explicitly.
- Use the ``field/3``, ``fields/2``, ``file/5``, and ``files/2``
  predicates when you want to inspect multipart form-data bodies after
  parsing.
- The ``http_client`` library can send common form-data requests
  directly from a ``form_data(Items)`` descriptor.
- The ``http_server`` library keeps multipart request bodies in the
  normalized form expected by this library, so handlers can inspect
  incoming multipart requests without reparsing them.

Current limitations
-------------------

- This slice is entirely in-memory.
- The form-data helpers only cover the common
  ``Content-Disposition: form-data`` parameters needed for ``name`` and
  ``filename`` extraction and construction.
- Textual field helpers target simple text field payloads. If a part has
  a non-text body or a more specialized encoding, callers should use the
  generic part accessors instead.
- Multipart wire parsing and generation still require a boundary
  parameter in the effective ``Content-Type`` metadata or an explicit
  ``boundary/1`` option, exactly as in the underlying ``http`` library.
- The library does not yet provide streaming sinks, temp-file policies,
  or large-upload orchestration.
