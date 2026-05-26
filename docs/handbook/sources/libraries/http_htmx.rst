.. _library_http_htmx:

``http_htmx``
=============

The ``http_htmx`` library provides router-agnostic helpers for HTMX
request classification, HTML reply construction, and HTMX
response-header decoration using normalized ``http`` request and
response terms.

Currently this library provides:

- HTMX request detection helpers
- ``reply/3-4`` helpers for pre-rendered HTML atoms and ``html``
  terms/lists
- ``page_fragment_reply/4-5`` helpers for full-page replies on ordinary,
  boosted, and history-restore HTMX requests and fragment replies on
  other HTMX requests
- ``add_response_headers/4`` for the common ``HX-*`` response headers
- the companion ``http_router_htmx`` category for router middleware
  integration

API documentation
-----------------

Open the
`../../apis/library_index.html#http_htmx <../../apis/library_index.html#http_htmx>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_htmx(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_htmx(tester)).

Current scope
-------------

The current library provides one public object and one companion
category:

- object ``http_htmx``
- category ``http_router_htmx``

The main public predicates are:

- ``is_htmx_request/1``
- ``is_boosted_request/1``
- ``is_history_restore_request/1``
- ``is_fragment_request/1``
- ``request_kind/2``
- ``current_url/2``
- ``current_url_abs_path/2``
- ``prompt/2``
- ``target/2``
- ``trigger/2``
- ``trigger_name/2``
- ``request_property/2``
- ``request_properties/2``
- ``reply/3``
- ``reply/4``
- ``page_fragment_reply/4``
- ``page_fragment_reply/5``
- ``add_response_headers/4``

The companion router category provides these middleware handler
predicates:

- ``annotate_htmx_request/2``
- ``add_htmx_response_headers/3``

For handlers that only need one HTMX request flag or value, prefer
``request_property/2``. Use ``request_properties/2`` when you need the
full derived property list, for example when annotating a request with
all HTMX metadata. The derived property set now also includes
``htmx_request_kind(Kind)``, mirroring ``request_kind/2`` for middleware
and other property-oriented consumers.

For handlers that need the same full-page versus fragment decision used
by ``page_fragment_reply/4-5``, prefer ``is_fragment_request/1`` instead
of repeating the ``HX-Request``, ``HX-Boosted``, and
``HX-History-Restore-Request`` checks.

For handlers that need one mutually exclusive classification for control
flow or presentation, prefer ``request_kind/2`` instead of open-coding
precedence between ordinary, fragment, boosted, and history-restore
requests.

For handlers that need a same-origin absolute-path form of
``HX-Current-URL``, prefer ``current_url_abs_path/2``. It returns the
path plus optional query and fails when the current URL does not match
the request scheme and host information.

The supported response-header options are currently:

- ``redirect(URL)``
- ``refresh(Boolean)``
- ``vary_hx_request(Boolean)``
- ``location(Value)``
- ``push_url(Value)``
- ``replace_url(Value)``
- ``reswap(Value)``
- ``retarget(Value)``
- ``reselect(Value)``
- ``trigger(Value)``
- ``trigger_after_settle(Value)``
- ``trigger_after_swap(Value)``

HTMX response headers are only added for non-``3xx`` responses. When a
request is an HTMX request and the selected options would emit one or
more ``HX-*`` response headers, attempting to combine those options with
a ``3xx`` response status raises an error instead of silently generating
headers that HTMX will ignore. If you need a normal HTTP redirect, use
the standard response headers directly.

When an endpoint can return both a full-page reply and an HTMX fragment
for the same URL, set ``vary_hx_request(true)`` so the helpers merge
``Vary: HX-Request`` with any existing ``Vary`` metadata. Otherwise,
browser or intermediary caches can reuse the wrong representation.

History restore requests after an HTMX history-cache miss are expected
to return the full page. The ``page_fragment_reply/4-5`` helpers
therefore treat ``HX-History-Restore-Request`` the same as ordinary or
boosted navigation. If your application uses ``HX-Request`` to select
between page and fragment representations, configure the client with
``htmx.config.historyRestoreAsHxRequest`` set to ``false`` so that
history restores do not also carry ``HX-Request``.

Structured values for ``location/1`` and the trigger options reuse the
repository JSON-term conventions through the shared HTTP JSON helpers,
with HTMX-specific validation of the top-level shape:

- ``location/1`` accepts either a URL atom or a JSON object with a
  required ``path`` key and only the documented HTMX optional keys
- ``trigger/1``, ``trigger_after_settle/1``, and
  ``trigger_after_swap/1`` accept either a single event atom, a list of
  event atoms, or a JSON object whose top-level keys are event names

The router companion category expects HTMX response options to be
provided as ``htmx_response_options(Options)`` request metadata or
response properties.
