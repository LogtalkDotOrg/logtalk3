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
- ``page_fragment_reply/4-5`` helpers for full-page replies on ordinary
  and boosted HTMX requests and fragment replies on non-boosted HTMX
  requests
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
- ``current_url/2``
- ``prompt/2``
- ``target/2``
- ``trigger/2``
- ``trigger_name/2``
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

The supported response-header options are currently:

- ``redirect(URL)``
- ``refresh(Boolean)``
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
