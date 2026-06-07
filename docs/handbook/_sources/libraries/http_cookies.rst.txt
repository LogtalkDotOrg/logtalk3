.. _library_http_cookies:

``http_cookies``
================

This library implements validation, parsing, and generation of HTTP
``Cookie`` and ``Set-Cookie`` header field values following RFC 6265
syntax. Header values can be represented as atoms, character lists, or
code lists.

The library predicates are defined in the
``http_cookies(_Representation_)`` parametric object where
``_Representation_`` can be one of:

- ``atom`` - cookie texts are represented as atoms
- ``chars`` - cookie texts are represented as lists of characters
- ``codes`` - cookie texts are represented as lists of character codes

The parameter must be bound when sending messages to the object.

``Cookie`` header values are represented as lists of ``Name-Value``
pairs.

``Set-Cookie`` header values are represented by a cookie name, a cookie
value, and a list of attributes.

where ``Attributes`` use ``Key-Value`` notation and can contain:

- ``expires-date_time(Year, Month, Day, Hours, Minutes, Seconds)``
- ``max_age-Seconds``
- ``domain-Domain``
- ``path-Path``
- ``secure-true``
- ``http_only-true``
- ``same_site-lax|strict|none``
- ``partitioned-true``
- ``priority-low|medium|high``
- ``extension-Attribute``

Unknown Set-Cookie attributes are preserved as ``extension-Attribute``
pairs so long as they do not reuse one of the reserved RFC 6265
attribute names.

The ``Expires`` attribute is normalized to a ``date_time/6`` term when
parsing and generated back using the canonical HTTP-date syntax.

The library also provides pure helpers for working with canonical
attribute lists:

- ``normalize_cookie_attributes/2``
- ``cookie_attribute_present/2``
- ``cookie_attribute_value/3-4``
- ``cookie_expiry/2-3``
- ``cookie_deletion/3``

API documentation
-----------------

Open the
`../../apis/library_index.html#http_cookies <../../apis/library_index.html#http_cookies>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_cookies(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_cookies(tester)).
