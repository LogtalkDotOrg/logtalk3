.. _library_http_session:

``http_session``
================

This library adds explicit reusable client state on top of the stateless
``http_client`` library.

The initial implementation provides two public objects:

- ``http_cookie_jar`` explicit in-memory cookie storage and request
  matching
- ``http_session`` reusable HTTP sessions that automatically replay and
  store cookies

The design keeps the current ``http_client`` API stateless. Automatic
cookie persistence is scoped to explicit session handles instead of
using hidden process-global state.

API documentation
-----------------

Open the `../../docs/index.html <../../docs/index.html>`__ file in a web
browser and choose the libraries index and then the ``http_session``
library.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_session(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_session(tester)).

Usage overview
--------------

Use ``http_client`` for one-shot stateless calls:

::

   | ?- http_client::get('http://127.0.0.1:8080/resource', Response, []).

Use ``http_session`` when you need cookie persistence across requests:

::

   | ?- http_session::open(Session),
        http_session::get(Session, 'http://127.0.0.1:8080/visits', First, []),
        http_session::get(Session, 'http://127.0.0.1:8080/visits', Second, []),
        http_session::close(Session).

Sessions can also reopen a saved cookie jar directly:

::

   | ?- http_session::open(Session, [cookies_file('cookies.state')]),
        http_session::get(Session, 'http://127.0.0.1:8080/visits', Response, []),
        http_session::close(Session).

You can also work directly with the cookie jar:

::

   | ?- http_cookie_jar::open(Jar),
        http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-'/'])]),
        http_cookie_jar::request_cookies(Jar, 'http://example.com/dashboard', Cookies).

Cookie jars can also be explicitly saved and restored:

::

   | ?- http_cookie_jar::open(Jar),
        http_cookie_jar::store_set_cookies(Jar, 'http://example.com/login', [set_cookie(session, '1', [path-'/'])]),
        http_cookie_jar::save(Jar, 'cookies.state'),
        http_cookie_jar::close(Jar),
        http_cookie_jar::open(RestoredJar, [cookies_file('cookies.state')]).

The ``cookies_file/1`` session option is a convenience for opening a
fresh owned cookie jar from disk. It is mutually exclusive with the
``cookie_jar/1`` session option.

Current scope:

- in-memory cookie storage with explicit save and load support
- automatic cookie replay for explicit session handles
- absolute ``http://`` URLs via the existing ``http_client`` facade
- core handling for host-only and domain cookies, default path
  computation, secure filtering, ``Max-Age``, and normalized ``Expires``
  HTTP-date values

Out of scope for this first slice:

- HTTPS transport support in the client facade
- public suffix enforcement
- SameSite policies
- session-owned connection-pool management
