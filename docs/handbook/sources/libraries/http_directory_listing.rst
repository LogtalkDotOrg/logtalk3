.. _library_http_directory_listing:

``http_directory_listing``
==========================

The ``http_directory_listing`` library provides a router-agnostic helper
for serving HTML directory listings from a configured document root
using normalized ``http_core`` request and response terms.

This library resolves a relative request path against a document root,
canonicalizes the joined target path using ``os::absolute_file_name/2``,
rejects canonicalized escapes with the same outward ``404 Not Found``
response used for missing directories, and renders a simple HTML5
listing for existing directory targets.

API documentation
-----------------

Open the
`../../apis/library_index.html#http_directory_listing <../../apis/library_index.html#http_directory_listing>`__
link in a web browser.

Loading
-------

To load the library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(http_directory_listing(loader)).

Testing
-------

To test this library, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(http_directory_listing(tester)).

Current scope
-------------

The current version provides one public object with two public
predicates:

- ``serve/4``
- ``serve/5``

Supported options are:

- ``dot_files(Boolean)``
- ``directories_first(Boolean)``
- ``sort_by(SortBy)``
- ``sort_order(SortOrder)``
- ``columns(Columns)``
- ``type_display(TypeDisplay)``
- ``size_display(SizeDisplay)``
- ``title(Title)``
- ``theme(Theme)``
- ``stylesheets(Stylesheets)``
- ``exclude(Exclusions)``
- ``cache_control(Directives)``
- ``expires(Expires)``

Default options are:

- ``dot_files(false)``
- ``directories_first(true)``
- ``sort_by(name)``
- ``sort_order(ascending)``
- ``columns([name, type, size, modified])``
- ``type_display(simple)``
- ``size_display(bytes)``
- ``title('Directory listing')``
- ``theme(default)``
- ``stylesheets([])``
- ``exclude([])``
- ``cache_control([])``
- ``expires(none)``

Supported features:

- ``GET`` and ``HEAD``
- ``200 OK`` HTML directory listings for existing directory targets
- canonical docroot-prefix checks after path normalization
- shared document-root path sandboxing with ``http_static_files``
- directory-first sorting by default, with configurable fallback to a
  single mixed ordering
- sortable ``Name``, ``Type``, ``Size``, and ``Modified`` columns
- query-driven ordering using ``sort=name|type|size|modified`` and
  ``order=ascending|descending`` for visible columns only
- breadcrumb navigation for the current directory path
- parent-directory navigation for non-root listings
- relative links for directory and file entries
- file metadata columns for entry type, file size, and file modification
  time
- optional column selection while keeping the ``Name`` column visible
- MIME-based file-type display using ``type_display(media)``
- optional file-size display units using ``size_display(kilobytes)``,
  ``size_display(megabytes)``, ``size_display(gigabytes)``, or
  ``size_display(terabytes)``
- theme-aware CSS hooks on the generated ``<body>``, ``<table>``, and
  entry rows
- optional stylesheet ``<link>`` generation for custom presentation
  themes
- optional inclusion of dot files
- configurable exclusion of entry names using exact names, prefixes,
  suffixes, or simple wildcard patterns
- configurable ``Cache-Control`` and ``Expires`` response headers
- ``404 Not Found`` for missing, unsafe, or non-directory targets
- ``405 Method Not Allowed`` for other methods

Presentation customization uses these option values:

- ``columns(Columns)`` accepts an ordered subset of
  ``[name, type, size, modified]`` that must include ``name``; hidden
  columns are also removed from the accepted query ``sort=`` values,
  with hidden or invalid sort keys falling back to the effective default
  visible column
- ``type_display(simple)`` keeps the existing ``file``/``directory``
  labels while ``type_display(media)`` renders MIME-based file type text
  such as ``text/plain``
- ``size_display(bytes)`` renders file sizes as raw byte counts;
  ``size_display(kilobytes)``, ``size_display(megabytes)``,
  ``size_display(gigabytes)``, and ``size_display(terabytes)`` render
  sizes with one decimal digit using binary multiples and labels such as
  ``1.2 KB`` or ``4.8 MB``; size sorting continues to use the raw byte
  value
- ``theme(Theme)`` adds a ``theme-Theme`` CSS class to the generated
  ``<body>`` and ``<table>`` elements
- ``stylesheets(Stylesheets)`` appends stylesheet links in the generated
  ``<head>``
- ``exclude(Exclusions)`` removes matching entry names before metadata
  and sorting; entries can be excluded using ``name(Name)``,
  ``prefix(Prefix)``, ``suffix(Suffix)``, ``wildcard(Pattern)``, or an
  atom shortcut for a wildcard pattern such as ``'.DS_Store'`` or
  ``'*.tmp'``; in wildcard patterns, only ``*`` has special meaning

Cache-policy configuration uses these option values:

- ``cache_control(Directives)`` where ``Directives`` is a list
  containing any of ``public``, ``private``, ``no_cache``, ``no_store``,
  ``no_transform``, ``must_revalidate``, ``proxy_revalidate``,
  ``immutable``, ``max_age(Seconds)``, ``s_maxage(Seconds)``,
  ``stale_while_revalidate(Seconds)``, ``stale_if_error(Seconds)``, or
  ``extension(Directive)``
- ``expires(none)`` to omit the header, ``expires(Seconds)`` for a
  relative expiry from the current system time, or
  ``expires(date_time(Year,Month,Day,Hour,Minute,Second))`` for an
  absolute expiry time

The generated HTML now includes stable CSS hooks such as:

- ``body`` class ``http-directory-listing theme-Theme``
- ``table`` class ``directory-listing-table theme-Theme columns-...``
- entry row classes ``entry entry-directory`` and ``entry entry-file``
