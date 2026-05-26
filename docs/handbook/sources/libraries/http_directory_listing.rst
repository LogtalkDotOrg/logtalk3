.. _library_http_directory_listing:

``http_directory_listing``
==========================

The ``http_directory_listing`` library provides a router-agnostic helper
for serving HTML directory listings from a configured document root
using normalized ``http`` request and response terms.

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

The current slice provides one public object with two public predicates:

- ``serve/4``
- ``serve/5``

Supported options are:

- ``dot_files(Boolean)``
- ``directories_first(Boolean)``
- ``sort_by(SortBy)``
- ``sort_order(SortOrder)``
- ``columns(Columns)``
- ``type_display(TypeDisplay)``
- ``title(Title)``
- ``theme(Theme)``
- ``stylesheets(Stylesheets)``

Default options are:

- ``dot_files(false)``
- ``directories_first(true)``
- ``sort_by(name)``
- ``sort_order(ascending)``
- ``columns([name, type, size, modified])``
- ``type_display(simple)``
- ``title('Directory listing')``
- ``theme(default)``
- ``stylesheets([])``

Supported features:

- ``GET`` and ``HEAD``
- ``200 OK`` HTML directory listings for existing directory targets
- canonical docroot-prefix checks after path normalization
- shared document-root path sandboxing with ``http_static_files``
- directory-first sorting by default, with configurable fallback to a
  single mixed ordering
- sortable ``Name``, ``Type``, ``Size``, and ``Modified`` columns
- query-driven ordering using ``sort=name|type|size|modified`` and
  ``order=ascending|descending``
- breadcrumb navigation for the current directory path
- parent-directory navigation for non-root listings
- relative links for directory and file entries
- file metadata columns for entry type, file size, and file modification
  time
- optional column selection while keeping the ``Name`` column visible
- MIME-based file-type display using ``type_display(media)``
- theme-aware CSS hooks on the generated ``<body>``, ``<table>``, and
  entry rows
- optional stylesheet ``<link>`` generation for custom presentation
  themes
- optional inclusion of dot files
- ``404 Not Found`` for missing, unsafe, or non-directory targets
- ``405 Method Not Allowed`` for other methods

Presentation customization uses these option values:

- ``columns(Columns)`` accepts an ordered subset of
  ``[name, type, size, modified]`` that must include ``name``
- ``type_display(simple)`` keeps the existing ``file``/``directory``
  labels while ``type_display(media)`` renders MIME-based file type text
  such as ``text/plain``
- ``theme(Theme)`` adds a ``theme-Theme`` CSS class to the generated
  ``<body>`` and ``<table>`` elements
- ``stylesheets(Stylesheets)`` appends stylesheet links in the generated
  ``<head>``

The generated HTML now includes stable CSS hooks such as:

- ``body`` class ``http-directory-listing theme-Theme``
- ``table`` class ``directory-listing-table theme-Theme columns-...``
- entry row classes ``entry entry-directory`` and ``entry entry-file``
