.. _library_url:

``url``
=======

This library implements validation, parsing, generating, and
normalization of URLs, which can be represented as atoms, character
lists, or code lists. It currently supports the following URL schemes:

Web protocols:

- ``http``
- ``https``
- ``ws``
- ``wss``
- ``gopher``

File transfer and version control:

- ``ftp``
- ``ftps``
- ``sftp``
- ``git``

File access:

- ``file``

Databases:

- ``jdbc``
- ``mongodb``
- ``mysql``
- ``postgresql``

Email and news:

- ``mailto``
- ``news``
- ``nntp``

Media streaming:

- ``mms``
- ``rtmp``
- ``rtsp``

Shell access:

- ``ssh``
- ``telnet``

Directory services:

- ``ldap``
- ``ldaps``

Other protocols:

- ``tel``
- ``urn``

The library predicates are defined in the ``url(_Representation_)``
parametric object where ``_Representation_`` can be one of:

- ``atom`` - strings are represented as atoms
- ``chars`` - strings are represented as lists of characters
- ``codes`` - strings are represented as lists of character codes

The parameter must be bound when sending messages to the object.

The library also provides an utility predicate,
``file_path_components/2``, for converting a file-system path into file
URL components. This predicate also handles Windows drive-letter and UNC
paths.

API documentation
-----------------

Open the
`../../apis/library_index.html#url <../../apis/library_index.html#url>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(url(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(url(tester)).

Usage
-----

The ``valid/1`` and ``parse/2`` predicates are intended for absolute
URLs:

::

   | ?- url(atom)::valid('https://example.com/path?x=1#frag').
   true.

   | ?- url(atom)::parse('https://example.com/path?x=1#frag', Components).
   Components = [scheme(https), authority('example.com'), path('/path'), query('x=1'), fragment(frag)].

For RFC 3986 relative references, use ``parse/3`` or
``reference_kind/2``:

::

   | ?- url(atom)::parse('../users/42?verbose=true#details', Components, Kind).
   Components = [path('../users/42'), query('verbose=true'), fragment(details)],
   Kind = relative_path.

   | ?- url(atom)::reference_kind('//example.com/v1', Kind).
   Kind = network_path.

The ``generate/2`` and ``normalize/2`` predicates work with both
absolute URLs and relative references:

::

   | ?- url(atom)::generate([authority('example.com'), path('/v1')], Reference).
   Reference = '//example.com/v1'.

   | ?- url(atom)::normalize('../users/./42', NormalizedReference).
   NormalizedReference = '../users/42'.

The ``resolve/3`` predicate resolves a reference against an absolute
base URL, while ``relativize/3`` computes a relative reference between
compatible URLs:

::

   | ?- url(atom)::resolve('http://example.com/a/b/c', '../d', Resolved).
   Resolved = 'http://example.com/a/d'.

   | ?- url(atom)::relativize('http://example.com/a/b/c', 'http://example.com/v1', Relative).
   Relative = '../../v1'.

The ``file_path_components/2`` predicate converts file-system paths into
file URL components and supports both POSIX and Windows paths:

::

   | ?- url(atom)::file_path_components('/home/user/document.txt', Components).
   Components = [authority(''), path('/home/user/document.txt')].

   | ?- url(atom)::file_path_components('C:\\Temp\\app\\file.txt', Components).
   Components = [authority(''), path('/C:/Temp/app/file.txt')].
