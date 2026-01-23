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
