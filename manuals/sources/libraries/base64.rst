.. _library_base64:

``base64``
==========

The ``base64`` library provides predicates for parsing and generating
data in the Base64 and Base64URL formats as per the specification found
at:

https://tools.ietf.org/html/rfc4648

API documentation
-----------------

Open the
`../../docs/library_index.html#base64 <../../docs/library_index.html#base64>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` utility
file:

::

   | ?- logtalk_load(base64(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(base64(tester)).

Encoding
--------

Encoding a list of bytes in Base64 format is accomplished by the
``base64::generate/2`` predicate. For example:

::

   | ?- atom_codes('Hello world!', Bytes),
        base64::generate(atom(Base64), Bytes).
   Base64 = 'SGVsbG8gd29ybGQh'
   Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
   yes

   | ?- atom_codes('Hello world!', Bytes),
        base64::generate(codes(Base64), Bytes).
   Base64 = [83,71,86,115,98,71,56,103,100,50,57,121,98,71,81,104]
   Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
   yes

The Base64 result can also be represented using a list of chars, written
to a file or to a stream. See the API documentation for details.

For safe encoding of URLs, use instead the Base64URL format. For
example:

::

   | ?- base64url::generate(atom(Base64URL), 'https://logtalk.org').
   Base64URL == 'aHR0cHM6Ly9sb2d0YWxrLm9yZw'
   yes

The Base64URL can also be represented using a list of chars or a list of
codes. The input URL should be in the same format.

Decoding
--------

Decoding of Base64 data is accomplished using the ``base64::parse/2``
predicate. For example:

::

   | ?- base64::parse(atom('SGVsbG8gd29ybGQh'), Bytes),
        atom_codes(Atom, Bytes).
   Atom = 'Hello world!'
   Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
   yes

   | ?- base64::parse(chars(['S','G','V',s,b,'G','8',g,d,'2','9',y,b,'G','Q',h]), Bytes),
        atom_codes(Atom, Bytes).
   Atom = 'Hello world!'
   Bytes = [72,101,108,108,111,32,119,111,114,108,100,33]
   yes

The ``base64::parse/2`` predicate accepts other input source such as a
file or a stream. See the API documentation for details.

For decoding of URLs in the Base64URL format, use the
``base64url::parse/2`` predicate. For example:

::

   | ?- base64url::parse(atom('aHR0cHM6Ly9sb2d0YWxrLm9yZw'), URL).
   URL == 'https://logtalk.org'
   yes

The ``base64url::parse/2`` predicate also accepts a list of chars or a
list of codes as input. See the API documentation for details.
