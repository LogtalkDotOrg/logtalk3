.. _library_csv:

``csv``
=======

The ``csv`` library provides predicates for reading and writing CSV
files and streams:

https://www.rfc-editor.org/rfc/rfc4180.txt

The main object, ``csv/4``, is a parametric object allowing passing
options for the handling of the header of the file, the fields
separator, the handling of double-quoted fields, and comments handling.
When comments handling is ``true``, lines starting with the ``#``
character are skipped when reading files and streams.

The ``csv/3`` parametric object is kept for backward compatibility and
extends ``csv/4`` by setting the comments option to ``false``. The
``csv`` object extends the ``csv/4`` parametric object using default
option values.

The library also includes predicates to guess the separator and guess
the number of columns in a given CSV file.

Files and streams can be read into a list of rows (with each row being
represented by a list of fields) or asserted using a user-defined
dynamic predicate. Reading can be done by first loading the whole file
(using the ``read_file/2-3`` predicates) into memory or line by line
(using the ``read_file_by_line/2-3`` predicates). Reading line by line
is usually the best option for parsing large CSV files.

Data can be saved to a CSV file or stream by providing the object and
predicate for accessing the data plus the name of the destination file
or the stream handle or alias.

API documentation
-----------------

Open the
`../../apis/library_index.html#csv <../../apis/library_index.html#csv>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(csv(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(csv(tester)).

Usage
-----

The ``csv(Header, Separator, IgnoreQuotes, Comments)`` parametric object
allows passing the following options:

1. ``Header``: possible values are ``missing``, ``skip``, and ``keep``.
2. ``Separator``: possible values are ``comma``, ``tab``, ``semicolon``,
   and ``colon``.
3. ``IgnoreQuotes``: possible values are ``true`` to ignore double
   quotes surrounding field data and ``false`` to preserve the double
   quotes.
4. ``Comments``: possible values are ``false`` (default) and ``true`` to
   skip lines starting with ``#``.

The ``csv`` object uses the default values ``keep``, ``comma``,
``false``, and ``false``.

When writing CSV files or streams, set the quoted fields option to
``false`` to write all non-numeric fields double-quoted (i.e., escaped).

The library objects can also be used to guess the separator used in a
CSV file if necessary. For example:

::

   | ?- csv::guess_separator('test_files/crlf_ending.csv', Separator).
   Is this the proper reading of a line of this file (y/n)? [aaa,bb,ccc]
   |> y.

   Separator = comma ?

This information can then be used to read the CSV file returning a list
of rows:

::

   | ?- csv(keep, comma, true)::read_file('test_files/crlf_ending.csv', Rows).

   Rows = [[aaa,bbb,ccc],[zzz,yyy,xxx]] ?

   | ?- csv(keep, comma, true, true)::read_file('test_files/comments.csv', Rows).

   Rows = [[aaa,bbb,ccc],[zzz,yyy,xxx]] ?

Alternatively, the CSV data can be saved using a public and dynamic
object predicate (that must be previously declared). For example:

::

   | ?- assertz(p(_,_,_)), retractall(p(_,_,_)).
   yes

   | ?- csv(keep, comma, true)::read_file('test_files/crlf_ending.csv', user, p/3).
   yes

   | ?-  p(A,B,C).

   A = aaa
   B = bbb
   C = ccc ? ;

   A = zzz
   B = yyy
   C = xxx

Given a predicate representing a table, the predicate data can be
written to a file or stream. For example:

::

   | ?- csv(keep, comma, true)::write_file('output.csv', user, p/3).
   yes

leaving the content just as the original file thanks to the use of
``true`` for the ``IgnoreQuotes`` option:

::

   aaa,bbb,ccc
   zzz,yyy,xxx

Otherwise:

::

   | ?- csv(keep, comma, false)::write_file('output.csv', user, p/3).
   yes

results in the following file content:

::

   "aaa","bbb","ccc"
   "zzz","yyy","xxx"

The ``guess_arity/2`` method, to identify the arity, i. e. the number of
fields or columns per record in a given CSV file, for example:

::

   | ?- csv(keep, comma, false)::guess_arity('test_files/crlf_ending.csv', Arity).
   Is this the proper reading of a line of this file (y/n)? [aaa,bbb,ccc]
   |> y.

   Arity = 3
