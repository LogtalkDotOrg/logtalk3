.. _library_tsv:

``tsv``
=======

The ``tsv`` library provides predicates for reading and writing TSV
files and streams:

https://www.iana.org/assignments/media-types/text/tab-separated-values

The main object, ``tsv/2``, is a parametric object allowing passing two
options: header handling (``keep`` or ``skip``) and comment handling
(``false`` or ``true``). When comment handling is ``true``, lines
starting with the ``#`` character are skipped when reading files and
streams.

The ``tsv/1`` parametric object is kept for backward compatibility and
extends ``tsv/2`` by setting the comments option to ``false``. The
``tsv`` object extends the ``tsv/2`` parametric object using the default
options ``keep`` and ``false``.

Files and streams can be read into a list of rows (with each row being
represented by a list of fields) or asserted using a user-defined
dynamic predicate. Reading can be done by first loading the whole file
(using the ``read_file/2-3`` predicates) into memory or line by line
(using the ``read_file_by_line/2-3`` predicates). Reading line by line
is usually the best option for parsing large TSV files.

Data can be saved to a TSV file or stream by providing the object and
predicate for accessing the data plus the name of the destination file
or the stream handle or alias.

API documentation
-----------------

Open the
`../../apis/library_index.html#tsv <../../apis/library_index.html#tsv>`__
link in a web browser.

Loading
-------

To load all entities in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(tsv(loader)).

Testing
-------

To test this library predicates, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(tsv(tester)).

Usage
-----

A TSV file can be read as a list of rows:

::

   | ?- tsv::read_file('test_files/data.tsv', Rows).

   Rows = [['Name','Age','Address'], ['Paul',23,'1115 W Franklin'], ['Bessy the Cow',5,'Big Farm Way'], ['Zeke,45,'W Main St']]
   yes

Alternatively, The TSV data can be saved using a public and dynamic
object predicate (that must be previously declared). For example:

::

   | ?- assertz(p(_,_,_)), retractall(p(_,_,_)).
   yes

   | ?- tsv(skip)::read_file('test_files/data.tsv', user, p/3).
   yes

   | ?- tsv(skip, true)::read_file('test_files/data_with_comments.tsv', user, p/3).
   yes

   | ?-  p(A,B,C).

   A = 'Paul', B = 23, C = '1115 W Franklin' ? ;
   ...

Given a predicate representing a table, the predicate data can be
written to a file or stream. For example:

::

   | ?- tsv::write_file('output.tsv', user, p/3).
   yes
