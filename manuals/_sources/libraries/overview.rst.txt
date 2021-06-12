Overview
========

This folder contains libraries of useful objects, categories, and
protocols. Specific notes about individual libraries can be found in the
corresponding library directory ``NOTES.md`` files.

A plain Prolog version of the Unicode 6.2 standard is also included in
the ``unicode_data`` folder. See its ``README.md`` file for details.

A ``parallel_logtalk_processes_setup.pl`` Prolog file is also provided
with sample code for selected backend Prolog compilers for initializing
Logtalk processes such that each process uses a unique scratch directory
therefore allowing parallel process execution (e.g. for usage at
continuous integration servers). Starting with Logtalk 3.48.0, this
setup is only required in general when running with the ``clean`` flag
turned off. See the comments in the file itself for usage instructions.

Library documentation
---------------------

Specific notes about each library can be found in the corresponding
``NOTES.md`` files. HTML documentation for each library API can be found
on the ``docs`` directory (open the ``../docs/index.html`` file with
your web browser). The documentation for these libraries can be
regenerated using the shell scripts ``../scripts/update_html_docs.sh``
and ``../scripts/update_svg_diagrams.sh``.

Loading libraries
-----------------

All the individual libraries can be loaded using the
``<library name>(loader)`` notation as argument for the compiling and
loading predicates. For example:

::

   | ?- logtalk_load(random(loader)).

For existing applications still relying on the old library
``*_loader.lgt`` files, these loader files are still provided but are
considered deprecated.

There is a file named ``all_loader.lgt`` that will load all libraries.
Simply type the goal:

::

   | ?- logtalk_load(library(all_loader)).

As a general rule, always use the corresponding loader file to load a
library. Most library entities are part of small hierarchies or depend
on other libraries and thus cannot be loaded and compiled separately
(e.g. the ``list`` object implements the ``listp`` protocol and is part
of a basic types hierarchy). Using the loader files takes care of all
dependencies and also ensures compilation in optimized mode.

Testing libraries
-----------------

Most of the libraries include unit tests in their directory, together
with a ``tester.lgt`` file for running them. For example, to run the
tests for the ``random`` library, we can use the goal:

::

   | ?- logtalk_load(random(tester)).

To run all libraries tests, we can use the ``logtalk_tester`` automation
script from the ``library`` directory at the root of the Logtalk
distribution. For example, assuming the Logtalk user directory is
``~/logtalk`` and that we want to run the tests using ECLiPSe as the
backend Prolog compiler:

::

   $ cd ~/logtalk/library
   $ logtalk_tester -p eclipse

Credits
-------

Some code in this library is based on public domain Prolog code, in
particular, code adopted from the Edinburgh Prolog library. The
definition of predicate ``reverse/2`` in object list is from Richard
O'Keefe and can be found in its book "The Craft of Prolog".

Some elements of this library are inspired by Richard O'Keefe library
proposal available at:

http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm

Some libraries, or part of libraries, are either ports of Prolog system
libraries or inspired by Prolog system libraries. See the individual
library notes for details. See also the ``NOTICE.txt`` file at the root
of the Logtalk distribution for copyright information on third-party
source code.

Other notes
-----------

Some files contained in this directory represent work in progress and
are not loaded by default by any loader utility file.
