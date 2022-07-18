.. _library_toychr:

``toychr``
==========

To load this port and for sample queries, please see the ``SCRIPT.txt``
file.

This folder contains a Logtalk port of ToyCHR, a reference
implementation of Constraint Handling Rules (CHR) available from:

::

   https://www.comp.nus.edu.sg/~gregory/toychr/

The port is work in progress and includes significant modifications to
the original code:

-  Instead of compiling ``.chr`` files, it uses the term-expansion
   mechanism, by defining ``toychrdb`` as a hook object, to support
   writing rules inside objects and categories. As a consequence, the
   original ``chr_compile/1`` is not available.

-  The port is portable and should run on all supported backends.

The port also includes examples ported from the SWI-Prolog CHR package
examples and documentation. These examples are ported using the same
license of the original code (BSD-2-Clause).
