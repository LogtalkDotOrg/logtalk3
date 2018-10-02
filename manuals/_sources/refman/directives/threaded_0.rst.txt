
.. index:: threaded/0
.. _directives_threaded_0:

threaded/0
==========

Description
-----------

::

   threaded

Declares that an object supports concurrent calls and asynchronous
messages. Any object containing calls to the built-in multi-threading
predicates (or importing a category that contains such calls) must
include this directive.

Template and modes
------------------

::

   threaded

Examples
--------

::

   :- threaded.
