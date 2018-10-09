
.. index:: if/1
.. _directives_if_1:

if/1
====

Description
-----------

::

   if(Goal)

Starts conditional compilation. The code following the directive is
compiled if ``Goal`` is true. The goal is subjected to goal expansion
when the directive occurs in a source file.

Conditional compilation goals cannot depend on predicate definitions
contained in the same source file that contains the conditional
compilation directives (as those predicates only become available after
the file is fully compiled and loaded).

Template and modes
------------------

::

   if(@callable)

Examples
--------

::

   :- if(current_prolog_flag(double_quotes, atom)).

.. seealso::

   :ref:`directives_elif_1`,
   :ref:`directives_else_0`,
   :ref:`directives_endif_0`
