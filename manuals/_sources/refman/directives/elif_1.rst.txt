
.. index:: elif/1
.. _directives_elif_1:

elif/1
======

Description
-----------

::

   elif(Goal)

Supports embedded conditionals when performing conditional compilation.
The code following the directive is compiled if ``Goal`` is true. The
goal is subjected to goal expansion when the directive occurs in a
source file.

Conditional compilation goals cannot depend on predicate definitions
contained in the same source file that contains the conditional
compilation directives (as those predicates only become available after
the file is fully compiled and loaded).

Template and modes
------------------

::

   elif(@callable)

Examples
--------

::

   :- elif(predicate_property(callable(_), built_in)).

See also
--------

:ref:`directives_else_0`,
:ref:`directives_endif_0`,
:ref:`directives_if_1`
