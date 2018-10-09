
.. index:: op/3
.. _directives_op_3:

op/3
====

Description
-----------

::

   op(Precedence, Associativity, Operator)

Declares operators. Operators declared inside entities have local scope.
Global operators can be declared inside a source file by writing the
respective directives before the entity opening directives.

Template and modes
------------------

::

   op(+integer, +associativity, +atom_or_atom_list)

Examples
--------

::

   :- op(200, fy, +).
   :- op(200, fy, ?).
   :- op(200, fy, @).
   :- op(200, fy, -).

.. seealso::

   :ref:`methods_current_op_3`
