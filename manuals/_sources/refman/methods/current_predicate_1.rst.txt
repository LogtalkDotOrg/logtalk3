
.. index:: current_predicate/1
.. _methods_current_predicate_1:

current_predicate/1
===================

Description
-----------

::

   current_predicate(Predicate)

Enumerates, by backtracking, visible user predicates. When the predicate
is declared in a ``uses/2`` or ``use_module/2`` directive, predicates
are enumerated for the referenced object or module. Otherwise, predicates
are enumerated for an object. In the case of objects, predicates not
declared using a scope directive are not enumerated.

Template and modes
------------------

::

   current_predicate(?predicate_indicator)

Errors
------

Predicate is neither a variable nor a valid predicate indicator:
   ``type_error(predicate_indicator, Predicate)``
Predicate is a Name/Arity term but Functor is neither a variable nor an atom:
   ``type_error(atom, Name)``
Predicate is a Name/Arity term but Arity is neither a variable nor an integer:
   ``type_error(integer, Arity)``
Predicate is a Name/Arity term but Arity is a negative integer:
   ``domain_error(not_less_than_zero, Arity)``

Examples
--------

To enumerate, by backtracking, the locally visible user predicates or the user predicates visible in :term:`this`:
   ``current_predicate(Predicate)``
To enumerate, by backtracking, the public and protected user predicates visible in :term:`self`:
   ``::current_predicate(Predicate)``
To enumerate, by backtracking, the public user predicates visible for an explicit object:
   ``Object::current_predicate(Predicate)``

.. seealso::

   :ref:`methods_current_op_3`,
   :ref:`methods_predicate_property_2`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
