
.. index:: retract/1
.. _methods_retract_1:

retract/1
=========

Description
-----------

::

   retract(Head)
   retract((Head:-Body))

Retracts a clause for a dynamic predicate. When the predicate indicator
for ``Head`` is declared in a :ref:`directives_uses_2` or
:ref:`directives_use_module_2` directive,
the clause is retracted in the referenced object or module. Otherwise
the clause is retracted in an object's dynamic predicate. On
backtracking, the predicate retracts the next matching clause.

This method may be used to retract clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in *this*.

Template and modes
------------------

::

   retract(+clause)

Errors
------

Head is a variable:
   ``instantiation_error``
Head is neither a variable nor a callable term:
   ``type_error(callable, Head)``
The predicate indicator of Head, Name/Arity, is that of a private predicate:
   ``permission_error(modify, private_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a protected predicate:
   ``permission_error(modify, protected_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a static predicate:
   ``permission_error(modify, static_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is not declared:
   ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

To retract a matching clause of a local dynamic predicate or a dynamic predicate in :term:`this`:
   ``retract(Clause)``
To retract a matching clause of a public or protected dynamic predicate in :term:`self`:
   ``::retract(Clause)``
To retract a matching clause of a public dynamic predicate in an explicit object:
   ``Object::retract(Clause)``

.. seealso::

   :ref:`methods_abolish_1`,
   :ref:`methods_asserta_1`,
   :ref:`methods_assertz_1`,
   :ref:`methods_clause_2`,
   :ref:`methods_retractall_1`,
   :ref:`directives_dynamic_0`,
   :ref:`directives_dynamic_1`
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
