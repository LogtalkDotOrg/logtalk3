
.. index:: retractall/1
.. _methods_retractall_1:

retractall/1
============

Description
-----------

::

   retractall(Head)

Retracts all clauses with a matching head for a dynamic predicate. When
the predicate indicator for ``Head`` is declared in a :ref:`directives_uses_2` or
:ref:`directives_use_module_2` directive, the clauses are retracted in the referenced
object or module. Otherwise the clauses are retracted in an object's
dynamic predicate.

This method may be used to retract clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in *this*.

Template and modes
------------------

::

   retractall(+callable)

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

To retract all local predicate clauses or all predicate clauses in :term:`this` with a matching head:
   ``retractall(Head)``
To retract all public or protected predicate clauses with a matching head in :term:`self`:
   ``::retractall(Head)``
To retract all public predicate clauses with a matching head in an explicit object:
   ``Object::retractall(Head)``

See also
--------

:ref:`methods_abolish_1`,
:ref:`methods_asserta_1`,
:ref:`methods_assertz_1`,
:ref:`methods_clause_2`,
:ref:`methods_retract_1`,
:ref:`directives_dynamic_0`,
:ref:`directives_dynamic_1`
:ref:`directives_uses_2`,
:ref:`directives_use_module_2`
