
.. index:: clause/2
.. _methods_clause_2:

clause/2
========

Description
-----------

::

   clause(Head, Body)

Enumerates, by backtracking, the clauses of a dynamic predicate. When
the predicate indicator for ``Head`` is declared in a ``uses/2`` or
``use_module/2`` directive, the predicate enumerates the clauses in the
referenced object or module. Otherwise, it enumerates the clauses for an
object's dynamic predicate.

This method may be used to enumerate clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in *this*.

Template and modes
------------------

::

   clause(+callable, ?body)

Errors
------

Head is a variable:
   ``instantiation_error``
Head is a neither a variable nor a callable term:
   ``type_error(callable, Head)``
Body is a neither a variable nor a callable term:
   ``type_error(callable, Body)``
The predicate indicator of Head, Name/Arity, is that of a private predicate:
   ``permission_error(access, private_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a protected predicate:
   ``permission_error(access, protected_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a static predicate:
   ``permission_error(access, static_predicate, Name/Arity)``
Head is not a declared predicate:
   ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

To retrieve a matching clause of a local dynamic predicate or a dynamic predicate in :term:`this`:
   ``clause(Head, Body)``
To retrieve a matching clause of a public or protected dynamic predicate in :term:`self`:
   ``::clause(Head, Body)``
To retrieve a matching clause of a public dynamic predicate in an explicit object:
   ``Object::clause(Head, Body)``

.. seealso::

   :ref:`methods_abolish_1`,
   :ref:`methods_asserta_1`,
   :ref:`methods_assertz_1`,
   :ref:`methods_retract_1`,
   :ref:`methods_retractall_1`
   :ref:`directives_dynamic_0`,
   :ref:`directives_dynamic_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
