
.. index:: assertz/1
.. _methods_assertz_1:

assertz/1
=========

Description
-----------

::

   assertz(Head)
   assertz((Head:-Body))

Asserts a clause as the last one for a dynamic predicate. When the
predicate indicator for ``Head`` is declared in a :ref:`directives_uses_2` or
:ref:`directives_use_module_2` directive, the clause is asserted in the referenced
object or module. Otherwise, the clause is asserted for an object's
dynamic predicate. If the predicate is not previously declared (using a
scope directive), then a dynamic predicate declaration is added to the
object (assuming that we are asserting locally or that the compiler flag
``dynamic_declarations`` was set to ``allow`` when the object was
created or compiled).

This method may be used to assert clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in *this*. This allows easy initialization of dynamically
created objects when writing constructors.

Template and modes
------------------

::

   assertz(+clause)

Errors
------

Head is a variable:
   ``instantiation_error``
Head is a neither a variable nor a callable term:
   ``type_error(callable, Head)``
Body cannot be converted to a goal:
   ``type_error(callable, Body)``
The predicate indicator of Head, Name/Arity, is that of a private predicate:
   ``permission_error(modify, private_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a protected predicate:
   ``permission_error(modify, protected_predicate, Name/Arity)``
The predicate indicator of Head, Name/Arity, is that of a static predicate:
   ``permission_error(modify, static_predicate, Name/Arity)``
Target object was created/compiled with support for dynamic declaration of predicates turned off:
   ``permission_error(create, predicate_declaration, Name/Arity)``

Examples
--------

To assert a clause as the last one for a local dynamic predicate or a dynamic predicate in :term:`this`:
   ``assertz(Clause)``
To assert a clause as the last one for any public or protected dynamic predicate in :term:`self`:
   ``::assertz(Clause)``
To assert a clause as the last one for any public dynamic predicate in an explicit object:
   ``Object::assertz(Clause)``

.. seealso::

   :ref:`methods_abolish_1`,
   :ref:`methods_asserta_1`,
   :ref:`methods_clause_2`,
   :ref:`methods_retract_1`,
   :ref:`methods_retractall_1`
   :ref:`directives_dynamic_0`,
   :ref:`directives_dynamic_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
