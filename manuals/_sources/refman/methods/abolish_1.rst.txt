
.. index:: abolish/1
.. _methods_abolish_1:

abolish/1
=========

Description
-----------

::

   abolish(Predicate)
   abolish(Name/Arity)

Abolishes a runtime declared dynamic predicate or a local dynamic
predicate. When the predicate indicator for ``Head`` is declared in a
:ref:`directives_uses_2` or :ref:`directives_use_module_2` directive,
the predicate is abolished in
the referenced object or module. Otherwise the predicate is abolished in
an object's database. In the case of objects, only predicates that are
dynamically declared (using a call to the ``asserta/1`` or ``assertz/1``
built-in methods) can be abolished.

Template and modes
------------------

::

   abolish(+predicate_indicator)

Errors
------

Predicate is a variable:
   ``instantiation_error``
Functor is a variable:
   ``instantiation_error``
Arity is a variable:
   ``instantiation_error``
Predicate is neither a variable nor a valid predicate indicator:
   ``type_error(predicate_indicator, Predicate)``
Functor is neither a variable nor an atom:
   ``type_error(atom, Functor)``
Arity is neither a variable nor an integer:
   ``type_error(integer, Arity)``
Predicate is statically declared:
   ``permission_error(modify, predicate_declaration, Name/Arity)``
Predicate is a private predicate:
   ``permission_error(modify, private_predicate, Name/Arity)``
Predicate is a protected predicate:
   ``permission_error(modify, protected_predicate, Name/Arity)``
Predicate is a static predicate:
   ``permission_error(modify, static_predicate, Name/Arity)``
Predicate is not declared for the object receiving the message:
   ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

To abolish a local dynamic predicate or a dynamic predicate in :term:`this`:
   ``abolish(Predicate)``
To abolish a public or protected dynamic predicate in :term:`self`:
   ``::abolish(Predicate)``
To abolish a public dynamic predicate in an explicit object:
   ``Object::abolish(Predicate)``

See also
--------

:ref:`methods_asserta_1`,
:ref:`methods_assertz_1`,
:ref:`methods_clause_2`,
:ref:`methods_retract_1`,
:ref:`methods_retractall_1`
:ref:`directives_dynamic_0`,
:ref:`directives_dynamic_1`
:ref:`directives_uses_2`,
:ref:`directives_use_module_2`
