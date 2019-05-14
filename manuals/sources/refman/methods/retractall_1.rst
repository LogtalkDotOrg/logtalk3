..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: retractall/1; Built-in method
.. _methods_retractall_1:

retractall/1
============

Description
-----------

::

   retractall(Head)

Retracts all clauses with a matching head for an object dynamic predicate.

When the predicate indicator for ``Head`` is declared in a
:ref:`directives_uses_2` or :ref:`directives_use_module_2` directive,
the clauses are retracted in the referenced object or module.

This method may be used to retract clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in :term:`this`.

Modes and number of proofs
--------------------------

::

   retractall(@callable) - one

Errors
------

| Head is a variable:
|     ``instantiation_error``
| Head is neither a variable nor a callable term:
|     ``type_error(callable, Head)``
| The predicate indicator of Head, Name/Arity, is that of a private predicate:
|     ``permission_error(modify, private_predicate, Name/Arity)``
| The predicate indicator of Head, Name/Arity, is that of a protected predicate:
|     ``permission_error(modify, protected_predicate, Name/Arity)``
| The predicate indicator of Head, Name/Arity, is that of a static predicate:
|     ``permission_error(modify, static_predicate, Name/Arity)``
| The predicate indicator of Head, Name/Arity, is not declared:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

| To retract all clauses with a matching head of a dynamic predicate in :term:`this`:
|     ``retractall(Head)``
| To retract all clauses with a matching head of a public or protected dynamic predicate in :term:`self`:
|     ``::retractall(Head)``
| To retract all clauses with a matching head of a public dynamic predicate in an explicit object:
|     ``Object::retractall(Head)``

.. seealso::

   :ref:`methods_abolish_1`,
   :ref:`methods_asserta_1`,
   :ref:`methods_assertz_1`,
   :ref:`methods_clause_2`,
   :ref:`methods_retract_1`,
   :ref:`directives_dynamic_0`,
   :ref:`directives_dynamic_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
