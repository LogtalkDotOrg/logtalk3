..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: retract/1
.. _methods_retract_1:

retract/1
=========

Description
-----------

::

   retract(Head)
   retract((Head:-Body))

Retracts a clause for an object dynamic predicate. On backtracking, the
predicate retracts the next matching clause.

When the predicate indicator for ``Head`` is declared in a
:ref:`directives_uses_2` or :ref:`directives_use_module_2` directive,
the clause is retracted in the referenced object or module.

This method may be used to retract clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in :term:`this`.

Modes and number of proofs
--------------------------

::

   retract(+clause) - zero_or_more

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

To retract a matching clause of a dynamic predicate in :term:`this`:
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
   :ref:`directives_dynamic_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
