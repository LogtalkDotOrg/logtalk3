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


.. index:: pair: clause/2; Built-in method
.. _methods_clause_2:

clause/2
========

Description
-----------

::

   clause(Head, Body)

Enumerates, by backtracking, the clauses of a dynamic predicate.

When the predicate indicator for ``Head`` is declared in a
:ref:`directives_uses_2` or :ref:`directives_use_module_2` directive,
the predicate enumerates the clauses in the referenced object or module.
When the backend Prolog compiler supports a module system, the head
argument can also be module qualified.

This method may be used to enumerate clauses for predicates that are not
declared dynamic for dynamic objects provided that the predicates are
declared in :term:`this`.

Modes and number of proofs
--------------------------

::

   clause(+callable, ?body) - zero_or_more

Errors
------

| ``Head`` is a variable:
|     ``instantiation_error``
| ``Head`` is a neither a variable nor a callable term:
|     ``type_error(callable, Head)``
| ``Body`` is a neither a variable nor a callable term:
|     ``type_error(callable, Body)``
| The predicate indicator of ``Head``, ``Name/Arity``, is that of a private predicate:
|     ``permission_error(access, private_predicate, Name/Arity)``
| The predicate indicator of ``Head``, ``Name/Arity``, is that of a protected predicate:
|     ``permission_error(access, protected_predicate, Name/Arity)``
| The predicate indicator of ``Head``, ``Name/Arity``, is that of a static predicate:
|     ``permission_error(access, static_predicate, Name/Arity)``
| ``Head`` is not a declared predicate:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

| To retrieve a matching clause of a local dynamic predicate or a dynamic predicate in :term:`this`:
|     ``clause(Head, Body)``
| To retrieve a matching clause of a public or protected dynamic predicate in :term:`self`:
|     ``::clause(Head, Body)``
| To retrieve a matching clause of a public dynamic predicate in an explicit object:
|     ``Object::clause(Head, Body)``

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
