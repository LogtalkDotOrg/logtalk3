..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: current_predicate/1; Built-in method
.. _methods_current_predicate_1:

current_predicate/1
===================

Description
-----------

::

   current_predicate(Predicate)

Enumerates, by backtracking, visible, user-defined, object predicates.
Built-in predicates and predicates not declared using a scope directive
are not enumerated.

When ``Predicate`` is ground at compile time, this predicate also
succeeds for any predicates listed in :ref:`directives_uses_2` and
:ref:`directives_use_module_2` directives.

When ``Predicate`` is bound at compile time to a ``:/2`` term, this
predicate enumerates module predicates (assuming that the
:term:`backend Prolog compiler` supports modules).

Modes and number of proofs
--------------------------

::

   current_predicate(?predicate_indicator) - zero_or_more

Errors
------

| ``Predicate`` is neither a variable nor a valid predicate indicator:
|     ``type_error(predicate_indicator, Predicate)``
| ``Predicate`` is a ``Name/Arity`` term but ``Functor`` is neither a variable nor an atom:
|     ``type_error(atom, Name)``
| ``Predicate`` is a ``Name/Arity`` term but ``Arity`` is neither a variable nor an integer:
|     ``type_error(integer, Arity)``
| ``Predicate`` is a ``Name/Arity`` term but ``Arity`` is a negative integer:
|     ``domain_error(not_less_than_zero, Arity)``

Examples
--------

| To enumerate, by backtracking, the locally visible user predicates or the user predicates visible in :term:`this`:
|     ``current_predicate(Predicate)``
| To enumerate, by backtracking, the public and protected user predicates visible in :term:`self`:
|     ``::current_predicate(Predicate)``
| To enumerate, by backtracking, the public user predicates visible for an explicit object:
|     ``Object::current_predicate(Predicate)``

.. seealso::

   :ref:`methods_current_op_3`,
   :ref:`methods_predicate_property_2`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
