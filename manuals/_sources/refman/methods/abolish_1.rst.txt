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


.. index:: abolish/1
.. _methods_abolish_1:

abolish/1
=========

Description
-----------

::

   abolish(Predicate)

Abolishes a runtime declared object dynamic predicate or an object 
local dynamic predicate. Only predicates that are dynamically declared
at runtime (using a call to the ``asserta/1`` or ``assertz/1`` built-in
methods) can be abolished. 

When the predicate indicator is declared in a :ref:`directives_uses_2`
or :ref:`directives_use_module_2` directive, the predicate is abolished
in the referenced object or module.


Modes and number of proofs
--------------------------

::

   abolish(@predicate_indicator) - one

Errors
------

| Predicate is a variable:
|     ``instantiation_error``
| Functor is a variable:
|     ``instantiation_error``
| Arity is a variable:
|     ``instantiation_error``
| Predicate is neither a variable nor a valid predicate indicator:
|     ``type_error(predicate_indicator, Predicate)``
| Functor is neither a variable nor an atom:
|     ``type_error(atom, Functor)``
| Arity is neither a variable nor an integer:
|     ``type_error(integer, Arity)``
| Predicate is statically declared:
|     ``permission_error(modify, predicate_declaration, Name/Arity)``
| Predicate is a private predicate:
|     ``permission_error(modify, private_predicate, Name/Arity)``
| Predicate is a protected predicate:
|     ``permission_error(modify, protected_predicate, Name/Arity)``
| Predicate is a static predicate:
|     ``permission_error(modify, static_predicate, Name/Arity)``
| Predicate is not declared for the object receiving the message:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

| To abolish a local dynamic predicate or a dynamic predicate in :term:`this`:
|     ``abolish(Predicate)``
| To abolish a public or protected dynamic predicate in :term:`self`:
|     ``::abolish(Predicate)``
| To abolish a public dynamic predicate in an explicit object:
|     ``Object::abolish(Predicate)``

.. seealso::

   :ref:`methods_asserta_1`,
   :ref:`methods_assertz_1`,
   :ref:`methods_clause_2`,
   :ref:`methods_retract_1`,
   :ref:`methods_retractall_1`
   :ref:`directives_dynamic_0`,
   :ref:`directives_dynamic_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
