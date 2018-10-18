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


.. index:: predicate_property/2
.. _methods_predicate_property_2:

predicate_property/2
====================

Description
-----------

::

   predicate_property(Predicate, Property)

Enumerates, by backtracking, the properties of a visible object predicate.
Properties for predicates not declared using a scope directive are not
enumerated. The valid predicate properties are listed in the language
gramar section on :ref:`predicate properties <grammar_predicate_properties>`.

When ``Predicate`` is ground at compile time and its predicate indicator
is listed in a :ref:`directives_uses_2` or :ref:`directives_use_module_2`
directive, properties are enumerated for the referenced object or module
predicate.

When ``Predicate`` is bound at compile time to a ``:/2`` term, this
predicate enumerates properties for module predicates (assuming that
the backend Prolog compiler supports modules).

Modes and number of proofs
--------------------------

::

   predicate_property(+callable, ?predicate_property) - zero_or_more

Errors
------

| Predicate is a variable:
|     ``instantiation_error``
| Predicate is neither a variable nor a callable term:
|     ``type_error(callable, Predicate)``
| Property is neither a variable nor a valid predicate property:
|     ``domain_error(predicate_property, Property)``

Examples
--------

| To enumerate, by backtracking, the properties of a locally visible user predicate or a user predicate visible in :term:`this`:
|     ``predicate_property(foo(_), Property)``
| To enumerate, by backtracking, the properties of a public or protected predicate visible in :term:`self`:
|     ``::predicate_property(foo(_), Property)``
| To enumerate, by backtracking, the properties of a public predicate visible in an explicit object:
|     ``Object::predicate_property(foo(_), Property)``

.. seealso::

   :ref:`methods_current_op_3`,
   :ref:`methods_current_predicate_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
