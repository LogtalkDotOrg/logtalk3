..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: phrase/2; Built-in method
.. _methods_phrase_2:

``phrase/2``
============

Description
-----------

::

   phrase(GrammarRuleBody, Input)
   phrase(::GrammarRuleBody, Input)
   phrase(Object::GrammarRuleBody, Input)

True when the ``GrammarRuleBody`` grammar rule body can be applied to
the ``Input`` list of tokens. In the most common case,
``GrammarRuleBody`` is a non-terminal defined by a grammar rule. This
built-in method is declared private and thus cannot be used as a message
to an object. When using a :term:`backend Prolog compiler` supporting a
module system, calls in the format ``phrase(Module:GrammarRuleBody, Input)``
may also be used.

This method is opaque to cuts in the first argument. When the first
argument is sufficiently instantiated at compile time, the method call
is compiled in order to eliminate the implicit overheads of converting
the grammar rule body into a goal and meta-calling it. For performance
reasons, the second argument is only type-checked at compile time.

Modes and number of proofs
--------------------------

::

   phrase(+callable, ?list) - zero_or_more

Errors
------

| ``GrammarRuleBody`` is a variable:
|     ``instantiation_error``
| ``GrammarRuleBody`` is neither a variable nor a callable term:
|     ``type_error(callable, GrammarRuleBody)``

Examples
--------

| To parse a list of tokens using a local non-terminal:
|     ``phrase(NonTerminal, Input)``
| To parse a list of tokens using a non-terminal within the scope of :term:`self`:
|     ``phrase(::NonTerminal, Input)``
| To parse a list of tokens using a public non-terminal of an explicit object:
|     ``phrase(Object::NonTerminal, Input)``

.. seealso::

   :ref:`methods_call_1`,
   :ref:`methods_phrase_1`,
   :ref:`methods_phrase_3`
