..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


.. rst-class:: align-right

**built-in method**

.. index:: pair: phrase//1; Built-in method
.. _methods_phrase_1:

``phrase//1``
=============

Description
-----------

::

   phrase(GrammarRuleBody)

This non-terminal takes a grammar rule body and parses it using the two
implicit grammar rule arguments. A common use is to wrap what otherwise
would be a :term:`naked meta-variable` in a grammar rule body when defining
a meta non-terminal.

Meta-non-terminal template
--------------------------

::

   phrase(0)

Modes and number of proofs
--------------------------

::

   phrase(+callable) - zero_or_more

Errors
------

| ``GrammarRuleBody`` is a variable:
|     ``instantiation_error``
| ``GrammarRuleBody`` is neither a variable nor a callable term:
|     ``type_error(callable, GrammarRuleBody)``

Examples
--------

   (none)

.. seealso::

   :ref:`methods_call_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
