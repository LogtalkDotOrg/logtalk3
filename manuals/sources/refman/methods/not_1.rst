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

.. index:: pair: (\+)/1; Built-in method
.. _methods_not_1:

``(\+)/1``
==========

Description
-----------

::

   \+ Goal

Not-provable meta-predicate. True iff ``call(Goal)`` is false. This
built-in meta-predicate is declared as a private method and thus cannot
be used as a message to an object.

.. warning::

   The argument is always compiled (for improved performance). As a
   consequence, when the argument is a control construct (e.g., a
   conjunction), any meta-variables will be wrapped with the equivalent
   to the ``call/1`` control construct. Note that these semantics differ
   from the ISO Prolog Core standard specification for the ``(\+)/1``
   built-in predicate. For example, assuming a conforming system:

   ::

      | ?- X = !, \+ (member(Y,[1,2,3]), X, write(Y), fail).
      1

      X = !

   But in Logtalk ``X`` is compiled into a meta-call, which is not
   cut-transparent:

   ::

      yes
      | ?- logtalk << (X = !, \+ (member(Y,[1,2,3]), X, write(Y), fail)).
      123

      X = !

   Note that the ISO Prolog Core standard doesn't specify a cut-transparent
   alternative to the ``call/1`` control construct.


Meta-predicate template
-----------------------

::

   \+ 0

Modes and number of proofs
--------------------------

::

   \+ +callable - zero_or_one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Not-provable goal in the context of the object or category containing the call:
|     ``\+ Goal``
| Not-provable goal sent as a message to :term:`self`:
|     ``\+ ::Goal``
| Not-provable goal sent as a message to an explicit object:
|     ``\+ Object::Goal``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_ignore_1`,
   :ref:`methods_once_1`
