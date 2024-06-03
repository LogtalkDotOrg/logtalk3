..
   This file is part of Logtalk <https://logtalk.org/>  
   SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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

**control construct**

.. index:: pair: (@)/1; Control construct
.. _call_in_this_1:

``(@)/1``
=========

Description
-----------

::

   @Predicate

Calls a predicate definition in :term:`this`. The argument must be a callable
term at compile time. The predicate must be declared (by a scope directive).
This control construct provides access to predicate definitions in *this* from
categories. For example, it allows overriding a predicate definition from a
complementing category with a new definition that calls goals before and
after calling the overriden definition (the overriding definition is sometimes
described in other programming languages as an *around method*). When used
within an object, it's the same as calling its argument.

Modes and number of proofs
--------------------------

::

   @ +callable - zero_or_more

Errors
------

| ``Predicate``, with predicate indicator ``Name/Arity``, is not declared:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

Assuming an object declaring a ``make_sound/0`` predicate, define an
*around method* in a complementing category:

::

   make_sound :-
       write('Started making sound...'), nl,
       @make_sound,
       write('... finished making sound.'), nl.

.. seealso::

   :ref:`control_send_to_object_2`,
   :ref:`control_send_to_self_1`,
   :ref:`control_delegate_message_1`
