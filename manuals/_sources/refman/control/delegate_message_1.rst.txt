..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


.. index:: pair: []/1; Control construct
.. _control_delegate_message_1:

``[]/1``
========

Description
-----------

::

   [Object::Message]
   [{Proxy}::Message]

This control construct allows the programmer to send a message to an
object while preserving the original sender. It is mainly used in the
definition of object handlers for unknown messages. This functionality
is usually known as *delegation* but be aware that this is an overloaded
word that can mean different things in different object-oriented
programming languages.

To prevent using of this control construct to break object
encapsulation, an attempt to delegate a message to the original sender
results in an error. The remaining error conditions are the same as the
:ref:`control_send_to_object_2` control construct.

Note that, despite the correct functor for this control construct being
(traditionally) ``'.'/2``, we refer to it as ``[]/1`` simply to
emphasize that the syntax is a list with a single element.

Modes and number of proofs
--------------------------

::

   [+object_identifier::+callable] - zero_or_more
   [{+object_identifier}::+callable] - zero_or_more

Errors
------

| ``Object`` is a variable:
|     ``instantiation_error``
| Object is neither a variable nor an object identifier:
|     ``type_error(object_identifier, Object)``
| ``Object`` does not exist:
|     ``existence_error(object, Object)``
| ``Object`` and the original *sender* are the same object:
|     ``permission_error(access, object, Sender)``
| 
| ``Proxy`` is a variable:
|     ``instantiation_error``
| ``Proxy`` is neither a variable nor an object identifier:
|     ``type_error(object_identifier, Proxy)``
| ``Proxy``, with predicate indicator ``Name/Arity``, does not exist in the ``user`` pseudo-object:
|     ``existence_error(procedure, Name/Arity)``
| 
| ``Message`` is a variable:
|     ``instantiation_error``
| ``Message`` is neither a variable nor a callable term:
|     ``type_error(callable, Message)``
| ``Message``, with predicate indicator ``Name/Arity``, is declared private:
|     ``permission_error(access, private_predicate, Name/Arity)``
| ``Message``, with predicate indicator ``Name/Arity``, is declared protected:
|     ``permission_error(access, protected_predicate, Name/Arity)``
| ``Message``, with predicate indicator ``Name/Arity``, is not declared:
|     ``existence_error(predicate_declaration, Name/Arity)``

Examples
--------

::

   % delegate unknown messages to the "backup" object:
   forward(Message) :-
       [backup::Message].

.. seealso::

   :ref:`control_send_to_object_2`,
   :ref:`control_send_to_self_1`,
   :ref:`control_call_super_1`,
   :ref:`methods_forward_1`
