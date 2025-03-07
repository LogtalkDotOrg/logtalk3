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

.. index:: pair: message_tokens//2; Built-in method
.. _methods_message_tokens_2:

``message_tokens//2``
=====================

Description
-----------

::

   message_tokens(Message, Component)

User-defined non-terminal hook used to rewrite a message term into a list
of tokens and declared in the :ref:`logtalk <apis:logtalk/0>` built-in
object as a public, multifile, and dynamic non-terminal. The list of tokens
can be printed by calling the :ref:`methods_print_message_tokens_3` method.
This non-terminal hook is automatically called by the
:ref:`methods_print_message_3` method.

Modes and number of proofs
--------------------------

::

   message_tokens(+nonvar, +nonvar) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- multifile(logtalk::message_tokens//2).
   :- dynamic(logtalk::message_tokens//2).

   logtalk::message_tokens(redefining_entity(Type, Entity), core) -->
       ['Redefining ~w ~q'-[Type, Entity], nl].

.. seealso::

   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`,
   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`
