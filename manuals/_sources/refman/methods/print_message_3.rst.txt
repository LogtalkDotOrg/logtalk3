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


.. index:: pair: print_message/3; Built-in method
.. _methods_print_message_3:

``print_message/3``
===================

Description
-----------

::

   print_message(Kind, Component, Term)

Built-in method for printing a message represented by a term, which is
converted to the message text using the
:ref:`logtalk::message_tokens(Term, Component) <methods_message_tokens_2>`
hook non-terminal. This method is declared in the
:ref:`logtalk <apis:logtalk/0>` built-in
object as a public predicate. The line prefix and the output stream used
for each ``Kind-Component`` pair can be found using the
:ref:`logtalk::message_prefix_stream(Kind, Component, Prefix, Stream) <methods_message_prefix_stream_4>`
hook predicate.

This predicate starts by converting the message term to a list of tokens
and by calling the
:ref:`logtalk::message_hook(Message, Kind, Component, Tokens) <methods_message_hook_4>`
hook predicate. If this predicate succeeds, the ``print_message/3``
predicate assumes that the message have been successfully printed.

Modes and number of proofs
--------------------------

::

   print_message(+nonvar, +nonvar, +nonvar) - one

Errors
------

(none)

Examples
--------

::

   ..., logtalk::print_message(information, core, redefining_entity(object, foo)), ...

.. seealso::

   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`,
   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`
