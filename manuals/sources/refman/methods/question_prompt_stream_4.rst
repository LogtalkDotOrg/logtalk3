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


.. rst-class:: align-right

**built-in method**

.. index:: pair: question_prompt_stream/4; Built-in method
.. _methods_question_prompt_stream_4:

``question_prompt_stream/4``
============================

Description
-----------

::

   question_prompt_stream(Kind, Component, Prompt, Stream)

User-defined hook method for specifying the default prompt and input
stream for asking a question for a given kind and :term:`component`.
This method is declared in the :ref:`logtalk <apis:logtalk/0>` built-in
object as a public, multifile, and dynamic predicate.

Modes and number of proofs
--------------------------

::

   question_prompt_stream(?nonvar, ?nonvar, ?atom, ?stream_or_alias) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- multifile(logtalk::question_prompt_stream/4).
   :- dynamic(logtalk::question_prompt_stream/4).

   logtalk::question_prompt_stream(question, debugger, '    > ', user_input).

.. seealso::

   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`
