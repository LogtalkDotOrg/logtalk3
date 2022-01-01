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


.. index:: pair: question_hook/6; Built-in method
.. _methods_question_hook_6:

``question_hook/6``
===================

Description
-----------

::

   question_hook(Question, Kind, Component, Tokens, Check, Answer)

User-defined hook method for intercepting asking a question, declared in
the :ref:`logtalk <apis:logtalk/0>` built-in object as a public, multifile,
and dynamic predicate. This hook method is automatically called by the
:ref:`methods_ask_question_5` method. When the call
succeeds, the ``ask_question/5`` method assumes that the question have
been successfully asked and replied.

Modes and number of proofs
--------------------------

::

   question_hook(+nonvar, +nonvar, +nonvar, +list(nonvar), +callable, -term) - zero_or_one

Meta-predicate template
-----------------------

::

   question_hook(*, *, *, *, 1, *)

Errors
------

(none)

Examples
--------

::

   :- multifile(logtalk::question_hook/6).
   :- dynamic(logtalk::question_hook/6).

   % use a pre-defined answer instead of asking the user
   logtalk::question_hook(upper_limit, question, my_app, _, _, 3.7).

.. seealso::

   :ref:`methods_ask_question_5`,
   :ref:`methods_question_prompt_stream_4`
   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`,
