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

.. index:: pair: print_message_token/4; Built-in method
.. _methods_print_message_token_4:

``print_message_token/4``
=========================

Description
-----------

::

   print_message_token(Stream, Prefix, Token, Tokens)

User-defined hook method for printing a message token, declared in the
:ref:`logtalk <apis:logtalk/0>` built-in object as a public, multifile,
and dynamic predicate. It allows the user to intercept the printing of
a message token. This hook method is automatically called by the
:ref:`methods_print_message_tokens_3` built-in
method for each token.

Modes and number of proofs
--------------------------

::

   print_message_token(@stream_or_alias, @atom, @nonvar, @list(nonvar)) - zero_or_one

Errors
------

(none)

Examples
--------

::

   :- multifile(logtalk::print_message_token/4).
   :- dynamic(logtalk::print_message_token/4).

   % ignore all flush tokens
   logtalk::print_message_token(_Stream, _Prefix, flush, _Tokens).

.. seealso::

   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`
