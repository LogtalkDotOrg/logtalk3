..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: print_message_tokens/3
.. _methods_print_message_tokens_3:

print_message_tokens/3
======================

Description
-----------

::

   print_message_tokens(Stream, Prefix, Tokens)

Built-in method for printing a list of message tokens, declared in the
:ref:`logtalk <apis:logtalk/0>` built-in object as a public predicate.
This method is automatically called by the
:ref:`methods_print_message_3` method (assuming that the
message was not intercepted by a
:ref:`methods_message_hook_4` definition) and calls the
user-defined hook predicate
:ref:`methods_print_message_token_4` for each
token. When a call to this hook predicate succeeds, the
``print_message_tokens/3`` predicate assumes that the token have been
printed. When the call fails, the ``print_message_tokens/3`` predicate
uses a default printing procedure for the token.

Modes and number of proofs
--------------------------

::

   print_message_tokens(@stream_or_alias, +atom, @list(nonvar)) - zero_or_one

Errors
------

(none)

Examples
--------

::

   ...,
   logtalk::print_message_tokens(user_error, '% ', ['Redefining ~w ~q'-[object,foo], nl]),
   ...

.. seealso::

   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_token_4`,
   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`
