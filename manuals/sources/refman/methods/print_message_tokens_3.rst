
.. index:: print_message_tokens/3
.. _methods_print_message_tokens_3:

print_message_tokens/3
======================

Description
-----------

::

   print_message_tokens(Stream, Prefix, Tokens)

Built-in method for printing a list of message tokens, declared in the
``logtalk`` built-in object as a public predicate. This method is
automatically called by the
:ref:`methods_print_message_3` method (assuming that the
message was not intercepted by a
:ref:`methods_message_hook_4` definition) and calls the
user-defined hook predicate
:ref:`methods_print_message_token_4` for each
token. When a call to this hook predicate succeeds, the
``print_message_tokens/3`` predicate assumes that the token have been
printed. When the call fails, the ``print_message_tokens/3`` predicate
uses a default printing procedure for the token.

Template and modes
------------------

::

   print_message_tokens(@stream_or_alias, +atom, @list(nonvar))

Errors
------

``(none)``

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
