
.. index:: print_message_token/4
.. _methods_print_message_token_4:

print_message_token/4
=====================

Description
-----------

::

   print_message_token(Stream, Prefix, Token, Tokens)

User-defined hook method for printing a message token, declared in the
``logtalk`` built-in object as a public, multifile, and dynamic
predicate. It allows the user to intercept the printing of a message
token. This hook method is automatically called by the
:ref:`methods_print_message_tokens_3` built-in
method for each token.

Template and modes
------------------

::

   print_message_token(@stream_or_alias, @atom, @nonvar, @list(nonvar))

Errors
------

``(none)``

Examples
--------

::

   :- multifile(logtalk::print_message_token/4).
   :- dynamic(logtalk::print_message_token/4).

   % ignore all flush tokens
   logtalk::print_message_token(_Stream, _Prefix, flush, _Tokens).

See also
--------

:ref:`methods_message_hook_4`,
:ref:`methods_message_prefix_stream_4`,
:ref:`methods_message_tokens_2`,
:ref:`methods_print_message_3`,
:ref:`methods_print_message_tokens_3`,
:ref:`methods_ask_question_5`,
:ref:`methods_question_hook_6`,
:ref:`methods_question_prompt_stream_4`
