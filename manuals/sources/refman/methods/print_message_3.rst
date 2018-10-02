
.. index:: print_message/3
.. _methods_print_message_3:

print_message/3
===============

Description
-----------

::

   print_message(Kind, Component, Term)

Built-in method for printing a message represented by a term, which is
converted to the message text using the
:ref:`logtalk::message_tokens(Term, Component) <methods_message_tokens_2>`
hook non-terminal. This method is declared in the ``logtalk`` built-in
object as a public predicate. The line prefix and the output stream used
for each ``Kind-Component`` pair can be found using the
:ref:`logtalk::message_prefix_stream(Kind, Component, Prefix, Stream) <methods_message_prefix_stream_4>`
hook predicate.

This predicate starts by converting the message term to a list of tokens
and by calling the
:ref:`logtalk::message_hook(Message, Kind, Component, Tokens) <methods_message_hook_4>`
hook predicate. If this predicate succeeds, the ``print_message/3``
predicate assumes that the message have been successfully printed.

Template and modes
------------------

::

   print_message(+nonvar, +nonvar, +nonvar)

Errors
------

``(none)``

Examples
--------

::

   ..., logtalk::print_message(information, core, redefining_entity(object, foo)), ...

See also
--------

:ref:`methods_message_hook_4`,
:ref:`methods_message_prefix_stream_4`,
:ref:`methods_message_tokens_2`,
:ref:`methods_print_message_tokens_3`,
:ref:`methods_print_message_token_4`,
:ref:`methods_ask_question_5`,
:ref:`methods_question_hook_6`,
:ref:`methods_question_prompt_stream_4`
