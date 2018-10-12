
.. index:: message_hook/4
.. _methods_message_hook_4:

message_hook/4
==============

Description
-----------

::

   message_hook(Message, Kind, Component, Tokens)

User-defined hook method for intercepting printing of a message,
declared in the ``logtalk`` built-in object as a public, multifile, and
dynamic predicate. This hook method is automatically called by the
:ref:`methods_print_message_3` method. When the call
succeeds, the ``print_message/3`` method assumes that the message have
been successfully printed.

Template and modes
------------------

::

   message_hook(@nonvar, @nonvar, @nonvar, @list(nonvar))

Errors
------

(none)

Examples
--------

::

   :- multifile(logtalk::message_hook/4).
   :- dynamic(logtalk::message_hook/4).

   % print silent messages instead of discarding them as default
   logtalk::message_hook(_, silent, core, Tokens) :-
       logtalk::message_prefix_stream(silent, core, Prefix, Stream),
       logtalk::print_message_tokens(Stream, Prefix, Tokens).

.. seealso::

   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`,
   :ref:`methods_ask_question_5`,
   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`
