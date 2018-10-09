
.. index:: message_tokens//2
.. _methods_message_tokens_2:

message_tokens//2
=================

Description
-----------

::

   message_tokens(Message, Component)

User-defined non-terminal hook used to rewrite a message term into a
list of tokens and declared in the ``logtalk`` built-in object as a
public, multifile, and dynamic non-terminal. The list of tokens can be
printed by calling the :ref:`methods_print_message_tokens_3` method.
This non-terminal hook is automatically called by the
:ref:`methods_print_message_3` method.

Template and modes
------------------

::

   message_tokens(+nonvar, +nonvar)

Errors
------

``(none)``

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
