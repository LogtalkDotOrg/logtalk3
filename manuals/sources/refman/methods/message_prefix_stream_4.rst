
.. index:: message_prefix_stream/4
.. _methods_message_prefix_stream_4:

message_prefix_stream/4
=======================

Description
-----------

::

   message_prefix_stream(Kind, Component, Prefix, Stream)

User-defined hook method for specifying the default prefix and stream
for printing a message for a given kind and component. This method is
declared in the ``logtalk`` built-in object as a public, multifile, and
dynamic predicate.

Template and modes
------------------

::

   message_prefix_stream(?nonvar, ?nonvar, ?atom, ?stream_or_alias)

Errors
------

``(none)``

Examples
--------

::

   :- multifile(logtalk::message_prefix_stream/4).
   :- dynamic(logtalk::message_prefix_stream/4).

   logtalk::message_prefix_stream(information, core, '% ', user_output).

See also
--------

:ref:`methods_message_hook_4`,
:ref:`methods_message_tokens_2`,
:ref:`methods_print_message_3`,
:ref:`methods_print_message_tokens_3`,
:ref:`methods_print_message_token_4`,
:ref:`methods_ask_question_5`,
:ref:`methods_question_hook_6`,
:ref:`methods_question_prompt_stream_4`
