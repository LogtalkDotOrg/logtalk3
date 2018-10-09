
.. index:: ask_question/5
.. _methods_ask_question_5:

ask_question/5
==============

Description
-----------

::

   ask_question(Question, Kind, Component, Check, Answer)

Built-in method for asking a question represented by a term,
``Question``, which is converted to the question text using the
:ref:`logtalk::message_tokens(Question, Component) <methods_message_tokens_2>`
hook predicate. This method is declared in the ``logtalk`` built-in
object as a public predicate. The default question prompt and the input
stream used for each ``Kind-Component`` pair can be found using the
:ref:`logtalk::question_prompt_stream(Kind, Component, Prompt, Stream) <methods_question_prompt_stream_4>`
hook predicate. The ``Check`` argument is a closure that is converted
into a checking goal by extending it with the user supplied answer. This
predicate implements a read-loop that terminates when the checking
predicate succeeds.

This predicate starts by calling the
:ref:`logtalk::question_hook(Question, Kind, Component, Check, Answer) <methods_question_hook_6>`
hook predicate. If this predicate succeeds, the ``ask_question/5``
predicate assumes that the question have been successfully asked and
replied.

Template and modes
------------------

::

   ask_question(+nonvar, +nonvar, +nonvar, +callable, -term)

Meta-predicate template
-----------------------

::

   ask_question(*, *, *, 1, *)

Errors
------

``(none)``

Examples
--------

::

   ...,
   logtalk::ask_question(enter_age, question, my_app, integer, Age),
   ...

.. seealso::

   :ref:`methods_question_hook_6`,
   :ref:`methods_question_prompt_stream_4`,
   :ref:`methods_message_hook_4`,
   :ref:`methods_message_prefix_stream_4`,
   :ref:`methods_message_tokens_2`,
   :ref:`methods_print_message_3`,
   :ref:`methods_print_message_tokens_3`,
   :ref:`methods_print_message_token_4`
