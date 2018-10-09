
.. index:: threaded_ignore/1
.. _predicates_threaded_ignore_1:

threaded_ignore/1
=================

Description
-----------

::

   threaded_ignore(Goal)

Proves ``Goal`` asynchronously using a new thread. Only the first goal
solution is found. The argument can be a message sending goal. This call
always succeeds, independently of the result (success, failure, or
exception), which is simply discarded instead of being sent back to the
message queue of the object containing the call
(:term:`this`).

Template and modes
------------------

::

   threaded_ignore(@callable)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

Prove ``Goal`` asynchronously in a new thread:
   ``threaded_ignore(Goal)``
Prove ``::Message`` asynchronously in a new thread:
   ``threaded_ignore(::Message)``
Prove ``Object::Message`` asynchronously in a new thread:
   ``threaded_ignore(Object::Message)``

.. seealso::

   :ref:`predicates_threaded_call_1_2`,
   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_peek_1_2`,
   :ref:`predicates_threaded_1`,
   :ref:`directives_synchronized_1`
