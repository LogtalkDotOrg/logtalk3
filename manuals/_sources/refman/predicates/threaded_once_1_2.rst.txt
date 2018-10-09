
.. index:: threaded_once/1-2
.. _predicates_threaded_once_1_2:

threaded_once/1-2
=================

Description
-----------

::

   threaded_once(Goal)
   threaded_once(Goal, Tag)

Proves ``Goal`` asynchronously using a new thread. Only the first goal
solution is found. The argument can be a message sending goal. This call
always succeeds. The result (success, failure, or exception) is sent
back to the message queue of the object containing the call
(:term:`this`).

The variant ``threaded_once/2`` returns a threaded call identifier tag
that can be used with the :ref:`predicates_threaded_exit_1_2` predicate.
Tags shall be regarded as opaque terms; users shall not rely on its type.

Template and modes
------------------

::

   threaded_once(@callable)
   threaded_once(@callable, -nonvar)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``
Tag is not a variable:
   ``type_error(variable, Goal)``

Examples
--------

Prove ``Goal`` asynchronously in a new thread:
   ``threaded_once(Goal)``
Prove ``::Message`` asynchronously in a new thread:
   ``threaded_once(::Message)``
Prove ``Object::Message`` asynchronously in a new thread:
   ``threaded_once(Object::Message)``

.. seealso::

   :ref:`predicates_threaded_call_1_2`,
   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`predicates_threaded_peek_1_2`,
   :ref:`predicates_threaded_1`,
   :ref:`directives_synchronized_1`
