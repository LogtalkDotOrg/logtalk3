
.. index:: threaded_call/1-2
.. _predicates_threaded_call_1_2:

threaded_call/1-2
=================

Description
-----------

::

   threaded_call(Goal)
   threaded_call(Goal, Tag)

Proves ``Goal`` asynchronously using a new thread. The argument can be a
message sending goal. Calls to this predicate always succeeds and return
immediately. The results (success, failure, or exception) are sent back
to the message queue of the object containing the call
(:term:`this`); they can be retrieved by calling
the :ref:`predicates_threaded_exit_1_2` predicate.

The variant ``threaded_call/2`` returns a threaded call identifier tag
that can be used with the :ref:`predicates_threaded_exit_1_2` predicate.
Tags shall be regarded as opaque terms; users shall not rely on its type.

Template and modes
------------------

::

   threaded_call(@callable)
   threaded_call(@callable, -nonvar)

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
   ``threaded_call(Goal)``
Prove ``::Message`` asynchronously in a new thread:
   ``threaded_call(::Message)``
Prove ``Object::Message`` asynchronously in a new thread:
   ``threaded_call(Object::Message)``

.. seealso::

   :ref:`predicates_threaded_exit_1_2`,
   :ref:`predicates_threaded_ignore_1`,
   :ref:`predicates_threaded_once_1_2`,
   :ref:`predicates_threaded_peek_1_2`
   :ref:`predicates_threaded_1`
