
.. index:: call//1-N
.. _methods_call_1:

call//1-N
=========

Description
-----------

::

   call(Closure)
   call(Closure, Arg1, ...)
   call(Object::Closure, Arg1, ...)
   call(::Closure, Arg1, ...)

This non-terminal takes a closure and is processed by appending the
input list of tokens and the list of remaining tokens to the arguments
of the closure. This built-in non-terminal is interpreted as a private
non-terminal and thus cannot be used as a message to an object. When
using a back-end Prolog compiler supporting a module system, calls in
the format ``call(Module:Closure)`` may also be used. By using as
argument a lambda expression, this built-in non-terminal provides
controlled access to the input list of tokens and to the list of the
remaining tokens processed by the grammar rule containing the call.

Template and modes
------------------

::

   call(+callable)
   call(+callable, ?term)
   call(+callable, ?term, ?term)
   ...

Errors
------

Closure is a variable:
   ``instantiation_error``
Closure is neither a variable nor a callable term:
   ``type_error(callable, Closure)``

Examples
--------

Calls a goal, constructed by appending the input list of tokens and the list of remaining tokens to the arguments of the closure, in the context of the object or category containing the call:
   ``call(Closure)``
To send a goal, constructed by appending the input list of tokens and the list of remaining tokens to the arguments of the closure, as a message to :term:`self`:
   ``call(::Closure)``
To send a goal, constructed by appending the input list of tokens and the list of remaining tokens to the arguments of the closure, as a message to an explicit object:
   ``call(Object::Closure)``

.. seealso::

   :ref:`methods_eos_0`,
   :ref:`methods_phrase_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
