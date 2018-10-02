
.. index:: context/1
.. _methods_context_1:

context/1
=========

Description
-----------

::

   context(Context)

Returns the execution context for a predicate clause using the term
``logtalk(Head,ExecutionContext)`` where ``Head`` is the head of the
clause containing the call. This private predicate is mainly used for
providing a default error context when type-checking predicate
arguments. The ``ExecutionContext`` term should be regarded as an opaque
term, which can be decoded using the ``logtalk::execution_context/7``
predicate. Calls to this predicate are inlined at compilation time.

Template and modes
------------------

::

   context(--callable)

Errors
------

Context is not a variable:
   ``type_error(var, Context)``

Examples
--------

::

   foo(A, N) :-
       % type-check arguments
       context(Context),
       type::check(atom, A, Context),
       type::check(integer, N, Context),
       % arguments are fine; go ahead
       ... .

See also
--------

:ref:`methods_parameter_2`,
:ref:`methods_self_1`,
:ref:`methods_sender_1`,
:ref:`methods_this_1`
