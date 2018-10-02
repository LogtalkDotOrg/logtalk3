
.. index:: threaded_engine_next/2
.. _predicates_threaded_engine_next_2:

threaded_engine_next/2
======================

Description
-----------

::

   threaded_engine_next(Engine, Answer)

Retrieves the next answer from an engine. This predicate blocks until an
answer becomes available. The predicate fails when there are no more
solutions to the engine goal. If the engine goal throws an exception,
calling this predicate will re-throw the exception and subsequent calls
will fail.

Template and modes
------------------

::

   threaded_engine_next(@nonvar, ?term)

Errors
------

Engine is a variable:
   ``instantiation_error``
Engine is neither a variable nor the name of an existing engine:
   ``existence_error(engine, Engine)``

Examples
--------

Gets the next engine answer:
   ``threaded_engine_next(worker_1, Answer)``

See also
--------

:ref:`predicates_threaded_engine_create_3`,
:ref:`predicates_threaded_engine_next_reified_2`,
:ref:`predicates_threaded_engine_yield_1`
