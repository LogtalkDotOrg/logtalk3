
.. index:: threaded_engine_next_reified/2
.. _predicates_threaded_engine_next_reified_2:

threaded_engine_next_reified/2
==============================

Description
-----------

::

   threaded_engine_next_reified(Engine, Answer)

Retrieves the next reified answer from an engine. This predicate
predicate always succeeds and blocks until an answer becomes available.
Answers are returned using the terms ``the(Answer)``, ``no``, and
``exception(Error)``.

Template and modes
------------------

::

   threaded_engine_next_reified(@nonvar, ?nonvar)

Errors
------

Engine is a variable:
   ``instantiation_error``
Engine is neither a variable nor the name of an existing engine:
   ``existence_error(engine, Engine)``

Examples
--------

Gets the next engine answer:
   ``threaded_engine_next_reified(worker_1, Answer)``

See also
--------

:ref:`predicates_threaded_engine_create_3`,
:ref:`predicates_threaded_engine_next_2`,
:ref:`predicates_threaded_engine_yield_1`
