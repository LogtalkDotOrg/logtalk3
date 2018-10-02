
.. index:: threaded_engine_create/3
.. _predicates_threaded_engine_create_3:

threaded_engine_create/3
========================

Description
-----------

::

   threaded_engine_create(AnswerTemplate, Goal, Engine)

Creates a new engine for proving the given goal and defines an answer
template for retrieving the goal solution bindings. A message queue for
passing arbitrary terms to the engine is also created. If the name for
the engine is not given, a unique name is generated and returned. Engine
names shall be regarded as opaque terms; users shall not rely on its
type.

Template and modes
------------------

::

   threaded_engine_create(@term, @callable, ?nonvar)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``
Engine is the name of an existing engine:
   ``permission_error(create, engine, Engine)``

Examples
--------

Create a new engine for finding members of a list:
   ``threaded_engine_create(X, member(X, [1,2,3]), worker_1)``

See also
--------

:ref:`predicates_threaded_engine_destroy_1`,
:ref:`predicates_threaded_engine_self_1`,
:ref:`predicates_threaded_engine_1`,
:ref:`predicates_threaded_engine_next_2`,
:ref:`predicates_threaded_engine_next_reified_2`
