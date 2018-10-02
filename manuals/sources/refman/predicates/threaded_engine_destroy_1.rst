
.. index:: threaded_engine_destroy/1
.. _predicates_threaded_engine_destroy_1:

threaded_engine_destroy/1
=========================

Description
-----------

::

   threaded_engine_destroy(Engine)

Stops an engine.

Template and modes
------------------

::

   threaded_engine_destroy(@nonvar)

Errors
------

Engine is a variable:
   ``instantiation_error``
Engine is neither a variable nor the name of an existing engine:
   ``existence_error(engine, Engine)``

Examples
--------

Stop an engine named ``worker_1``:
   ``threaded_engine_destroy(worker_1)``
Stop all engines:
   ``forall(threaded_engine(Engine), threaded_engine_destroy(Engine))``

See also
--------

:ref:`predicates_threaded_engine_create_3`,
:ref:`predicates_threaded_engine_self_1`,
:ref:`predicates_threaded_engine_1`
