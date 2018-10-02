
.. index:: threaded_engine_post/2
.. _predicates_threaded_engine_post_2:

threaded_engine_post/2
======================

Description
-----------

::

   threaded_engine_post(Engine, Term)

Posts a term to the engine term queue.

Template and modes
------------------

::

   threaded_engine_post(@nonvar, @term)

Errors
------

Engine is a variable:
   ``instantiation_error``
Engine is neither a variable nor the name of an existing engine:
   ``existence_error(engine, Engine)``

Examples
--------

Post the atom ``ready`` to the ``worker_1`` engine queue:
   ``threaded_engine_post(worker_1, ready)``

See also
--------

:ref:`predicates_threaded_engine_fetch_1`
