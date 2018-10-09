
.. index:: threaded_engine_self/1
.. _predicates_threaded_engine_self_1:

threaded_engine_self/1
======================

Description
-----------

::

   threaded_engine_self(Engine)

Queries the name of engine calling the predicate.

Template and modes
------------------

::

   threaded_engine_self(?nonvar)

Errors
------

``(none)``

Examples
--------

Find the name of the engine making the query:
   ``threaded_engine_self(Engine)``
Check if the name of the engine making the query is ``worker_1``:
   ``threaded_engine_self(worker_1)``

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_destroy_1`,
   :ref:`predicates_threaded_engine_1`
