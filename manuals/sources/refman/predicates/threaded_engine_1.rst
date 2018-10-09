
.. index:: threaded_engine/1
.. _predicates_threaded_engine_1:

threaded_engine/1
=================

Description
-----------

::

   threaded_engine(Engine)

Enumerates, by backtracking, all existing engines. Engine names shall be
regarded as opaque terms; users shall not rely on its type.

Template and modes
------------------

::

   threaded_engine(?nonvar)

Errors
------

``(none)``

Examples
--------

Check that an engine named ``worker_1`` exists:
   ``threaded_engine(worker_1)``
Write the names of all existing engines:
   ``forall(threaded_engine(Engine), (writeq(Engine), nl))``

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_self_1`,
   :ref:`predicates_threaded_engine_destroy_1`
