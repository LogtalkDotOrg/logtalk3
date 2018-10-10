
.. index:: threaded_engine_fetch/1
.. _predicates_threaded_engine_fetch_1:

threaded_engine_fetch/1
=======================

Description
-----------

::

   threaded_engine_fetch(Term)

Fetches a term from the engine term queue. Blocks until a term is
available. Fails if not called from within an engine.

Template and modes
------------------

::

   threaded_engine_fetch(?term)

Errors
------

``(none)``

Examples
--------

Fetch a term from the engine term queue:
   ``threaded_engine_fetch(Term)``

.. seealso::

   :ref:`predicates_threaded_engine_post_2`
