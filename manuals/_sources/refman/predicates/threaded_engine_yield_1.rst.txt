
.. index:: threaded_engine_yield/1
.. _predicates_threaded_engine_yield_1:

threaded_engine_yield/1
=======================

Description
-----------

::

   threaded_engine_yield(Answer)

Returns an answer independent of the solutions of the engine goal. Fails
if not called from within an engine. This predicate is usually used when
the engine goal is call to a recursive predicate processing terms from
the engine term queue.

This predicate blocks until the returned answer is consumed.

Note that this predicate should not be called as the last element of a
conjunction resulting in an engine goal solution as, in this case, an
answer will always be returned. For example, instead of
``(threaded_engine_yield(ready); member(X,[1,2,3]))`` use
``(X=ready; member(X,[1,2,3]))``.

Template and modes
------------------

::

   threaded_engine_yield(@term)

Errors
------

``(none)``

Examples
--------

Returns the atom ``ready`` as an engine answer:
   ``threaded_engine_yield(ready)``

See also
--------

:ref:`predicates_threaded_engine_create_3`,
:ref:`predicates_threaded_engine_next_2`,
:ref:`predicates_threaded_engine_next_reified_2`
