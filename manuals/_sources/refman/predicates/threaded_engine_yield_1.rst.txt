..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: threaded_engine_yield/1; Built-in predicate
.. _predicates_threaded_engine_yield_1:

``threaded_engine_yield/1``
===========================

Description
-----------

::

   threaded_engine_yield(Answer)

Returns an answer independent of the solutions of the engine goal. Fails
if not called from within an engine. This predicate is usually used when
the engine goal is a call to a recursive predicate processing terms from
the engine term queue.

This predicate blocks until the returned answer is consumed.

Note that this predicate should not be called as the last element of a
conjunction resulting in an engine goal solution as, in this case, an
answer will always be returned. For example, instead of
``(threaded_engine_yield(ready); member(X,[1,2,3]))`` use
``(X=ready; member(X,[1,2,3]))``.

Modes and number of proofs
--------------------------

::

   threaded_engine_yield(@term) - zero_or_one

Errors
------

(none)

Examples
--------

::

   % returns the atom "ready" as an engine answer:
   ..., threaded_engine_yield(ready), ...

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_next_2`,
   :ref:`predicates_threaded_engine_next_reified_2`
