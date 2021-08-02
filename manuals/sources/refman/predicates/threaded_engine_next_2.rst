..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: threaded_engine_next/2; Built-in predicate
.. _predicates_threaded_engine_next_2:

``threaded_engine_next/2``
==========================

Description
-----------

::

   threaded_engine_next(Engine, Answer)

Retrieves an answer from an engine and signals it to start computing the
next answer. This predicate blocks until an answer becomes available.
The predicate fails when there are no more solutions to the engine goal.
If the engine goal throws an exception, calling this predicate will
re-throw the exception and subsequent calls will fail.

.. note::

   This predicate requires a :term:`backend Prolog compiler` providing
   compatible multi-threading primitives. The value of the read-only
   :ref:`engines <flag_engines>` flag is set to ``supported`` when that
   is the case.

Modes and number of proofs
--------------------------

::

   threaded_engine_next(@nonvar, ?term) - zero_or_one

Errors
------

| ``Engine`` is a variable:
|     ``instantiation_error``
| ``Engine`` is neither a variable nor the name of an existing engine:
|     ``existence_error(engine, Engine)``

Examples
--------

::

   % get the next answer from the worker_1 engine:
   | ?- threaded_engine_next(worker_1, Answer).

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_next_reified_2`,
   :ref:`predicates_threaded_engine_yield_1`
