..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: threaded_engine_next_reified/2
.. _predicates_threaded_engine_next_reified_2:

threaded_engine_next_reified/2
==============================

Description
-----------

::

   threaded_engine_next_reified(Engine, Answer)

Retrieves the next reified answer from an engine. This predicate
always succeeds and blocks until an answer becomes available.
Answers are returned using the terms ``the(Answer)``, ``no``, and
``exception(Error)``.

Modes and number of proofs
--------------------------

::

   threaded_engine_next_reified(@nonvar, ?nonvar) - one

Errors
------

| Engine is a variable:
|     ``instantiation_error``
| Engine is neither a variable nor the name of an existing engine:
|     ``existence_error(engine, Engine)``

Examples
--------

::

   % get the next answer from the worker_1 engine:
   | ?- threaded_engine_next_reified(worker_1, Answer).

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_next_2`,
   :ref:`predicates_threaded_engine_yield_1`
