..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: threaded_engine_create/3; Built-in predicate
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

Modes and number of proofs
--------------------------

::

   threaded_engine_create(@term, @callable, @nonvar) - one
   threaded_engine_create(@term, @callable, --nonvar) - one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``
| ``Engine`` is the name of an existing engine:
|     ``permission_error(create, engine, Engine)``

Examples
--------

::

   % create a new engine for finding members of a list:
   | ?- threaded_engine_create(X, member(X, [1,2,3]), worker_1).

.. seealso::

   :ref:`predicates_threaded_engine_destroy_1`,
   :ref:`predicates_threaded_engine_self_1`,
   :ref:`predicates_threaded_engine_1`,
   :ref:`predicates_threaded_engine_next_2`,
   :ref:`predicates_threaded_engine_next_reified_2`
