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


.. index:: pair: threaded_engine_self/1; Built-in predicate
.. _predicates_threaded_engine_self_1:

``threaded_engine_self/1``
==========================

Description
-----------

::

   threaded_engine_self(Engine)

Queries the name of engine calling the predicate.

.. note::

   This predicate requires a :term:`backend Prolog compiler` providing
   compatible multi-threading primitives. The value of the read-only
   :ref:`engines <flag_engines>` flag is set to ``supported`` when that
   is the case.

Modes and number of proofs
--------------------------

::

   threaded_engine_self(?nonvar) - zero_or_one

Errors
------

(none)

Examples
--------

::

   % find the name of the engine making the query:
   ..., threaded_engine_self(Engine), ...

   % check if the the engine making the query is worker_1:
   ..., threaded_engine_self(worker_1), ...

.. seealso::

   :ref:`predicates_threaded_engine_create_3`,
   :ref:`predicates_threaded_engine_destroy_1`,
   :ref:`predicates_threaded_engine_1`
