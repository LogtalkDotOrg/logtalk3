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

(none)

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
