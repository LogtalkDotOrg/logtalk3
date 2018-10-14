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

Modes and number of proofs
--------------------------

::

   threaded_engine_fetch(?term) - zero_or_one

Errors
------

(none)

Examples
--------

Fetch a term from the engine term queue:
   ``threaded_engine_fetch(Term)``

.. seealso::

   :ref:`predicates_threaded_engine_post_2`
