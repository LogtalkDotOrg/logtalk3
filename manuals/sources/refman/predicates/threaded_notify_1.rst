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


.. index:: pair: threaded_notify/1; Built-in predicate
.. _predicates_threaded_notify_1:

threaded_notify/1
=================

Description
-----------

::

   threaded_notify(Term)
   threaded_notify([Term| Terms])

Sends ``Term`` as a notification to any thread suspended waiting for it
in order to proceed. The call must be made within the same object
(:term:`this`) containing the calls to the
:ref:`predicates_threaded_wait_1` predicate waiting for the
notification. The argument may also be a list of notifications,
``[Term| Terms]``. In this case, all notifications in the list will be
sent to any threads suspended waiting for them in order to proceed.

Modes and number of proofs
--------------------------

::

   threaded_notify(@term) - one
   threaded_notify(@list(term)) - one

Errors
------

(none)

Examples
--------

::

   % send a "data_available" notification:
   ..., threaded_notify(data_available), ...

.. seealso::

   :ref:`predicates_threaded_wait_1`
