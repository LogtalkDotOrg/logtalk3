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


.. index:: pair: threaded_wait/1; Built-in predicate
.. _predicates_threaded_wait_1:

``threaded_wait/1``
===================

Description
-----------

::

   threaded_wait(Term)
   threaded_wait([Term| Terms])

Suspends the thread making the call until a notification is received
that unifies with ``Term``. The call must be made within the same object
(:term:`this`) containing the calls to the
:ref:`predicates_threaded_notify_1` predicate that will
eventually send the notification. The argument may also be a list of
notifications, ``[Term| Terms]``. In this case, the thread making the
call will suspend until all notifications in the list are received.

Modes and number of proofs
--------------------------

::

   threaded_wait(?term) - one
   threaded_wait(+list(term)) - one

Errors
------

(none)

Examples
--------

::

   % wait until the "data_available" notification is received:
   ..., threaded_wait(data_available), ...

.. seealso::

   :ref:`predicates_threaded_notify_1`
