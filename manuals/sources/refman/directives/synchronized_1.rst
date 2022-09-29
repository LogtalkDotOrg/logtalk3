..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


.. rst-class:: align-right

**directive**

.. index:: pair: synchronized/1; Directive
.. _directives_synchronized_1:

``synchronized/1``
==================

Description
-----------

::

   synchronized(Name/Arity)
   synchronized((Name/Arity, ...))
   synchronized([Name/Arity, ...])

   synchronized(Name//Arity)
   synchronized((Name//Arity, ...))
   synchronized([Name//Arity, ...])

Declares synchronized predicates and synchronized grammar rule non-terminals.
The most common use is for predicates that have side effects (e.g. asserting
or retracting clauses for a dynamic predicate) in multi-threaded applications.
A synchronized predicate (or synchronized non-terminal) is protected by a
mutex in order to allow for thread synchronization when proving a call to
the predicate (or non-terminal).

All predicates (and non-terminals) declared in the same synchronized
directive share the same mutex. In order to use a separate mutex for
each predicate (non-terminal) so that they are independently synchronized,
a per-predicate synchronized directive must be used.

.. warning::

   Declaring a predicate synchronized implicitly makes it **deterministic**.
   When using a single-threaded :term:`backend Prolog compiler`, calls
   to synchronized predicates behave as wrapped by the standard
   :ref:`methods_once_1` meta-predicate.

Note that synchronized predicates cannot be declared
:ref:`dynamic <directives_dynamic_1>` (when necessary, declare the
predicates updating the dynamic predicates as synchronized).

Template and modes
------------------

::

   synchronized(+predicate_indicator_term)
   synchronized(+non_terminal_indicator_term)

Examples
--------

::

   :- synchronized(db_update/1).

   :- synchronized((write_stream/2, read_stream/2)).

   :- synchronized([add_to_queue/2, remove_from_queue/2]).

.. seealso::

   :ref:`methods_predicate_property_2`
