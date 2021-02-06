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


.. index:: pair: self/1; Built-in method
.. _methods_self_1:

``self/1``
==========

Description
-----------

::

   self(Self)

Returns the object that has received the message under processing. This
private method is translated to a unification between its argument and
the corresponding implicit context argument in the predicate clause making
the call. This unification occurs at the clause head when the argument
is not instantiated (the most common case).

Modes and number of proofs
--------------------------

::

   self(?object_identifier) - zero_or_one

Errors
------

(none)

Examples
--------

::

   % upon compilation, the write/1 call will be
   % the first goal on the clause body
   test :-
       self(Self),
       write('executing a method in behalf of '),
       writeq(Self), nl.

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_parameter_2`,
   :ref:`methods_sender_1`,
   :ref:`methods_this_1`
