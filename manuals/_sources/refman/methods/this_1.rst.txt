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


.. index:: this/1
.. _methods_this_1:

this/1
======

Description
-----------

::

   this(This)

Unifies its argument with the identifier of the object for which the
predicate clause whose body is being executed is defined (or the object
importing the category that contains the predicate clause). This private
method is implemented as a unification between its argument and the
corresponding implicit execution-context argument in the predicate
clause making the call. This unification occurs at the clause head when
the argument is not instantiated (the most common case). This method is
useful for avoiding hard-coding references to an object identifier or
for retrieving all object parameters with a single call when using
parametric objects.

Modes and number of proofs
--------------------------

::

   this(?object_identifier) - zero_or_one

Errors
------

(none)

Examples
--------

::

   % after compilation, the write/1 call will
   % be the first goal on the clause body
   test :-
       this(This),
       write('Using a predicate clause contained in '),
       writeq(This), nl.

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_parameter_2`,
   :ref:`methods_self_1`,
   :ref:`methods_sender_1`
