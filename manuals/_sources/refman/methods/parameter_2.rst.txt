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


.. index:: parameter/2
.. _methods_parameter_2:

parameter/2
===========

Description
-----------

::

   parameter(Number, Term)

Used in :term:`parametric objects <parametric entity>` (and
parametric categories), this private method provides runtime access to
the parameter values of the entity that contains the predicate clause
whose body is being executed by using the argument number in the entity
identifier. This predicate is implemented as a unification between its
second argument and the corresponding implicit execution-context
argument in the predicate clause making the call. This unification
occurs at the clause head when the second argument is not instantiated
(the most common case). When the second argument is instantiated, the
unification must be delayed to runtime and thus occurs at the clause
body.

Entity parameters can also be accessed using *parameter variables*,
which use the syntax ``_VariableName_``. The compiler recognizes
occurrences of these variables in entity clauses. Parameter variables
allows us to abstract parameter positions thus simplifying code
maintenance.

Modes and number of proofs
--------------------------

::

   parameter(+integer, ?term) - zero_or_one

Errors
------

Number is a variable:
   ``instantiation_error``
Number is neither a variable nor an integer value:
   ``type_error(integer, Number)``
Number is smaller than one or greater than the parametric entity identifier arity:
   ``domain_error(out_of_range, Number)``
Entity identifier is not a compound term:
   ``type_error(compound, Entity)``

Examples
--------

::

   :- object(box(_Color, _Weight)).

       ...
       
       % this clause is translated into
       % a fact upon compilation
       color(Color) :-
           parameter(1, Color).
       
       % upon compilation, the >/2 call will be
       % the single goal in the clause body
       heavy :-
           parameter(2, Weight),
           Weight > 10.
       ...

The same example using *parameter variables*:

::

   :- object(box(_Color_, _Weight_)).

       ...
       
       color(_Color_).

       heavy :-
           _Weight_ > 10.

       ...

.. seealso::

   :ref:`methods_context_1`,
   :ref:`methods_self_1`,
   :ref:`methods_sender_1`,
   :ref:`methods_this_1`
