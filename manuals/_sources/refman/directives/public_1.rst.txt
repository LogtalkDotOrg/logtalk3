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


.. index:: public/1
.. _directives_public_1:

public/1
========

Description
-----------

::

   public(Name/Arity)
   public((Name/Arity, ...))
   public([Name/Arity, ...])

   public(Name//Arity)
   public((Name//Arity, ...))
   public([Name//Arity, ...])

   public(op(Precedence,Associativity,Operator))
   public((op(Precedence,Associativity,Operator), ...))
   public([op(Precedence,Associativity,Operator), ...])

Declares public predicates, public grammar rule non-terminals, and
public operators. A public predicate can be called from any object. A
public non-terminal can be used as an argument in :ref:`methods_phrase_2`
and :ref:`methods_phrase_3` calls from any object. Public operators are
not exported but declaring them provides useful information for defining
client objects.

Template and modes
------------------

::

   public(+predicate_indicator_term)
   public(+non_terminal_indicator_term)
   public(+operator_declaration)

Examples
--------

::

   :- public(ancestor/1).

   :- public((instance/1, instances/1)).

   :- public([leaf/1, leaves/1]).

.. seealso::

   :ref:`directives_private_1`,
   :ref:`directives_protected_1`,
   :ref:`methods_predicate_property_2`
