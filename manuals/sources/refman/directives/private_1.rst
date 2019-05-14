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


.. index:: pair: private/1; Directive
.. _directives_private_1:

private/1
=========

Description
-----------

::

   private(Name/Arity)
   private((Name/Arity, ...))
   private([Name/Arity, ...])

   private(Name//Arity)
   private((Name//Arity, ...))
   private([Name//Arity, ...])


   private(op(Precedence,Associativity,Operator))
   private((op(Precedence,Associativity,Operator), ...))
   private([op(Precedence,Associativity,Operator), ...])

Declares private predicates, private grammar rule non-terminals, and
private operators. A private predicate can only be called from the
object containing the private directive. A private non-terminal can
only be used in a call of the :ref:`methods_phrase_2` and
:ref:`methods_phrase_3` methods from the object
containing the private directive.

Template and modes
------------------

::

   private(+predicate_indicator_term)
   private(+non_terminal_indicator_term)
   private(+operator_declaration)

Examples
--------

::

   :- private(counter/1).

   :- private((init/1, free/1)).

   :- private([data/3, key/1, keys/1]).

.. seealso::

   :ref:`directives_protected_1`,
   :ref:`directives_public_1`,
   :ref:`methods_predicate_property_2`
