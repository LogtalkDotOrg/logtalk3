..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: protected/1; Directive
.. _directives_protected_1:

protected/1
===========

Description
-----------

::

   protected(Name/Arity)
   protected((Name/Arity, ...))
   protected([Name/Arity, ...])

   protected(Name//Arity)
   protected((Name//Arity, ...))
   protected([Name//Arity, ...])

   protected(op(Precedence,Associativity,Operator))
   protected((op(Precedence,Associativity,Operator), ...))
   protected([op(Precedence,Associativity,Operator), ...])

Declares protected predicates, protected grammar rule non-terminals, and
protected operators. A protected predicate can only be called from the
object containing the directive or from an object that inherits the
directive. A protected non-terminal can only be used as an argument in a
:ref:`methods_phrase_2` and :ref:`methods_phrase_3` calls from the object
containing the directive or from an object that inherits the directive.
Protected operators are not inherited but declaring them provides a
reusable specification for using them in descendant objects (or categories).

Template and modes
------------------

::

   protected(+predicate_indicator_term)
   protected(+non_terminal_indicator_term)
   protected(+operator_declaration)

Examples
--------

::

   :- protected(init/1).

   :- protected((print/2, convert/4)).

   :- protected([load/1, save/3]).

.. seealso::

   :ref:`directives_private_1`,
   :ref:`directives_public_1`,
   :ref:`methods_predicate_property_2`
