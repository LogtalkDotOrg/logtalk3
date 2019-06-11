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


.. index:: pair: else/0; Directive
.. _directives_else_0:

else/0
======

Description
-----------

::

   else

Starts an *else* branch when performing conditional compilation.
The code following this directive is compiled iff the goal in the
matching :ref:`directives_if_1` directive is false.

Template and modes
------------------

::

   else

Examples
--------

An example where an hypothetic application would have some limitations
that the user should be made aware when running on a backend Prolog
compiler with bounded arithmetic:

::

   :- if(current_prolog_flag(bounded, true)).

       :- initialization(
           logtalk::print_message(warning,app,bounded_arithmetic)
       ).
   
   :- else.

       :- initialization(
           logtalk::print_message(comment,app,unbounded_arithmetic)
       ).

   :- endif.

.. seealso::

   :ref:`directives_elif_1`,
   :ref:`directives_endif_0`,
   :ref:`directives_if_1`
