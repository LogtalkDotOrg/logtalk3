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


.. index:: pair: set_logtalk_flag/2; Directive
.. _directives_set_logtalk_flag_2:

``set_logtalk_flag/2``
======================

Description
-----------

::

   set_logtalk_flag(Flag, Value)

Sets local flag values. The scope of this directive is the entity or
the source file containing it. For global scope, use the corresponding
:ref:`predicates_set_logtalk_flag_2` built-in predicate called from an
:ref:`directives_initialization_1` directive. For a description of the
predefined compiler flags, consult the :ref:`programming_flags`
section in the User Manual. The directive affects the compilation of
all terms that follow it within scope of the directive.

Template and modes
------------------

::

   set_logtalk_flag(+atom, +nonvar)

Errors
------

| Flag is a variable:
|     ``instantiation_error``
| Value is a variable:
|     ``instantiation_error``
| Flag is not an atom:
|     ``type_error(atom, Flag)``
| Flag is neither a variable nor a valid flag:
|     ``domain_error(flag, Flag)``
| Value is not a valid value for flag Flag:
|     ``domain_error(flag_value, Flag + Value)``
| Flag is a read-only flag:
|     ``permission_error(modify, flag, Flag)``

Examples
--------

::

   % turn off the compiler unknown entity warnings
   % during the compilation of this source file:
   :- set_logtalk_flag(unknown_entities, silent).


   :- object(...).

       % generate events for messages sent from this object:
       :- set_logtalk_flag(events, allow).
       ...

       % turn off suspicious call lint checks for the next predicate:
       :- set_logtalk_flag(suspicious_calls, silent).
       foo :-
           ...   
       :- set_logtalk_flag(suspicious_calls, warning).
       ...
