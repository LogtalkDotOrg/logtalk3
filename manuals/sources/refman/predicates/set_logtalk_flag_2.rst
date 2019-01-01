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


.. index:: set_logtalk_flag/2
.. _predicates_set_logtalk_flag_2:

set_logtalk_flag/2
==================

Description
-----------

::

   set_logtalk_flag(Flag, Value)

Sets Logtalk default, global, flag values. For local flag scope, use the
corresponding :ref:`directives_set_logtalk_flag_2`
directive. To set a global flag value when compiling and loading a
source file, wrap the calls to this built-in predicate with an
:ref:`directives_initialization_1` directive.

Modes and number of proofs
--------------------------

::

   set_logtalk_flag(+atom, +nonvar) - one

Errors
------

| Flag is a variable:
|     ``instantiation_error``
| Value is a variable:
|     ``instantiation_error``
| Flag is neither a variable nor an atom:
|     ``type_error(atom, Flag)``
| Flag is an atom but an invalid flag:
|     ``domain_error(flag, Flag)``
| Value is not a valid value for flag Flag:
|     ``domain_error(flag_value, Flag + Value)``
| Flag is a read-only flag:
|     ``permission_error(modify, flag, Flag)``

Examples
--------

::

   % turn off globally and by default the compiler
   % unknown entities warnings:
   | ?- set_logtalk_flag(unknown_entities, silent).

.. seealso::

   :ref:`predicates_create_logtalk_flag_3`,
   :ref:`predicates_current_logtalk_flag_2`
