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


.. index:: create_logtalk_flag/3
.. _predicates_create_logtalk_flag_3:

create_logtalk_flag/3
=====================

Description
-----------

::

   create_logtalk_flag(Flag, Value, Options)

Creates a new Logtalk flag and sets its default value. User-defined
flags can be queried and set in the same way as pre-defined flags by
using, respectively, the :ref:`predicates_current_logtalk_flag_2` and
:ref:`predicates_set_logtalk_flag_2` built-in predicates.

This predicate is based on the specification of the SWI-Prolog
``create_prolog_flag/3`` built-in predicate and supports the same
options: ``access(Access)``, where ``Access`` can be either
``read_write`` (the default) or ``read_only``; ``keep(Keep)``, where
``Keep`` can be either ``false`` (the default) or ``true``, for deciding
if an existing definition of the flag should be kept or replaced by the
new one; and ``type(Type)`` for specifying the type of the flag, which
can be ``boolean``, ``atom``, ``integer``, ``float``, or ``term`` (which
only restricts the flag value to ground terms). When the ``type/1``
option is not specified, the type of the flag is inferred from its
initial value.

Modes and number of proofs
--------------------------

::

   create_logtalk_flag(+atom, +ground, +list(ground)) - one

Errors
------

Flag is a variable:
   ``instantiation_error``
Value is not a ground term:
   ``instantiation_error``
Options is not a ground term:
   ``instantiation_error``
Flag is neither a variable nor an atom:
   ``type_error(atom, Flag)``
Options is neither a variable nor a list:
   ``type_error(atom, Flag)``
Value is not a valid value for flag Flag:
   ``domain_error(flag_value, Flag + Value)``
Flag is a system-defined flag:
   ``permission_error(modify, flag, Flag)``
An element Option of the list Options is not a valid option
   domain_error(flag_option,Option)
The list Options contains a type(Type) option and Value is not a Type term
   type_error(Type, Value)

Examples
--------

::

   | ?- create_logtalk_flag(pretty_print_blobs, false, []).

.. seealso::

   :ref:`predicates_current_logtalk_flag_2`,
   :ref:`predicates_set_logtalk_flag_2`
