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


.. index:: pair: \+/1; Built-in method
.. _methods_not_1:

\\+/1
=====

Description
-----------

::

   \+ Goal

Not-provable meta-predicate. True iff ``call(Goal)`` is false. This
built-in meta-predicate is declared as a private method and thus cannot
be used as a message to an object.

Modes and number of proofs
--------------------------

::

   \+ +callable - zero_or_one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Not-provable goal in the context of the object or category containing the call:
|     ``\+ Goal``
| Not-provable goal sent as a message to :term:`self`:
|     ``\+ ::Goal``
| Not-provable goal sent as a message to an explicit object:
|     ``\+ Object::Goal``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_ignore_1`,
   :ref:`methods_once_1`
