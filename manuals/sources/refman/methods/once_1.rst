..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


.. index:: pair: once/1; Built-in method
.. _methods_once_1:

``once/1``
==========

Description
-----------

::

   once(Goal)

This predicate behaves as ``call(Goal)`` but it is not re-executable.
This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

This meta-predicate is opaque to cuts in its argument.

Modes and number of proofs
--------------------------

::

   once(+callable) - zero_or_one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Call a goal deterministically in the context of the object or category containing the call:
|     ``once(Goal)``
| To send a goal as a non-backtracable message to :term:`self`:
|     ``once(::Goal)``
| To send a goal as a non-backtracable message to an explicit object:
|     ``once(Object::Goal)``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_ignore_1`,
   :ref:`methods_not_1`
