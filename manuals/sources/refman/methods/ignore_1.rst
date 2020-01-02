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


.. index:: pair: ignore/1; Built-in method
.. _methods_ignore_1:

ignore/1
========

Description
-----------

::

   ignore(Goal)

This predicate succeeds whether its argument succeeds or fails and it is
not re-executable. This built-in meta-predicate is declared as a private
method and thus cannot be used as a message to an object.

This meta-predicate is opaque to cuts in its argument.

Modes and number of proofs
--------------------------

::

   ignore(+callable) - one

Errors
------

| ``Goal`` is a variable:
|     ``instantiation_error``
| ``Goal`` is neither a variable nor a callable term:
|     ``type_error(callable, Goal)``

Examples
--------

| Call a goal and succeeding even if it fails:
|     ``ignore(Goal)``
| To send a message succeeding even if it fails to :term:`self`:
|     ``ignore(::Goal)``
| To send a message succeeding even if it fails to an explicit object:
|     ``ignore(Object::Goal)``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_once_1`,
   :ref:`methods_not_1`
