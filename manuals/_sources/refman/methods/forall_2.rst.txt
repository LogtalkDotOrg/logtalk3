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


.. index:: pair: forall/2; Built-in method
.. _methods_forall_2:

forall/2
========

Description
-----------

::

   forall(Generator, Test)

For all solutions of ``Generator``, ``Test`` is true. This meta-predicate
implements a *generate-and-test* loop using a definition equivalent to
``\+ (Generator, \+ Test)``.

This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

Modes and number of proofs
--------------------------

::

   forall(@callable, @callable) - zero_or_one

Errors
------

| Either Generator or Test is a variable:
|     ``instantiation_error``
| Generator is neither a variable nor a callable term:
|     ``type_error(callable, Generator)``
| Test is neither a variable nor a callable term:
|     ``type_error(callable, Test)``

Examples
--------

| To call both goals in the context of the object or category containing the call:
|     ``forall(Generator, Test)``
| To send both goals as messages to :term:`self`:
|     ``forall(::Generator, ::Test)``
| To send both goals as messages to explicit objects:
|     ``forall(Object1::Generator, Object2::Test)``

.. seealso::

   :ref:`methods_bagof_3`,
   :ref:`methods_findall_3`,
   :ref:`methods_findall_4`,
   :ref:`methods_setof_3`
