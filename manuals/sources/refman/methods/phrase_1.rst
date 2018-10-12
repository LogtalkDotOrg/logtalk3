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


.. index:: phrase//1
.. _methods_phrase_1:

phrase//1
=========

Description
-----------

::

   phrase(NonTerminal)

This non-terminal takes a non-terminal or a grammar rule body and parses
it using the current implicit list of tokens. A common use is to wrap
what otherwise would be a naked variable in a grammar rule body.

Template and modes
------------------

::

   phrase(+callable)

Errors
------

NonTerminal is a variable:
   ``instantiation_error``
NonTerminal is neither a variable nor a callable term:
   ``type_error(callable, NonTerminal)``

Examples
--------

   (none)

.. seealso::

   :ref:`methods_call_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
