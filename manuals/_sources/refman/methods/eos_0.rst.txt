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


.. index:: pair: eos//0; Built-in method
.. _methods_eos_0:

eos//0
======

Description
-----------

::

   eos

This non-terminal matches the end-of-input. It is implemented by
checking that the implicit difference list unifies with ``[]-[]``.

Modes and number of proofs
--------------------------

::

   eos - zero_or_one

Errors
------

(none)

Examples
--------

::

   abc --> a, b, c, eos.

.. seealso::

   :ref:`methods_call_1`,
   :ref:`methods_phrase_1`,
   :ref:`methods_phrase_2`,
   :ref:`methods_phrase_3`
