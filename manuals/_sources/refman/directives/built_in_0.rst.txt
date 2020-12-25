..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: built_in/0; Directive
.. _directives_built_in_0:

``built_in/0``
==============

Description
-----------

::

   built_in

Declares an entity as built-in. Built-in entities must be static and cannot
be redefined once loaded. This directive is used in the pre-defined protocols,
categories, and objects that are automatically loaded at startup.

Template and modes
------------------

::

   built_in

Examples
--------

::

   :- built_in.
