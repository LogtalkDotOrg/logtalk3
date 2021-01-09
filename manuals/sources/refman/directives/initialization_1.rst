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


.. index:: pair: initialization/1; Directive
.. _directives_initialization_1:

``initialization/1``
====================

Description
-----------

::

   initialization(Goal)

When used within an object, this directive defines a goal to be called
after the object has been loaded into memory. When used at a global
level within a source file, this directive defines a goal to be called
after the compiled source file is loaded into memory.

Multiple initialization directives can be used in a source file or in an
object. Their goals will be called in the same order as the directives at
loading time.

.. note::

   Categories and protocols cannot contain ``initialization/1`` directives
   as the initialization goals would lack a complete
   :term:`execution context <predicate execution context>` that is only
   available for objects.

Although technically a global ``initialization/1`` directive in a source
file is a Prolog directive, calls to Logtalk built-in predicates from it
are usually compiled to improve portability, improve performance, and
provide better support for embedded applications.

.. warning::

   Some backend Prolog compilers declare ``initialization`` as an operator
   for a lighter syntax. But this makes the code non-portable and is
   a practice best avoided.

Template and modes
------------------

::

   initialization(@callable)

Examples
--------

::

   % call the init/0 predicate after loading the
   % source file containing the directive:
   :- initialization(init).
