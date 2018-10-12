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


.. index:: elif/1
.. _directives_elif_1:

elif/1
======

Description
-----------

::

   elif(Goal)

Supports embedded conditionals when performing conditional compilation.
The code following the directive is compiled if ``Goal`` is true. The
goal is subjected to goal expansion when the directive occurs in a
source file.

Conditional compilation goals cannot depend on predicate definitions
contained in the same source file that contains the conditional
compilation directives (as those predicates only become available after
the file is fully compiled and loaded).

Template and modes
------------------

::

   elif(@callable)

Examples
--------

::

   :- elif(predicate_property(callable(_), built_in)).

.. seealso::

   :ref:`directives_else_0`,
   :ref:`directives_endif_0`,
   :ref:`directives_if_1`
