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


.. index:: pair: endif/0; Directive
.. _directives_endif_0:

``endif/0``
===========

Description
-----------

::

   endif

Ends conditional compilation for the matching :ref:`directives_if_1`
directive.

Template and modes
------------------

::

   endif

Examples
--------

::

   :- if(date::today(_,5,25)).

       :- initialization(write('Happy Towel Day!\n')).

   :- endif.

.. seealso::

   :ref:`directives_elif_1`,
   :ref:`directives_else_0`,
   :ref:`directives_if_1`
