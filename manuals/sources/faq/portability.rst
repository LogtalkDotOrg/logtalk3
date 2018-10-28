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


.. _faq_portability:

Portability
===========

* :ref:`faq_portability_prolog`
* :ref:`faq_portability_os`

.. _faq_portability_prolog:

Are my Logtalk applications portable across Prolog compilers?
-------------------------------------------------------------

Yes, as long you don't use built-in predicates or special features
only available on some Prolog compilers. There is a compiler flag
(``portability``) that you can set to instruct Logtalk to print a
warning for each occurrence of non-ISO Prolog standard features such
as proprietary built-in predicates. In addition, it is advisable that you constrain,
if possible, the use of platform or compiler dependent code to a
small number of objects with clearly defined protocols. You may also
use Logtalk support for conditional compilation to compile different
entity or predicate definitions depending on the backend Prolog
compiler being used.

.. _faq_portability_os:

Are my Logtalk applications portable across operating systems?
--------------------------------------------------------------

Yes, as long you don't use built-in predicates or special features
that your chosen backend Prolog compiler only supports in some
operating-systems. You may need to change the end-of-lines characters
of your source files to match the ones on the target operating system
and the expectations of your Prolog compiler. Some Prolog compilers
silently fail to compile source files with the wrong end-of-lines
characters.
