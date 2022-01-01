..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


.. _faq_general:

General
=======

* :ref:`faq_general_versions`
* :ref:`faq_general_prolog`
* :ref:`faq_general_modules`
* :ref:`faq_general_expansion`

.. _faq_general_versions:

Why are all versions of Logtalk numbered 2.x or 3.x?
----------------------------------------------------

The numbers "2" and "3" in the Logtalk version string refers to,
respectively, the second and the third generations of the Logtalk
language. Development of Logtalk 2 started on January 1998, with the
first alpha release for registered users on July and the first public
beta on October. The first stable version of Logtalk 2 was released
on February 9, 1999. Development of Logtalk 3 started on April 2012,
with the first public alpha released on August 21, 2012. The first
stable version of Logtalk 3 was released on January 7, 2015.

.. _faq_general_prolog:

Why do I need a Prolog compiler to use Logtalk?
-----------------------------------------------

Currently, the Logtalk language is implemented as a Prolog extension
instead of as a standalone compiler. Compilation of Logtalk source
files is performed in two steps. First, the Logtalk compiler converts
a source file to a Prolog file. Second, the chosen Prolog compiler is
called by Logtalk to compile the intermediate Prolog file generated
on the first step. The implementation of Logtalk as a Prolog
extension allows users to use Logtalk together with features only
available on specific Prolog compilers.

.. _faq_general_modules:

Is the Logtalk implementation based on Prolog modules?
------------------------------------------------------

No. Logtalk is (currently) implemented is plain Prolog code. Only a
few Prolog compilers include a module system, with several
compatibility problems between them. Moreover, the current ISO Prolog
standard for modules is next to worthless and is ignored by most of
the Prolog community. Nevertheless, the Logtalk compiler is able to
compile simple modules (using a common subset of module directives)
as objects for backward-compatibility with existing code (see the
:ref:`migration_migration` for details).

.. _faq_general_expansion:

Does the Logtalk implementation use term-expansion?
---------------------------------------------------

No. Term-expansion mechanisms are not standard and are not available
in all supported Prolog compilers.
