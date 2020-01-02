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


.. _faq_compatibility:

Compatibility
=============

* :ref:`faq_compatibility_requirements`
* :ref:`faq_compatibility_constraints`
* :ref:`faq_compatibility_modules`

.. _faq_compatibility_requirements:

What are the backend Prolog compiler requirements to run Logtalk?
-----------------------------------------------------------------

See the `backend Prolog compiler requirements guide <https://logtalk.org/backend_requirements.html>`_.

.. _faq_compatibility_constraints:

Can I use constraint-based packages with Logtalk?
-------------------------------------------------

Usually, yes. Some constraint-based packages may define operators
which clash with the ones defined by Logtalk. In these cases,
compatibility with Logtalk depends on the constraint-based packages
providing an alternative for accessing the functionality provided by
those operators. When the constraint solver is encapsulated using a
Prolog module, a possible workaround is to use either explicit module
qualification or encapsulate the call using the :ref:`control_external_call_1`
control construct (thus bypassing the Logtalk compiler).

.. _faq_compatibility_modules:

Can I use Logtalk objects and Prolog modules at the same time?
--------------------------------------------------------------

Yes. In order to call a module predicate from within an object (or category)
you may use an :ref:`directives_use_module_2` directive or use explicit
module qualification (possibly wrapping the call using the Logtalk
control construct :ref:`control_external_call_1` that allows bypassing of
the Logtalk compiler when compiling a predicate call). Logtalk also allows
modules to be compiled as objects (see the :ref:`migration_migration`
for details).
