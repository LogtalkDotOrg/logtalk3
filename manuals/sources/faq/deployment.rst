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


.. _faq_deployment:

Deployment
==========

* :ref:`faq_deployment_create`

.. _faq_deployment_create:

Can I create standalone applications with Logtalk?
--------------------------------------------------

It depends on the Prolog compiler that you use to run Logtalk. Assuming
that your Prolog compiler supports the creation of standalone executables,
your application must include the adapter file for your compiler and the
Logtalk compiler and runtime. The distribution includes embedding scripts
for selected backend Prolog compilers and embedding examples.

For instructions on how to embed Logtalk and Logtalk applications see
the `embedding guide <https://logtalk.org/embedding.html>`_.
