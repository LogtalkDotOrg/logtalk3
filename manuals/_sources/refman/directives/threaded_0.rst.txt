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


.. rst-class:: align-right

**directive**

.. index:: pair: threaded/0; Directive
.. _directives_threaded_0:

``threaded/0``
==============

Description
-----------

::

   threaded

Declares that an object supports threaded engines, concurrent calls,
and asynchronous messages. Any object containing calls to the built-in
multi-threading predicates (or importing a category that contains such
calls) must include this directive.

This directive results in the automatic creation and set up of an object
message queue when the object is loaded or created at runtime. Object
message queues are used for exchanging thread notifications and for
storing concurrent goal solutions and replies to the multi-threading
calls made within the object. The message queue for the
:ref:`user <objects_user>` pseudo-object is automatically created at
Logtalk startup (provided that multi-threading programming is supported
and enabled for the chosen :term:`backend Prolog compiler`).

.. note::

   This directive requires a :term:`backend Prolog compiler` providing
   compatible multi-threading primitives. The value of the read-only
   :ref:`threads <flag_threads>` flag is set to ``supported`` when that
   is the case.

Template and modes
------------------

::

   threaded

Examples
--------

::

   :- threaded.

.. seealso::

   :ref:`directives_synchronized_1`,
   :ref:`predicates_object_property_2`
