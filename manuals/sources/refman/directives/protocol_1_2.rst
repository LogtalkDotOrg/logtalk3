..
   This file is part of Logtalk <https://logtalk.org/>
   SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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

.. index:: pair: protocol/1-2; Directive
.. _directives_protocol_1_2:

``protocol/1-2``
================

Description
-----------

::

   protocol(Protocol)

   protocol(Protocol,
       extends(Protocols))

Starting protocol directive.

Template and modes
------------------

::

   protocol(+protocol_identifier)

   protocol(+protocol_identifier,
       extends(+extended_protocols))

Examples
--------

::

   :- protocol(listp).

   :- protocol(listp,
       extends(compoundp)).

   :- protocol(queuep,
       extends(protected::listp)).

.. seealso::

   :ref:`directives_end_protocol_0`
