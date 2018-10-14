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


.. index:: forward/1
.. _methods_forward_1:

forward/1
=========

Description
-----------

::

   forward(Message)

User-defined method for forwarding unknown messages sent to an object
(using the :ref:`control_send_to_object_2` control
construct), automatically called by the runtime when defined. This
method is declared in the ``forwarding`` built-in protocol as a public
predicate. Note that you can make its scope protected or private by
using, respectively, protected or private implementation of the
``forwarding`` protocol.

Modes and number of proofs
--------------------------

::

   forward(+callable) - zero_or_more

Errors
------

(none)

Examples
--------

::

   :- object(proxy,
       implements(forwarding),
       ...).

       forward(Message) :-
           % delegate the unknown message to other object
           [real::Message].

.. seealso::

   :ref:`control_delegate_message_1`
