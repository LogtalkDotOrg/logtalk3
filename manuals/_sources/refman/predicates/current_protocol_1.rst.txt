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


.. index:: current_protocol/1
.. _predicates_current_protocol_1:

current_protocol/1
==================

Description
-----------

::

   current_protocol(Protocol)

Enumerates, by backtracking, all currently defined protocols. All
protocols are found, either static, dynamic, or built-in.

Modes and number of proofs
--------------------------

::

   current_protocol(?protocol_identifier) - zero_or_more

Errors
------

| Protocol is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, Protocol)``

Examples
--------

::

   % enumerate the defined protocols:
   | ?- current_protocol(Protocol).
   
   Protocol = expanding ;
   Protocol = monitoring ;
   Protocol = forwarding ;
   ...

.. seealso::

   :ref:`predicates_abolish_protocol_1`,
   :ref:`predicates_create_protocol_3`,
   :ref:`predicates_protocol_property_2`,
   :ref:`predicates_conforms_to_protocol_2_3`,
   :ref:`predicates_extends_protocol_2_3`,
   :ref:`predicates_implements_protocol_2_3`
