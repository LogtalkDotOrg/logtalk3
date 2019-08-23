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


.. index:: pair: protocol_property/2; Built-in predicate
.. _predicates_protocol_property_2:

protocol_property/2
===================

Description
-----------

::

   protocol_property(Protocol, Property)

Enumerates, by backtracking, the properties associated with the currently
defined protocols. The valid properties are listed in the language grammar
section on :ref:`entity properties <grammar_entity_properties>` and described
in the User Manual section on :ref:`protocol properties <protocols_properties>`.

Modes and number of proofs
--------------------------

::

   protocol_property(?protocol_identifier, ?protocol_property) - zero_or_more

Errors
------

| Protocol is neither a variable nor a valid protocol identifier:
|     ``type_error(protocol_identifier, Protocol)``
| Property is neither a variable nor a callable term:
|     ``type_error(callable, Property)``
| Property is a callable term but not a valid protocol property:
|     ``domain_error(protocol_property, Property)``

Examples
--------

::

   % enumerate the properties of the monitoring built-in protocol:
   | ?- protocol_property(monitoring, Property).
   
   Property = source_data ;
   Property = static ;
   Property = built_in ;
   ...

.. seealso::

   :ref:`predicates_abolish_protocol_1`,
   :ref:`predicates_create_protocol_3`,
   :ref:`predicates_current_protocol_1`,
   :ref:`predicates_conforms_to_protocol_2_3`,
   :ref:`predicates_extends_protocol_2_3`,
   :ref:`predicates_implements_protocol_2_3`
