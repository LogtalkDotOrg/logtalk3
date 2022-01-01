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


.. index:: pair: object/1-5; Directive
.. _directives_object_1_5:

``object/1-5``
==============

Description
-----------

*Stand-alone objects (prototypes)*

::

   object(Object)

   object(Object,
       implements(Protocols))

   object(Object,
       imports(Categories))

   object(Object,
       implements(Protocols),
       imports(Categories))

*Prototype extensions*

::

   object(Object,
       extends(Objects))

   object(Object,
       implements(Protocols),
       extends(Objects))

   object(Object,
       imports(Categories),
       extends(Objects))

   object(Object,
       implements(Protocols),
       imports(Categories),
       extends(Objects))

*Class instances*

::

   object(Object,
       instantiates(Classes))

   object(Object,
       implements(Protocols),
       instantiates(Classes))

   object(Object,
       imports(Categories),
       instantiates(Classes))

   object(Object,
       implements(Protocols),
       imports(Categories),
       instantiates(Classes))

*Classes*

::

   object(Object,
       specializes(Classes))

   object(Object,
       implements(Protocols),
       specializes(Classes))

   object(Object,
       imports(Categories),
       specializes(Classes))

   object(Object,
       implements(Protocols),
       imports(Categories),
       specializes(Classes))

*Classes with metaclasses*

::

   object(Object,
       instantiates(Classes),
       specializes(Classes))

   object(Object,
       implements(Protocols),
       instantiates(Classes),
       specializes(Classes))

   object(Object,
       imports(Categories),
       instantiates(Classes),
       specializes(Classes))

   object(Object,
       implements(Protocols),
       imports(Categories),
       instantiates(Classes),
       specializes(Classes))

Starting object directive.

Template and modes
------------------

*Stand-alone objects (prototypes)*

::

   object(+object_identifier)

   object(+object_identifier,
       implements(+implemented_protocols))

   object(+object_identifier,
       imports(+imported_categories))

   object(+object_identifier,
       implements(+implemented_protocols),
       imports(+imported_categories))

*Prototype extensions*

::

   object(+object_identifier,
       extends(+extended_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       extends(+extended_objects))

   object(+object_identifier,
       imports(+imported_categories),
       extends(+extended_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       imports(+imported_categories),
       extends(+extended_objects))

*Class instances*

::

   object(+object_identifier,
       instantiates(+instantiated_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       instantiates(+instantiated_objects))

   object(+object_identifier,
       imports(+imported_categories),
       instantiates(+instantiated_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       imports(+imported_categories),
       instantiates(+instantiated_objects))

*Classes*

::

   object(+object_identifier,
       specializes(+specialized_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       specializes(+specialized_objects))

   object(+object_identifier,
       imports(+imported_categories),
       specializes(+specialized_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       imports(+imported_categories),
       specializes(+specialized_objects))

*Class with metaclasses*

::

   object(+object_identifier,
       instantiates(+instantiated_objects),
       specializes(+specialized_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       instantiates(+instantiated_objects),
       specializes(+specialized_objects))

   object(+object_identifier,
       imports(+imported_categories),
       instantiates(+instantiated_objects),
       specializes(+specialized_objects))

   object(+object_identifier,
       implements(+implemented_protocols),
       imports(+imported_categories),
       instantiates(+instantiated_objects),
       specializes(+specialized_objects))

Examples
--------

::

   :- object(list).

   :- object(list,
       implements(listp)).

   :- object(list,
       extends(compound)).

   :- object(list,
       implements(listp),
       extends(compound)).

   :- object(object,
       imports(initialization),
       instantiates(class)).

   :- object(abstract_class,
       instantiates(class),
       specializes(object)).

   :- object(agent,
       imports(private::attributes)).

.. seealso::

   :ref:`directives_end_object_0`
