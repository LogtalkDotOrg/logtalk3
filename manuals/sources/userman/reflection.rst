..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: single: reflection
.. _reflection_reflection:

Reflection
==========

Logtalk provides support for both *structural* and *behavioral* reflection.
Structural reflection supports computations over an application structure
while behavioral reflection computations over what an application does while
running. The structural and behavioral reflection APIs are used by all the
:doc:`developer tools <../devtools/index>`, which are regular applications.

.. index:: single: structural reflection
.. _reflection_structural:

Structural reflection
---------------------

Structural reflection allows querying the properties of objects, categories,
protocols, and predicates. This API provides two views on the structure of
an application: a *transparent-box view* and a *black-box view*, described
next.

.. index:: single: transparent-box view

Transparent-box view
~~~~~~~~~~~~~~~~~~~~

The transparent-box view provides a structural view of the contents and
properties of entities, predicates, and source files akin to accessing
the corresponding source code.

For entities, built-in predicates are provided for
:ref:`enumerating entities <enumerating_entity_predicates>`,
:ref:`enumerating entity properties <enumerating_entity_property_predicates>`
(including entity declared, defined, called, and updated predicates),
and :ref:`enumerating entity relations <entity_relation_predicates>`.
For a detailed description of the supported entity properties, see the sections
on :ref:`object properties <objects_properties>`,
:ref:`protocol properties <protocols_properties>`, and
:ref:`category properties <categories_properties>`.
For examples of querying entity relations, see the sections
on :ref:`object relations <objects_relationships>`,
:ref:`protocol relations <protocols_relationships>`, and
:ref:`category relations <categories_relationships>`.

.. note::

   Some entity and predicate properties are only available when the source
   files are compiled with the :ref:`source_data <flag_source_data>` flag
   turned on.

The :ref:`logtalk <apis:logtalk/0>` built-in object provides predicates for
querying loaded source files and their properties. 

.. index:: single: black-box view

Black-box view
~~~~~~~~~~~~~~

The black-box view provides a view that respects entity encapsulation and
thus only allow querying about predicates and operators that are within
scope of the entity calling the reflection methods.

Built-in methods are provided for querying the :ref:`predicates that are
declared and can be called or used as messages <methods_current_predicate_1>`
and for querying the :ref:`predicate properties <methods_predicate_property_2>`.
It is also possible to enumerate :ref:`entity operators <methods_current_op_3>`.
See the sections on :ref:`finding declared predicates <predicates_finding>` and
on :ref:`predicate properties <predicates_properties>` for more details.

.. index:: single: behavioral reflection
.. _reflection_behavioral:

Behavioral reflection
---------------------

Behavioral reflection provides insight on what an application does when running.
Specifically, by observing and acting on the messages being exchanged between
objects. See the section on :ref:`event-driven programming <events_events>`
for details. There is also a :doc:`../libraries/dependents` library that
provides an implementation of Smalltalk dependents mechanism.

For use in debugging tools, there is also a small reflection API providing
:ref:`trace and debug event predicates <debugging_events>` provided by the
:ref:`logtalk <apis:logtalk/0>` built-in object.
