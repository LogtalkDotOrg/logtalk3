
.. index:: object_property/2
.. _predicates_object_property_2:

object_property/2
=================

Description
-----------

::

   object_property(Object, Property)

Enumerates, by backtracking, the properties associated with the defined
objects. The valid object properties are listed in the language
:ref:`grammar <grammar_entity_properties>`.

Template and modes
------------------

::

   object_property(?object_identifier, ?object_property)

Errors
------

Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``
Property is neither a variable nor a callable term:
   ``type_error(callable, Property)``
Property is a callable term but not a valid object property:
   ``domain_error(object_property, Property)``

Examples
--------

::

   | ?- object_property(list, Property).

.. seealso::

   :ref:`predicates_abolish_object_1`,
   :ref:`predicates_create_object_4`,
   :ref:`predicates_current_object_1`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`,
   :ref:`predicates_complements_object_2`
