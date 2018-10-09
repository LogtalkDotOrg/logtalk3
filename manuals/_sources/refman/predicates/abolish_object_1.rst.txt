
.. index:: abolish_object/1
.. _predicates_abolish_object_1:

abolish_object/1
================

Description
-----------

::

   abolish_object(Object)

Abolishes a dynamic object.

Template and modes
------------------

::

   abolish_object(+object_identifier)

Errors
------

Object is a variable:
   ``instantiation_error``
Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``
Object is an identifier of a static object:
   ``permission_error(modify, static_object, Object)``
Object does not exist:
   ``existence_error(object, Object)``

Examples
--------

::

   | ?- abolish_object(list).

.. seealso::

   :ref:`predicates_create_object_4`,
   :ref:`predicates_current_object_1`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`
