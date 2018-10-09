
.. index:: current_object/1
.. _predicates_current_object_1:

current_object/1
================

Description
-----------

::

   current_object(Object)

Enumerates, by backtracking, all currently defined objects. All objects
are found, either static, dynamic or built-in.

Template and modes
------------------

::

   current_object(?object_identifier)

Errors
------

Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``

Examples
--------

::

   | ?- current_object(list).

.. seealso::

   :ref:`predicates_abolish_object_1`,
   :ref:`predicates_create_object_4`,
   :ref:`predicates_object_property_2`,
   :ref:`predicates_extends_object_2_3`,
   :ref:`predicates_instantiates_class_2_3`,
   :ref:`predicates_specializes_class_2_3`
