
.. index:: complements_object/2
.. _predicates_complements_object_2:

complements_object/2
====================

Description
-----------

::

   complements_object(Category, Object)

Enumerates, by backtracking, all category–object pairs such that the
category explicitly complements the object.

Template and modes
------------------

::

   complements_object(?category_identifier, ?object_identifier)

Errors
------

Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Prototype)``
Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Parent)``

Examples
--------

::

   | ?- complements_object(logging, employee).

See also
--------

:ref:`predicates_current_category_1`,
:ref:`predicates_imports_category_2_3`
