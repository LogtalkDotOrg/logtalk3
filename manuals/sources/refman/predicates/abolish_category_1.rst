
.. index:: abolish_category/1
.. _predicates_abolish_category_1:

abolish_category/1
==================

Description
-----------

::

   abolish_category(Category)

Abolishes a dynamic category.

Template and modes
------------------

::

   abolish_category(+category_identifier)

Errors
------

Category is a variable:
   ``instantiation_error``
Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Category)``
Category is an identifier of a static category:
   ``permission_error(modify, static_category, Category)``
Category does not exist:
   ``existence_error(category, Category)``

Examples
--------

::

   | ?- abolish_category(monitoring).

See also
--------

:ref:`predicates_category_property_2`,
:ref:`predicates_create_category_4`,
:ref:`predicates_current_category_1`
:ref:`predicates_complements_object_2`,
:ref:`predicates_extends_category_2_3`,
:ref:`predicates_imports_category_2_3`
