
.. index:: current_category/1
.. _predicates_current_category_1:

current_category/1
==================

Description
-----------

::

   current_category(Category)

Enumerates, by backtracking, all currently defined categories. All
categories are found, either static, dynamic, or built-in.

Template and modes
------------------

::

   current_category(?category_identifier)

Errors
------

Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Category)``

Examples
--------

::

   | ?- current_category(monitoring).

.. seealso::

   :ref:`predicates_abolish_category_1`,
   :ref:`predicates_category_property_2`,
   :ref:`predicates_create_category_4`,
   :ref:`predicates_complements_object_2`,
   :ref:`predicates_extends_category_2_3`,
   :ref:`predicates_imports_category_2_3`
