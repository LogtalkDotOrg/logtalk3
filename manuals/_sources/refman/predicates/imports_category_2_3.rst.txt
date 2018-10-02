
.. index:: imports_category/2-3
.. _predicates_imports_category_2_3:

imports_category/2-3
====================

Description
-----------

::

   imports_category(Object, Category)

   imports_category(Object, Category, Scope)

Enumerates, by backtracking, importation relations between objects and
categories. The relation scope is represented by the atoms ``public``,
``protected``, and ``private``.

Template and modes
------------------

::

   imports_category(?object_identifier, ?category_identifier)

   imports_category(?object_identifier, ?category_identifier, ?scope)

Errors
------

Object is neither a variable nor a valid object identifier:
   ``type_error(object_identifier, Object)``
Category is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Category)``
Scope is neither a variable nor an atom:
   ``type_error(atom, Scope)``
Scope is an atom but an invalid entity scope:
   ``domain_error(scope, Scope)``

Examples
--------

::

   | ?- imports_category(debugger, monitoring).

   | ?- imports_category(Object, monitoring, protected).

See also
--------

:ref:`predicates_current_category_1`,
:ref:`predicates_complements_object_2`
