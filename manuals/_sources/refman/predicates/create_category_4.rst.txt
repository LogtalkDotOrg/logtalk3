
.. index:: create_category/4
.. _predicates_create_category_4:

create_category/4
=================

Description
-----------

::

   create_category(Identifier, Relations, Directives, Clauses)

Creates a new, dynamic category. This predicate is often used as a
primitive to implement high-level category creation methods.

Note that, when opting for runtime generated category identifiers, it's
possible to run out of identifiers when using a back-end Prolog compiler
with bounded integer support. The portable solution, when creating a
large number of dynamic category in long running applications, is to
recycle, whenever possible, the identifiers.

When using Logtalk multi-threading features, predicates calling this
built-in predicate may need to be declared synchronized in order to
avoid race conditions.

Template and modes
------------------

::

   create_category(?category_identifier, @list(category_relation), @list(category_directive), @list(clause))

Errors
------

Relations, Directives, or Clauses is a variable:
   ``instantiation_error``
Identifier is neither a variable nor a valid category identifier:
   ``type_error(category_identifier, Identifier)``
Identifier is already in use:
   ``permission_error(modify, category, Identifier)``
   ``permission_error(modify, object, Identifier)``
   ``permission_error(modify, protocol, Identifier)``
Relations is neither a variable nor a proper list:
   ``type_error(list, Relations)``
Repeated entity relation clause:
   ``permission_error(repeat, entity_relation, implements/1)``
   ``permission_error(repeat, entity_relation, extends/1)``
   ``permission_error(repeat, entity_relation, complements/1)``
Directives is neither a variable nor a proper list:
   ``type_error(list, Directives)``
Clauses is neither a variable nor a proper list:
   ``type_error(list, Clauses)``

Examples
--------

::

   | ?- create_category(
           tolerances,
           [implements(comparing)],
           [],
           [epsilon(1e-15), (equal(X, Y) :- epsilon(E), abs(X-Y) =< E)]
        ).

See also
--------

:ref:`predicates_abolish_category_1`, :ref:`predicates_category_property_2`,
:ref:`predicates_current_category_1`,
:ref:`predicates_complements_object_2`, :ref:`predicates_extends_category_2_3`,
:ref:`predicates_imports_category_2_3`
