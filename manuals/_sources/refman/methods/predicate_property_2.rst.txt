
.. index:: predicate_property/2
.. _methods_predicate_property_2:

predicate_property/2
====================

Description
-----------

::

   predicate_property(Predicate, Property)

Enumerates, by backtracking, the properties of a visible predicate. When
the predicate indicator for ``Predicate`` is declared in a ``uses/2`` or
``use_module/2`` directive, properties are enumerated for the referenced
object or module predicate. Otherwise properties are enumerated for an
object predicate. In the case of objects, properties for predicates not
declared using a scope directive are not enumerated. The valid predicate
properties are listed in the language :ref:`grammar <grammar_predicate_properties>`.

Template and modes
------------------

::

   predicate_property(+callable, ?predicate_property)

Errors
------

Predicate is a variable:
   ``instantiation_error``
Predicate is neither a variable nor a callable term:
   ``type_error(callable, Predicate)``
Property is neither a variable nor a valid predicate property:
   ``domain_error(predicate_property, Property)``

Examples
--------

To enumerate, by backtracking, the properties of a locally visible user predicate or a user predicate visible in :term:`this`:
   ``predicate_property(foo(_), Property)``
To enumerate, by backtracking, the properties of a public or protected predicate visible in :term:`self`:
   ``::predicate_property(foo(_), Property)``
To enumerate, by backtracking, the properties of a public predicate visible in an explicit object:
   ``Object::predicate_property(foo(_), Property)``

.. seealso::

   :ref:`methods_current_op_3`,
   :ref:`methods_current_predicate_1`,
   :ref:`directives_uses_2`,
   :ref:`directives_use_module_2`
