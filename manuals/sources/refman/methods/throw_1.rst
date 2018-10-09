
.. index:: throw/1
.. _methods_throw_1:

throw/1
=======

Description
-----------

::

   throw(Exception)

Throws an exception. This built-in predicate is declared as a private
method and thus cannot be used as a message to an object.

Template and modes
------------------

::

   throw(+nonvar)

Errors
------

Exception is a variable:
   ``instantiation_error``
Exception does not unify with the second argument of any call of :ref:`catch/3 <methods_catch_3>`:
   ``system_error``

Examples
--------

``(none)``

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_evaluation_error_1`,
   :ref:`methods_representation_error_1`
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
