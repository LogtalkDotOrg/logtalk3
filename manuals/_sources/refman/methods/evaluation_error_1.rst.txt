
.. index:: evaluation_error/1
.. _methods_evaluation_error_1:

evaluation_error/1
==================

Description
-----------

::

   evaluation_error(Exception)

Throws an ``error(evaluation_error(Exception), logtalk(Head,Context))``
exception term where ``Head`` is the head of the clause from where this
predicate is called and ``Context`` is the execution context of the
call. This built-in predicate is declared as a private method and thus
cannot be used as a message to an object.

Template and modes
------------------

::

   evaluation_error(+nonvar)

Errors
------

``(none)``

Examples
--------

``(none)``

.. seealso::

   :ref:`methods_catch_3`,
   :ref:`methods_throw_1`,
   :ref:`methods_context_1`,
   :ref:`methods_instantiation_error_0`,
   :ref:`methods_type_error_2`,
   :ref:`methods_domain_error_2`,
   :ref:`methods_existence_error_2`,
   :ref:`methods_permission_error_3`,
   :ref:`methods_representation_error_1`,
   :ref:`methods_resource_error_1`,
   :ref:`methods_syntax_error_1`,
   :ref:`methods_system_error_0`
