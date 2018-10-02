
.. index:: permission_error/3
.. _methods_permission_error_3:

permission_error/3
==================

Description
-----------

::

   permission_error(Operation, Permission, Culprit)

Throws an
``error(permission_error(Operation,Permission,Culprit), logtalk(Head,Context))``
exception term where ``Head`` is the head of the clause from where this
predicate is called and ``Context`` is the execution context of the
call. This built-in predicate is declared as a private method and thus
cannot be used as a message to an object.

Template and modes
------------------

::

   permission_error(+nonvar)

Errors
------

``(none)``

Examples
--------

``(none)``

See also
--------

| ``catch/3``, ``throw/1``, ``context/1``
| ``instantiation_error/0``, ``type_error/2``, ``domain_error/2``,
  ``existence_error/2``, ``representation_error/1``,
  ``evaluation_error/1``, ``resource_error/1``, ``syntax_error/1``,
  ``system_error/0``
