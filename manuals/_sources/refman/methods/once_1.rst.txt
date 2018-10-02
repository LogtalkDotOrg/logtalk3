
.. index:: once/1
.. _methods_once_1:

once/1
======

Description
-----------

::

   once(Goal)

This predicate behaves as ``call(Goal)`` but it is not re-executable.
This built-in meta-predicate is declared as a private method and thus
cannot be used as a message to an object.

This meta-predicate is opaque to cuts in its argument.

Template and modes
------------------

::

   once(+callable)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

Call a goal deterministically in the context of the object or category containing the call:
   ``once(Goal)``
To send a goal as a non-backtracable message to :term:`self`:
   ``once(::Goal)``
To send a goal as a non-backtracable message to an explicit object:
   ``once(Object::Goal)``

See also
--------

:ref:`methods_call_N`,
:ref:`methods_ignore_1`,
:ref:`methods_not_1`
