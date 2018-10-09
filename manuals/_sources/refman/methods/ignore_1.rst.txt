
.. index:: ignore/1
.. _methods_ignore_1:

ignore/1
========

Description
-----------

::

   ignore(Goal)

This predicate succeeds whether its argument succeeds or fails and it is
not re-executable. This built-in meta-predicate is declared as a private
method and thus cannot be used as a message to an object.

This meta-predicate is opaque to cuts in its argument.

Template and modes
------------------

::

   ignore(+callable)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

Call a goal and succeeding even if it fails:
   ``ignore(Goal)``
To send a message succeeding even if it fails to :term:`self`:
   ``ignore(::Goal)``
To send a message succeeding even if it fails to an explicit object:
   ``ignore(Object::Goal)``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_once_1`,
   :ref:`methods_not_1`
