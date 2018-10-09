
.. index:: \+/1
.. _methods_not_1:

\\+/1
=====

Description
-----------

::

   \+ Goal

Not-provable meta-predicate. True iff ``call(Goal)`` is false. This
built-in meta-predicate is declared as a private method and thus cannot
be used as a message to an object.

Template and modes
------------------

::

   \+ +callable

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

Not-provable goal in the context of the object or category containing the call:
   ``\+ Goal``
Not-provable goal sent as a message to :term:`self`:
   ``\+ ::Goal``
Not-provable goal sent as a message to an explicit object:
   ``\+ Object::Goal``

.. seealso::

   :ref:`methods_call_N`,
   :ref:`methods_ignore_1`,
   :ref:`methods_once_1`
