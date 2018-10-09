
.. index:: catch/3
.. _methods_catch_3:

catch/3
=======

Description
-----------

::

   catch(Goal, Catcher, Recovery)

Catches exceptions thrown by a goal. See the ISO Prolog standard
definition. This built-in meta-predicate is declared as a private method
and thus cannot be used as a message to an object.

Template and modes
------------------

::

   catch(?callable, ?term, ?term)

Errors
------

Goal is a variable:
   ``instantiation_error``
Goal is neither a variable nor a callable term:
   ``type_error(callable, Goal)``

Examples
--------

``(none)``

.. seealso::

   :ref:`methods_throw_1`
