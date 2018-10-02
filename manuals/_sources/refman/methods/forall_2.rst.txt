
.. index:: forall/2
.. _methods_forall_2:

forall/2
========

Description
-----------

::

   forall(Generator, Test)

For all solutions of ``Generator``, ``Test`` is true. This built-in
meta-predicate is declared as a private method and thus cannot be used
as a message to an object.

Template and modes
------------------

::

   forall(+callable, +callable)

Errors
------

Either Generator or Test is a variable:
   ``instantiation_error``
Generator is neither a variable nor a callable term:
   ``type_error(callable, Generator)``
Test is neither a variable nor a callable term:
   ``type_error(callable, Test)``

Examples
--------

To call both goals in the context of the object or category containing the call:
   ``forall(Generator, Test)``
To send both goals as messages to :term:`self`:
   ``forall(::Generator, ::Test)``
To send both goals as messages to explicit objects:
   ``forall(Object1::Generator, Object2::Test)``

See also
--------

:ref:`methods_bagof_3`,
:ref:`methods_findall_3`,
:ref:`methods_findall_4`,
:ref:`methods_setof_3`
