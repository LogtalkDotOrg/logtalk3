
.. index:: initialization/1
.. _directives_initialization_1:

initialization/1
================

Description
-----------

::

   initialization(Goal)

When used within an object, this directive defines a goal to be called
after the object has been loaded into memory. When used at a global
level within a source file, this directive defines a goal to be called
after the compiled source file is loaded into memory.

Multiple initialization directives can be used in a source file or in an
object. Their goals will be called in order at loading time.

Categories and protocols cannot contain initialization directives as the
initialization goals would lack a complete execution context which is
only available for objects.

Although technically a global ``initialization/1`` directive in a source
file is a Prolog directive, calls to Logtalk built-in predicates from it
are usually compiled to improve performance and providing better support
for embedded applications.

Template and modes
------------------

::

   initialization(@callable)

Examples
--------

::

   :- initialization(init).
