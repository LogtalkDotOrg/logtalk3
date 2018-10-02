
.. index:: phrase//1
.. _methods_phrase_1:

phrase//1
=========

Description
-----------

::

   phrase(NonTerminal)

This non-terminal takes a non-terminal or a grammar rule body and parses
it using the current implicit list of tokens. A common use is to wrap
what otherwise would be a naked variable in a grammar rule body.

Template and modes
------------------

::

   phrase(+callable)

Errors
------

NonTerminal is a variable:
   ``instantiation_error``
NonTerminal is neither a variable nor a callable term:
   ``type_error(callable, NonTerminal)``

Examples
--------

   ``(none)``

See also
--------

:ref:`methods_call_1`,
:ref:`methods_phrase_2`,
:ref:`methods_phrase_3`
