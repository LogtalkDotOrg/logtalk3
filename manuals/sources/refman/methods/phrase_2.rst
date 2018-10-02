
.. index:: phrase/2
.. _methods_phrase_2:

phrase/2
========

Description
-----------

::

   phrase(GrammarRuleBody, Input)
   phrase(::GrammarRuleBody, Input)
   phrase(Object::GrammarRuleBody, Input)

True when the ``GrammarRuleBody`` grammar rule body can be applied to
the ``Input`` list of tokens. In the most common case,
``GrammarRuleBody`` is a non-terminal defined by a grammar rule. This
built-in method is declared private and thus cannot be used as a message
to an object. When using a back-end Prolog compiler supporting a module
system, calls in the format ``phrase(Module:GrammarRuleBody, Input)``
may also be used.

This method is opaque to cuts in the first argument. When the first
argument is sufficiently instantiated at compile time, the method call
is compiled in order to eliminate the implicit overheads of converting
the grammar rule body into a goal and meta-calling it. For performance
reasons, the second argument is only type-checked at compile time.

Template and modes
------------------

::

   phrase(+callable, ?list)

Errors
------

NonTerminal is a variable:
   ``instantiation_error``
NonTerminal is neither a variable nor a callable term:
   ``type_error(callable, NonTerminal)``

Examples
--------

To parse a list of tokens using a local non-terminal:
   ``phrase(NonTerminal, Input)``
To parse a list of tokens using a non-terminal within the scope of :term:`self`:
   ``phrase(::NonTerminal, Input)``
To parse a list of tokens using a public non-terminal of an explicit object:
   ``phrase(Object::NonTerminal, Input)``

See also
--------

:ref:`methods_call_1`,
:ref:`methods_phrase_1`,
:ref:`methods_phrase_3`
