
.. index:: meta_non_terminal/1
.. _directives_meta_non_terminal_1:

meta_non_terminal/1
===================

Description
-----------

::

   meta_non_terminal(MetaNonTerminalTemplate)
   meta_non_terminal((MetaNonTerminalTemplate1, ...))
   meta_non_terminal([MetaNonTerminalTemplate1, ...])

   meta_non_terminal(Entity::MetaNonTerminalTemplate)
   meta_non_terminal((Entity1::MetaNonTerminalTemplate1, ...))
   meta_non_terminal([Entity1::MetaNonTerminalTemplate1, ...])

   meta_non_terminal(Module:MetaNonTerminalTemplate)
   meta_non_terminal((Module1:MetaNonTerminalTemplate1, ...))
   meta_non_terminal([Module1:MetaNonTerminalTemplate1, ...])

Declares meta-non-terminals, i.e., non-terminals that have arguments
that will be called as non-terminals (or grammar rule bodies). An
argument may also be a *closure* instead of a goal if the non-terminal
uses the :ref:`methods_call_1` Logtalk built-in
methods to construct and call the actual non-terminal from the closure
and the additional arguments.

Meta-arguments which are non-terminals are represented by the integer
``0``. Meta-arguments which are closures are represented by a positive
integer, ``N``, representing the number of additional arguments that
will be appended to the closure in order to construct the corresponding
meta-call. Normal arguments are represented by the atom ``*``.
Meta-arguments are always called in the meta-non-terminal calling
context, not in the meta-non-terminal definition context.

Template and modes
------------------

::

   meta_non_terminal(+meta_non_terminal_template_term)

   meta_non_terminal(+object_identifier::+meta_non_terminal_template_term)
   meta_non_terminal(+category_identifier::+meta_non_terminal_template_term)

   meta_non_terminal(+module_identifier:+meta_non_terminal_template_term)

Examples
--------

::

   :- meta_non_terminal(findall(*, 0, *)).

   :- meta_non_terminal(forall(0, 0)).

   :- meta_non_terminal(maplist(2, *, *)).

.. seealso::

   :ref:`directives_meta_predicate_1`,
   :ref:`methods_predicate_property_2`
