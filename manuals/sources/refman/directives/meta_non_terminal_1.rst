..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. index:: pair: meta_non_terminal/1; Directive
.. _directives_meta_non_terminal_1:

meta_non_terminal/1
===================

Description
-----------

::

   meta_non_terminal(Template)
   meta_non_terminal((Template, ...))
   meta_non_terminal([Template, ...])

   meta_non_terminal(Entity::Template)
   meta_non_terminal((Entity::Template, ...))
   meta_non_terminal([Entity::Template, ...])

   meta_non_terminal(Module:Template)
   meta_non_terminal((Module:Template, ...))
   meta_non_terminal([Module:Template, ...])

Declares meta-non-terminals, i.e., non-terminals that have arguments
that will be called as non-terminals (or grammar rule bodies). An
argument may also be a :term:`closure` instead of a goal if the
non-terminal uses the :ref:`methods_call_1` Logtalk built-in
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

   :- meta_non_terminal(phrase(1, *)).
   phrase(X, T) --> call(X, T).

.. seealso::

   :ref:`directives_meta_predicate_1`,
   :ref:`methods_predicate_property_2`
