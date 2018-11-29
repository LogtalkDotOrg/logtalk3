..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.


.. _expansion_expansion:

Term and goal expansion
=======================

Logtalk supports an :ref:`methods_expand_term_2` built-in
method for expanding a term into another term or a list of terms. This
method is mainly used to translate grammar rules into Prolog clauses. It
can be customized, e.g. for bypassing the default Logtalk grammar rule
translator, by defining clauses for the :ref:`methods_term_expansion_2` hook
predicate. Logtalk also supports an :ref:`methods_expand_goal_2` built-in
method for expanding a goal. This method can also be customized by
defining clauses for the :ref:`methods_goal_expansion_2` hook
predicate.

Term and goal expansion may be performed either by calling the
``expand_term/2`` and ``expand_goal/2`` built-in methods explicitly or
by using *hook objects*. A hook object is simply an object defining
clauses for the term- and goal-expansion hook predicates. To compile a
source file using a hook object for expanding its terms and goals, you
can use the :ref:`hook <flag_hook>` compiler
flag in the second argument of the :ref:`predicates_logtalk_compile_2`
or :ref:`predicates_logtalk_load_2` built-in predicates. In alternative,
you can use a :ref:`directives_set_logtalk_flag_2`
directive in the source file itself. When compiling a source file, the
compiler will first try the source file specific hook object, if
defined. If that fails, it tries the default hook object, if defined. If
that also fails, the compiler tries the Prolog dialect specific
expansion predicate definitions if defined in the adapter file.

.. index:: single: begin_of_file
.. index:: single: end_of_file

When using an hook object to expand the terms of a source file, two
virtual terms are generated: ``begin_of_file`` and ``end_of_file``.
These terms allow the user to define term-expansions before and after
the actual source file terms.

Sometimes we have multiple hook objects that we need to use in the
compilation of a source file. The Logtalk library includes support for
two basic expansion workflows: a pipeline of hook objects, where the
expansion results from a hook object are feed to the next hook object
in the pipeline, and a set of hook objects, where expansions are tried
until one of is found that succeeds. These workflows are implemented as
parametric objects allowing combining them to implement more
sophisticated expansion workflows.

Clauses for the ``term_expansion/2`` and ``goal_expansion/2`` predicates
defined within an object or a category are never used in the compilation
of the object or the category itself, however. In order to use clauses
for the ``term_expansion/2`` and ``goal_expansion/2`` predicates defined
in plain Prolog, simply specify the pseudo-object ``user`` as the hook
object when compiling source files. When using backend Prolog compilers
that support a module system, it can also be specified a module
containing clauses for the expanding predicates as long as the module
name doesn't coincide with an object name. But note that Prolog module
libraries may provide definitions of the expansion predicates that are
not compatible with the Logtalk compiler. Specially when setting the
hook object to ``user``, be aware of any Prolog library that is loaded,
possibly by default or implicitly by the Prolog system, that may be
contributing definitions of the expansion predicates. It is usually much
safer to define a specific hook object for combining multiple expansions
in a fully controlled way.

Logtalk provides a :ref:`predicates_logtalk_load_context_2`
built-in predicate that can be used to access the compilation/loading
context when performing term-expansion or goal-expansion.
