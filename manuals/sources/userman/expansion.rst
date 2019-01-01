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


.. _expansion_expansion:

Term and goal expansion
=======================

Logtalk supports the *term and goal expansion mechanism* also found in some
Prolog systems. This macro mechanism is used to define source-to-source
transformations. Two common uses are the definition of language extensions
and domain-specific languages.

Defining expansions
-------------------

Term and goal expansions are defined using, respectively, the predicates
:ref:`methods_term_expansion_2` and :ref:`methods_goal_expansion_2`, which
are declared in the :ref:`expanding <apis:expanding/0>` built-in protocol.
For example:

::

   :- object(an_object,
       implements(expanding)).

       term_expansion(ping, pong).
       term_expansion(
           colors,
           [white, yellow, blue, green, read, black]
       ).

       goal_expansion(a, b).
       goal_expansion(b, c).
       goal_expansion(X is Expression, true) :-
           catch(X is Expression, _, fail).

   :- end_object.

These predicates can be explicitly called using the :ref:`methods_expand_term_2`
and :ref:`methods_expand_goal_2` built-in methods.

Clauses for the ``term_expansion/2`` predicate are called until of them
succeeds. The returned expansion can be a single term or a list of terms. 
For example:

.. code-block:: text

   | ?- an_object::expand_term(ping, Term).
   
   Term = pong
   yes
   
   | ?- an_object::expand_term(colors, Colors).
   
   Colors = [white, yellow, blue, green, read, black]
   yes

When no ``term_expansion/2`` clause applies, the same term that we are
trying to expand is returned:

.. code-block:: text

   | ?- an_object::expand_term(sounds, Sounds).
   
   Sounds = sounds
   yes

Clauses for the ``goal_expansion/2`` predicate are recursively called on
the expanded goal until a fixed point is reached. Care must be taken to
avoid compilation loops. For example:

.. code-block:: text
   
   | ?- an_object::expand_goal(a, Goal).
   
   Goal = c
   yes

   | ?- an_object::expand_goal(X is 3+2*5, Goal).
   
   X = 13,
   Goal = true
   yes

When no ``goal_expansion/2`` clause applies, the same goal that we are
trying to expand is returned:

.. code-block:: text
   
   | ?- an_object::expand_goal(3 =:= 5, Goal).
   
   Goal = (3=:=5)
   yes

Term and goal expansion predicates can also be used when compiling a source
file as described below.

Expanding grammar rules
-----------------------

A common term expansion is the translation of grammar rules into predicate
clauses. This transformation is performed automatically by the compiler
when a source file entity defines grammar rules. It can also be done
explicitly by calling the ``expand_term/2`` built-in method. For example: 

.. code-block:: text

   | ?- logtalk::expand_term((a --> b, c), Clause).

   Clause = (a(A,B) :- b(A,C), c(C,B))
   yes

Note that the default translation of grammar rules can be overriden by
defining clauses for the :ref:`methods_term_expansion_2` predicate.

Hook objects
------------

Term and goal expansion of a source file during its compilation is performed
by using *hook objects*. A hook object is simply an object implementing the
:ref:`expanding <apis:expanding/0>` built-in protocol, defining clauses for
the term and goal expansion hook predicates.

To compile a source file using a hook object, we can use the
:ref:`hook <flag_hook>` compiler flag in the second argument of the
:ref:`predicates_logtalk_compile_2` and :ref:`predicates_logtalk_load_2`
built-in predicates. For example:

.. code-block:: text

   | ?- logtalk_load(source_file, [hook(hook_object)]).
   ...

In alternative, we can use a :ref:`directives_set_logtalk_flag_2`
directive in the source file itself. For example:

::

   :- set_logtalk_flag(hook, hook_object).

It is also possible to define a default hook object by defining a global
value for the ``hook`` flag by calling the :ref:`predicates_set_logtalk_flag_2`
predicate. For example:

.. code-block:: text

   | ?- set_logtalk_flag(hook, hook_object).
   
   yes

When compiling a source file, the compiler will first try the source file
specific hook object, if defined. If that fails, it tries the default hook
object, if defined. If that also fails, the compiler tries the Prolog dialect
specific expansion predicate definitions if defined in the adapter file.

.. note::

   Clauses for the ``term_expansion/2`` and ``goal_expansion/2`` predicates
   defined within an object or a category are never used in the compilation
   of the object or the category itself.

.. index:: single: begin_of_file
.. index:: single: end_of_file

When using an hook object to expand the terms of a source file, two
virtual terms are generated: ``begin_of_file`` and ``end_of_file``.
These terms allow the user to define term-expansions before and after
the actual source file terms.

Logtalk provides a :ref:`predicates_logtalk_load_context_2`
built-in predicate that can be used to access the compilation/loading
context when performing expansions. The :ref:`logtalk <objects_logtalk>`
built-in object also provides a set of predicates that can be useful,
notably when adding Logtalk support for languages extensions originally
developed for Prolog.

Bypassing expansions
--------------------

Terms and goals wrapped by the :ref:`control_external_call_1` control
construct are not expanded. For example:

.. code-block:: text

   | ?- an_object::expand_term({ping}, Term).
   
   Term = {ping}
   yes
   
   | ?- an_object::expand_goal({a}, Goal).
   
   Goal = {a}
   yes

This also applies to source file terms and source file goals.

Combining multiple expansions
-----------------------------

Sometimes we have multiple hook objects that we need to use in the compilation
of a source file. The Logtalk library includes support for two basic expansion
workflows: a :ref:`pipeline <apis:hook_pipeline/1>` of hook objects, where the
expansion results from a hook object are feed to the next hook object in the
pipeline, and a :ref:`set <apis:hook_set/1>` of hook objects, where expansions
are tried until one of them succeeds. These workflows are implemented as
parametric objects allowing combining them to implement more sophisticated
expansion workflows.

Using Prolog defined expansions
-------------------------------

In order to use clauses for the ``term_expansion/2`` and ``goal_expansion/2``
predicates defined in plain Prolog, simply specify the pseudo-object ``user``
as the hook object when compiling source files. When using backend Prolog
compilers that support a module system, it can also be specified a module
containing clauses for the expanding predicates as long as the module
name doesn't coincide with an object name. But note that Prolog module
libraries may provide definitions of the expansion predicates that are
not compatible with the Logtalk compiler. Specially when setting the
hook object to ``user``, be aware of any Prolog library that is loaded,
possibly by default or implicitly by the Prolog system, that may be
contributing definitions of the expansion predicates. It is usually
safer to define a specific hook object for combining multiple expansions
in a fully controlled way.
