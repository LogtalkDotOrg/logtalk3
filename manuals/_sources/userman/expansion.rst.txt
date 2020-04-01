..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>

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

Logtalk supports a *term and goal expansion mechanism* that can be used to
define source-to-source transformations. Two common uses are the definition
of language extensions and domain-specific languages.

Logtalk improves upon the term-expansion mechanism found on some Prolog
systems by providing the user with fine-grained control on *if*, *when*,
and *how* expansions are applied. It allows declaring in a source file itself
which expansions, if any, will be used when compiling it. It allows declaring
which expansions will be used when compiling a file using compile and loading
predicate options. It defines a concept of *hook objects* that can be used
as building blocks to create custom and reusable *expansion workflows* with
explicit and well defined semantics. It prevents the simply act of loading
expansion rules affecting subsequent compilation of files. It prevents
conflicts between groups of expansion rules of different origins. It avoids
a set of buggy expansion rules from breaking other sets of expansions rules.


Defining expansions
-------------------

Term and goal expansions are defined using, respectively, the predicates
:ref:`methods_term_expansion_2` and :ref:`methods_goal_expansion_2`, which
are declared in the :ref:`expanding <apis:expanding/0>` built-in protocol.
Note that, unlike Prolog systems also providing these two predicates, they
are **not** declared as :term:`multifile predicates <multifile predicate>`
in the protocol. This design decision is key to give the programmer full
control of the expansion process and prevent the problems that inflict most
Prolog system providing a term-expansion mechanism.

An example of an object defining expansion rules:

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
and :ref:`methods_expand_goal_2` built-in methods or called automatically
by the compiler when compiling a source file (see the section below on *hook
objects*).

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

Clauses for the ``goal_expansion/2`` predicate are recursively called on the
expanded goal until a fixed point is reached. For example:

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

The goal-expansion mechanism prevents an infinite loop when expanding a goal
by checking that a goal to be expanded does not resulted from a previous
expansion of the same goal. For example, consider the following object:

::

   :- object(fixed_point,
       implements(expanding)).

       goal_expansion(a, b).
       goal_expansion(b, c).
       goal_expansion(c, (a -> b; c)).

   :- end_object.

The expansion of the goal ``a`` results in the goal ``(a -> b; c)`` with no
attempt to further expand the ``a``, ``b``, and ``c`` goals as they have
already been expanded.


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

Note that the default translation of grammar rules can be overridden by
defining clauses for the :ref:`methods_term_expansion_2` predicate.


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

This also applies to source file terms and source file goals when using hook
objects (discussed next).


Hook objects
------------

Term and goal expansion of a source file during its compilation is performed
by using *hook objects*. A hook object is simply an object implementing the
:ref:`expanding <apis:expanding/0>` built-in protocol and defining clauses
for the term and goal expansion hook predicates. Hook objects must be compiled
and loaded prior to be used to expand a source file.

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

Note that, due to the ``set_logtalk_flag/2`` directive being local to a source,
file, using it to specify a hook object will override any defined default hook
object or any hook object specified as a ``logtalk_compile/2`` or ``logtalk_load/2``
predicate compiler option for compiling or loading the source file.

.. note::

   Clauses for the ``term_expansion/2`` and ``goal_expansion/2`` predicates
   defined within an object or a category are never used in the compilation
   of the object or the category itself.

.. index:: single: begin_of_file
.. index:: single: end_of_file


Virtual source file terms and loading context
---------------------------------------------

When using a hook object to expand the terms of a source file, two
virtual file terms are generated: ``begin_of_file`` and ``end_of_file``.
These terms allow the user to define term-expansions before and after
the actual source file terms.

Logtalk also provides a :ref:`predicates_logtalk_load_context_2`
built-in predicate that can be used to access the compilation/loading
context when performing expansions. The :ref:`logtalk <objects_logtalk>`
built-in object also provides a set of predicates that can be useful,
notably when adding Logtalk support for languages extensions originally
developed for Prolog.

As an example of using the virtual terms and the ``logtalk_load_context/2``
predicate, assume that you want to convert plain Prolog files to Logtalk by
wrapping the Prolog code in each file using an object (named after the file)
that implements a given protocol. This could be accomplished by defining
the following hook object:

::

   :- object(wrapper(_Protocol_),
       implements(expanding)).

       term_expansion(begin_of_file, (:- object(Name,implements(_Protocol_)))) :-
           logtalk_load_context(file, File),
           os::decompose_file_name(File,_ , Name, _).

       term_expansion(end_of_file, (:- end_object)).

   :- end_object.

Assuming e.g. ``my_car.pl`` and ``lease_car.pl`` files  to be wrapped and a
``car_protocol`` protocol, we could then load them using:

.. code-block:: text

   | ?- logtalk_load(
            ['my_car.pl', 'lease_car.pl'],
            [hook(wrapper(car_protocol))]
        ).
   
   yes

.. note::

   When a source file also contains plain Prolog directives and predicates,
   these are term-expanded but not goal-expanded (with the exception of
   ``initialization/1`` directives, where the goal argument is expanded to
   improve code portability across backends).


Default compiler expansion workflow
-----------------------------------

When compiling a source file, the compiler will first try, by default,
the source file specific hook object specified using a local
``set_logtalk_flag/2`` directive, if defined. If that expansion fails,
it tries the hook object specified using the ``hook/1`` compiler option
in the ``logtalk_compile/2`` or ``logtalk_load/2`` goal that compiles
or loads the file, if defined. If that expansion fails, it tries the
default hook object, if defined. If that expansion also fails, the
compiler tries the Prolog dialect specific expansion rules found found
in the :term:`adapter file` (which are used to support non-standard
Prolog features).


User defined expansion workflows
--------------------------------

Sometimes we have multiple hook objects that we need to use in the compilation
of a source file. Logtalk includes a :doc:`../libraries/hook_flows` library
that supports two basic expansion workflows: a :ref:`pipeline <apis:hook_pipeline/1>`
of hook objects, where the expansion results from a hook object are feed to
the next hook object in the pipeline, and a :ref:`set <apis:hook_set/1>` of
hook objects, where expansions are tried until one of them succeeds. These
workflows are implemented as parametric objects allowing combining them to
implement more sophisticated expansion workflows. There is also a
:doc:`../libraries/hook_objects` library that provides convenient hook
objects for defining custom expansion workflows. This library includes an
hook object that can be used to restore the default expansion workflow used
by the compiler.

For example, assuming that you want to apply the Prolog backend specific
expansion rules defined in its adapter file, using the
:ref:`backend_adapter_hook <apis:backend_adapter_hook/0>` library object,
passing the resulting terms to your own expansion when compiling a source
file, we could use the goal:

.. code-block:: text

   | ?- logtalk_load(
            source,
            [hook(hook_pipeline([backend_adapter_hook, my_expansion]))]
        ).

As a second example, we can prevent expansion of a source file using the library
object :ref:`identity_hook <apis:identity_hook/0>` by adding as the first term in a
source file the directive:

::

   :- set_logtalk_flag(hook, identity_hook).

The file will be compiled as-is as any hook object (specified as a compiler
option or as a default hook object) and any backend adapter expansion rules
are overriden by the directive.


Using Prolog defined expansions
-------------------------------

In order to use clauses for the ``term_expansion/2`` and ``goal_expansion/2``
predicates defined in plain Prolog, simply specify the pseudo-object ``user``
as the hook object when compiling source files. When using
:term:`backend Prolog compilers <backend Prolog compiler>` that support a
module system, it can also be specified a module containing clauses for the
expanding predicates as long as the module name doesn't coincide with an
object name. When defining a custom workflow, the library object
:ref:`prolog_module_hook/1 <apis:prolog_module_hook/1>` can be used as a
workflow step. For example, assuming a module ``functions`` defining expansion
rules that we want to use:

.. code-block:: text

   | ?- logtalk_load(
            source,
            [hook(hook_set([prolog_module_hook(functions), my_expansion]))]
        ).

But note that Prolog module
libraries may provide definitions of the expansion predicates that are
not compatible with the Logtalk compiler. Specially when setting the
hook object to ``user``, be aware of any Prolog library that is loaded,
possibly by default or implicitly by the Prolog system, that may be
contributing definitions of the expansion predicates. It is usually
safer to define a specific hook object for combining multiple expansions
in a fully controlled way.

.. note::

   The ``user`` object declares ``term_expansion/2`` and ``goal_expansion/2``
   as multifile and dynamic predicates. This helps in avoiding predicate
   existence errors when compiling source files with the ``hook`` flag set
   to ``user`` as these predicates are only natively declared in some of the
   supported backend Prolog compilers.

Debugging expansions
--------------------

The ``term_expansion/2`` and ``goal_expansion/2`` predicates can be debugged
as any other object predicates. An alternative to the debugging tools is to
use a monitor for the runtime messages that call the predicates. For example,
assume a ``expansions_debug.lgt`` file with the contents:

::

   :- initialization(
   	define_events(after, edcg, _, _, expansions_debug)
   ).


   :- object(expansions_debug,
       implements(monitoring)).

       after(edcg, term_expansion(T,E), _) :-
           \+ \+ (
               numbervars(term_expansion(T,E), 0, _),
               writeq(term_expansion(T,E)), nl
           ).

   :- end_object.

We can use this monitor to help debug the expansion rules of the
:doc:`../libraries/edcg` library when applied to the ``edcgs`` example using
the queries:

.. code-block:: text

   | ?- {expansions_debug}.
   ...

   | ?- set_logtalk_flag(events, allow).
   yes

   | ?- {edcgs(loader)}.
   ...
   term_expansion(begin_of_file,[begin_of_file,(:-op(1200,xfx,-->>))])
   term_expansion((:-object(gemini)),[(:-object(gemini))])
   term_expansion(acc_info(castor,A,B,C,true),[])
   term_expansion(pass_info(pollux),[])
   term_expansion(pred_info(p,1,[castor,pollux]),[])
   term_expansion(pred_info(q,1,[castor,pollux]),[])
   term_expansion(pred_info(r,1,[castor,pollux]),[])
   term_expansion((p(A)-->>B is A+1,q(B),r(B)),(p(A,C,D,E):-B is A+1,q(B,C,F,E),r(B,F,D,E)))
   term_expansion((q(A)-->>[]),(q(A,B,B,C):-true))
   term_expansion((r(A)-->>[]),(r(A,B,B,C):-true))
   term_expansion(end_of_file,end_of_file)
   ...

This solution does not require (re)compiling the ``edcg`` hook object in
debug mode or modifying its expansion rules to emit debug messages. We
could also simply use the ``user`` pseudo-object as the monitor object:

.. code-block:: text

   | ?- assertz((
            after(_, term_expansion(T,E), _) :-
                \+ \+ (
                    numbervars(term_expansion(T,E), 0, _),
                    writeq(term_expansion(T,E)), nl
                )
        )).
   yes

   | ?- define_events(after, edcg, _, Sender, user).
   yes
