``hook_objects``
================

This library provides a set of convenient hook objects for defining
custom expansion workflows (using e.g. the ``hook_flows`` library) and
for debugging. They are usable and useful as-is but should also be
regarded as term- and goal-expansion examples that you can learn from,
clone, and change to fit your application requirements.

API documentation
-----------------

Open the
`../../docs/library_index.html#hook-objects <../../docs/library_index.html#hook-objects>`__
link in a web browser.

Loading
-------

To load all hook objects in this library, load the ``loader.lgt`` file:

::

   | ?- logtalk_load(hook_objects(loader)).

To load a specific hook object, e.g. the ``backend_adapter_hook``
object:

::

   | ?- logtalk_load(hook_objects(backend_adapter_hook)).

Testing
-------

To test this library hook objects, load the ``tester.lgt`` file:

::

   | ?- logtalk_load(hook_objects(tester)).

Usage
-----

The provided hook objects cover different expansion scenarios as
follows.

Using the Prolog backend adapter file expansion rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Useful when defining a custom expansion workflow. This can be
accomplished by loading the ``backend_adapter_hook.lgt`` file, which
defines a ``backend_adapter_hook`` hook object that can be used as a
workflow step.

Restoring the default compiler expansion workflow
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this case, load the ``default_workflow_hook.lgt`` file, which defines
a ``default_workflow_hook`` hook object, and use the following **goal**
to set the default hook object:

::

   | ?- set_logtalk_flag(hook, default_workflow_hook).

Preventing applying any (other) user-defined expansion rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When compiling a source file, we sometimes want to prevent applying
expansion rules. This can be accomplished by simply loading the
``identity_hook.lgt`` file, which defines the ``identity_hook`` hook
object, whose expansion rules simply succeed without changing the terms
and goals, and setting it as the file specific hook object writing as
the first term in the file the **directive**:

::

   :- set_logtalk_flag(hook, identity_hook).

Note that the compiler will always convert any grammar rules defined in
the file into clauses. Although this conversion can also be performed as
an expansion, grammar rules are part of the Logtalk language. If you to
preserve the grammar rules, use the hook objects described below to
write them to a stream.

Expanding grammar rules into clauses independently of the compiler
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load the ``grammar_rules_hook.lgt`` and use the term-expansion rules in
the ``grammar_rules_hook`` object. For example:

::

   | ?- grammar_rules_hook::term_expansion((a --> [b],c), Clause).

   Clause = (a([b|T], C) :- c(T, C))
   yes

Using the expansion rules defined in a Prolog module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load the ``prolog_module_hook.lgt``, which defines the parametric hook
object ``prolog_module_hook(Module)``. To use this hook object, you need
to instantiate the parameter to the name of the module. For example:

::

   :- set_logtalk_flag(hook, prolog_module_hook(user)).

Wrap the contents of a plain Prolog file as an object
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load the ``object_wrapper_hook.lgt``, which defines the
``object_wrapper_hook/0-2`` hook objects. Use them to wrap the contents
of a plain Prolog file as an object named after the file (optionally
implementing a protocol) or an object with the given name and object
relations. Can be used to apply Logtalk developer tools to plain Prolog
code or when porting Prolog application to Logtalk. For example:

::

   | ?- logtalk_load('plain.pl', [hook(object_wrapper_hook)]).
   ...

   | ?- current_object(plain).
   yes

Or:

::

   | ?- logtalk_load('world_1.pl', [hook(object_wrapper_hook(some_protocol))]).
   ...

   | ?- current_object(world_1).
   yes

   | ?- implements_protocol(world_1, Protocol).
   Protocol = some_protocol
   yes

Or:

::

   | ?- logtalk_load('foo.pl', [hook(object_wrapper_hook(bar,[imports(some_category))]).
   ...

   | ?- current_object(bar).
   yes

   | ?- imports_category(bar, Category).
   Category = some_category
   yes

The ``object_wrapper_hook`` object sets the ``context_switching_calls``
flag to ``allow`` for the generated object. This enables calling the
predicates using the ``<</2`` context-switching control construct. But
it's usually better to define a protocol for the predicates being
encapsulated and use instead the ``object_wrapper_hook/1-2`` objects.

Outputting term-expansion results to a stream
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Load the ``write_to_stream_hook.lgt`` file and using the
``write_to_stream_hook(Stream)`` or
``write_to_stream_hook(Stream, Options)`` hook object. The terms are not
modified and thus these hook objects may be used at any point in an
expansion workflow.

Printing entity predicate goals before or after calling them
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is helpful for quick debugging. Load the ``print_goal_hook.lgt``
file and use the ``print_goal_hook`` hook object. For example, we can
set this hook object as the default hook:

::

   | ?- set_logtalk_flag(hook, print_goal_hook).

Then, edit the entity source code to print selected goals:

::

   foo :-
       - bar,   % print goal before calling it
       + baz,   % print goal after calling it
       * quux.  % print goal before and after calling it

Suppressing goals
~~~~~~~~~~~~~~~~~

The ``suppress_goal_hook.lgt`` file provides the ``suppress_goal_hook``
hook object that supports suppressing a goal in a clause body by
prefixing it using the ``--`` operator. We can set this hook object as
the default hook using the goal:

::

   | ?- set_logtalk_flag(hook, suppress_goal_hook).

If the expansion is only to be used in a single file, use instead the
source file directive:

::

   :- set_logtalk_flag(hook, suppress_goal_hook).

Then, edit entity predicates to suppress goals. For example:

::

   foo :-
       bar,
       -- baz,
       quux.

The suppressed goals are replaced by calls to ``true/0``.
