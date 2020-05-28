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


.. _predicates_predicates:

Predicates
==========

Predicate directives and clauses can be encapsulated inside objects and
categories. Protocols can only contain predicate directives. From the
point-of-view of a traditional imperative object-oriented language,
predicates allows both object state and object behavior to be represented.
Mutable object state can be represented using dynamic object predicates
but should only be used when strictly necessary as it breaks declarative
semantics.

.. _predicates_reserved:

Reserved predicate names
------------------------

For practical and performance reasons, some predicate names have a fixed
interpretation. These predicates are declared in the built-protocols.
They are: :ref:`methods_goal_expansion_2` and :ref:`methods_term_expansion_2`,
declared in the :ref:`expanding <apis:expanding/0>` protocol;
:ref:`methods_before_3` and :ref:`methods_after_3`, declared in the
:ref:`monitoring <apis:monitoring/0>` protocol; and
:ref:`methods_forward_1`, declared in the
:ref:`forwarding <apis:forwarding/0>` protocol.
By default, the compiler prints a warning when
a definition for one of these predicates is found but the reference to
the corresponding built-in protocol is missing.

.. _predicates_declaring:

Declaring predicates
--------------------

Logtalk provides a clear distinction between *declaring a predicate* and
*defining a predicate* and thus clear :term:`closed-world assumption` semantics.
Messages or calls for declared but undefined predicates fail. Messages or
calls for unknown (i.e. non declared) predicates throw an error. Note that
this is a fundamental requirement for supporting :ref:`protocols <protocols_protocols>`:
we must be able to declare a predicate without necessarily defining it.

All object (or category) predicates that we want to access from other
objects (or categories) must be explicitly declared. A predicate
declaration must contain, at least, a *scope* directive. Other
directives may be used to document the predicate or to ensure proper
compilation of the predicate clauses.

.. _predicates_scope:

Scope directives
~~~~~~~~~~~~~~~~

A predicate scope directive specifies *from where* the predicate can be
called, i.e. its *visibility*. Predicates can be *public*, *protected*,
*private*, or *local*. Public predicates can be called from any object.
Protected predicates can only be called from the container object or
from a container descendant. Private predicates can only be called from
the container object. Predicates are local when they are not declared in
a scope directive. Local predicates, like private predicates, can only be
called from the container object (or category) but they are *invisible*
to the reflection built-in methods (:ref:`methods_current_predicate_1`
and :ref:`methods_predicate_property_2`) and to the message error handling
mechanisms (i.e. sending a message corresponding to a local predicate
results in a ``predicate_declaration`` existence error instead of a scope
error).

The scope declarations are made using the directives
:ref:`directives_public_1`, :ref:`directives_protected_1`, and
:ref:`directives_private_1`. For example:

::

   :- public(init/1).

   :- protected(valid_init_option/1).

   :- private(process_init_options/1).

If a predicate does not have a (local or inherited) scope declaration,
it is assumed that the predicate is local. Note that we do not need to
write scope declarations for all defined predicates. One exception is
local dynamic predicates: declaring them as private predicates may allow
the Logtalk compiler to generate optimized code for asserting and
retracting clauses.

Note that a predicate scope directive doesn't specify *where* a
predicate is, or can be, defined. For example, a private predicate can
only be called from an object holding its scope directive. But it can be
defined in descendant objects. A typical example is an object playing
the role of a class defining a private (possibly dynamic) predicate for
its descendant instances. Only the class can call (and possibly
assert/retract clauses for) the predicate but its clauses can be
found/defined in the instances themselves.

Scope directives may also be used to declare grammar rule non-terminals
and operators. For example:

::

   :- public(url//1).

   :- public(op(800, fx, tag)).

.. _predicates_mode:

Mode directive
~~~~~~~~~~~~~~

Often predicates can only be called using specific argument patterns.
The valid arguments and instantiation modes of those arguments can be
documented by using the :ref:`directives_mode_2` directive. For
example:

::

   :- mode(member(?term, ?list), zero_or_more).

The first directive argument describes a valid calling mode. The minimum
information will be the instantiation mode of each argument. The first
four possible values are described in [ISO95]_). The remaining two can
also be found in use in some Prolog systems.

``+``
   Argument must be instantiated (but not necessarily ground).
``-``
   Argument should be a free (non-instantiated) variable (when bound,
   the call will unify the returned term with the given term).
``?``
   Argument can either be instantiated or free.
``@``
   Argument will not be further instantiated (modified).
``++``
   Argument must be ground.
``--``
   Argument must be unbound. Used mainly when returning an opaque term.

These six mode atoms are also declared as prefix operators by the
Logtalk compiler. This makes it possible to include type information
for each argument like in the example above. Some possible type
values are: ``event``, ``object``, ``category``, ``protocol``,
``callable``, ``term``, ``nonvar``, ``var``, ``atomic``, ``atom``,
``number``, ``integer``, ``float``, ``compound``, and ``list``. The
first four are Logtalk specific. The remaining are common Prolog types.
We can also use our own types that can be either atoms or ground
compound terms.

The second directive argument documents the number of proofs, but not
necessarily distinct solutions, for the specified mode. As an example,
the ``member(X, [1,1,1,1])`` goal have only one distinct solution but four
proofs for that solution. Note that different modes for the same predicate
often have different determinism. The possible values are:

``zero``
   Predicate always fails.
``one``
   Predicate always succeeds once.
``zero_or_one``
   Predicate either fails or succeeds.
``zero_or_more``
   Predicate has zero or more proofs.
``one_or_more``
   Predicate has one or more proofs.
``one_or_error``
   Predicate either succeeds once or throws an error (see below).
``error``
   Predicate will throw an error.

Mode declarations can also be used to document that some call modes will
throw an error. For instance, regarding the ``arg/3`` and ``open/3`` ISO
Prolog built-in predicates, we may write:

::

   :- mode(arg(-, -, +), error).
   :- mode(open(@, @, --), one_or_error).

Note that most predicates have more than one valid mode implying several
mode directives. For example, to document the possible use modes of the
``atom_concat/3`` ISO built-in predicate we would write:

::

   :- mode(atom_concat(?atom, ?atom, +atom), one_or_more).
   :- mode(atom_concat(+atom, +atom, -atom), zero_or_one).

Some old Prolog compilers supported some sort of mode directives to
improve performance. To the best of my knowledge, there is no modern
Prolog compiler supporting this kind of directive for that purpose.
The current Logtalk version simply parses this directive for collecting
its information for use in the :ref:`reflection API <reflection_reflection>`
(assuming the :ref:`source_data <flag_source_data>` flag is turned on).
In any case, the use of mode directives is a good starting point for
documenting your predicates.

.. _predicates_meta:

Meta-predicate directive
~~~~~~~~~~~~~~~~~~~~~~~~

Some predicates may have arguments that will be called as goals or interpreted
as :term:`closures <closure>` that will be used for constructing goals. To
ensure that these goals will be executed in the correct context (i.e. in the
*calling context*, not in the meta-predicate *definition context*) we need to
use the :ref:`directives_meta_predicate_1` directive. For example:

::

   :- meta_predicate(findall(*, 0, *)).
   :- meta_predicate(map(2, *, *)).

The meta-predicate mode arguments in this directive have the following
meaning:

``0``
   Meta-argument that will be called as a goal.
``N``
   Meta-argument that will be a closure used to construct a call by
   extending it with ``N`` arguments. The value of ``N`` must be a
   positive integer.
``::``
   Argument that is context-aware but that will not be called as a goal
   or a closure. It can contain, however, sub-terms that will be called
   as goals or closures.
``^``
   Goal that may be existentially quantified (``Vars^Goal``).
``*``
   Normal argument.

The following meta-predicate mode arguments are for use only when writing
backend Prolog :term:`adapter files <adapter file>` to deal with proprietary
built-in meta-predicates and meta-directives:

``/``
   Predicate indicator (``Name/Arity``), list of predicate indicators,
   or conjunction of predicate indicators.
``//``
   Non-terminal indicator (``Name//Arity``), list of predicate
   indicators, or conjunction of predicate indicators.
``[0]``
   List of goals.
``[N]``
   List of closures.
``[/]``
   List of predicate indicators.
``[//]``
   List of non-terminal indicators.

To the best of my knowledge, the use of non-negative integers to specify
closures has first introduced on Quintus Prolog for providing
information for predicate cross-reference tools.

As each Logtalk entity is independently compiled, this directive must be
included in every object or category that contains a definition for the
described meta-predicate, even if the meta-predicate declaration is
inherited from another entity, to ensure proper compilation of
meta-arguments.

.. _predicates_discontiguous:

Discontiguous directive
~~~~~~~~~~~~~~~~~~~~~~~

The clause of an object (or category) predicate may not be contiguous.
In that case, we must declare the predicate discontiguous by using the
:ref:`directives_discontiguous_1` directive:

::

   :- discontiguous(foo/1).

This is a directive that we should avoid using: it makes your code
harder to read and it is not supported by some Prolog compilers.

As each Logtalk entity is compiled independently of other entities,
this directive must be included in every object or category that
contains a definition for the described predicate (even if the predicate
declaration is inherited from other entity).

.. _predicates_dynamic:

Dynamic directive
~~~~~~~~~~~~~~~~~

An object predicate can be static or dynamic. By default, all object
predicates are static. To declare a dynamic predicate we use the
:ref:`directives_dynamic_1` directive:

::

   :- dynamic(foo/1).

This directive may also be used to declare dynamic grammar rule
non-terminals. As each Logtalk entity is compiled independently from
other entities, this directive must be included in every object that
contains a definition for the described predicate (even if the predicate
declaration is inherited from other object or imported from a category).
If we omit the dynamic declaration then the predicate definition will be
compiled static. In the case of dynamic objects, static predicates
cannot be redefined using the database built-in methods (despite being
internally compiled to dynamic code).

Dynamic predicates can be used to represent persistent mutable object
state. Note that static objects may declare and define dynamic
predicates.

.. _predicates_op:

Operator directive
~~~~~~~~~~~~~~~~~~

An object (or category) predicate can be declared as an operator using
the familiar :ref:`directives_op_3` directive:

::

   :- op(Priority, Specifier, Operator).

Operators are local to the object (or category) where they are declared.
This means that, if you declare a public predicate as an operator, you
cannot use operator notation when sending to an object (where the
predicate is visible) the respective message (as this would imply
visibility of the operator declaration in the context of the *sender* of
the message). If you want to declare global operators and, at the same
time, use them inside an entity, just write the corresponding directives
at the top of your source file, before the entity opening directive.

Note that operators can also be declared using a scope directive. Only
these operators are visible to the :ref:`methods_current_op_3` reflection
method.

When the same operators are used on several entities within the same source
file, the corresponding directives must either be repeated in each entity or
appear before any entity that uses them. But in the later case, this results
in a global scope for the operators. If you prefer the operators to be local
to the source file, just *undefine* them at the end of the file. For example:

::

   % before any entity that uses the operator
   :- op(400, xfx, results).

   ...

   % after all entities that used the operator
   :- op(0, xfx, results).

Global operators can be declared in the application loader file.

.. _predicates_uses:

Uses directive
~~~~~~~~~~~~~~

When a predicate makes heavy use of predicates defined on other objects,
its predicate clauses can be verbose due to all the necessary message
sending goals. Consider the following example:

::

   foo :-
       ...,
       findall(X, list::member(X, L), A),
       list::append(A, B, C),
       list::select(Y, C, R),
       ...

Logtalk provides a directive, :ref:`directives_uses_2`, which allows us to
simplify the code above. One of the usage templates for this directive is:

::

   :- uses(Object, [
       Name1/Arity1, Name2/Arity2, ...
   ]).

Rewriting the code above using this directive results in a simplified
and more readable predicate definition:

::

   :- uses(list, [
       append/3, member/2, select/3
   ]).

   foo :-
       ...,
       findall(X, member(X, L), A),
       append(A, B, C),
       select(Y, C, R),
       ...

Logtalk also supports an extended version of this directive that allows
the declaration of :term:`predicate aliases <predicate alias>` using the
notation ``Predicate as Alias`` (or the alternative notation
``Predicate::Alias``). For example:

::

   :- uses(btrees, [new/1 as new_btree/1]).
   :- uses(queues, [new/1 as new_queue/1]).

You may use this extended version for solving conflicts between
predicates declared on several ``uses/2`` directives or just for giving
new names to the predicates that will be more meaningful on their using
context. It's also possible to define predicate aliases that are also
:term:`predicate shorthands <predicate shorthand>`. See the directive
documentation for details and examples.

The ``uses/2`` directive allows simpler predicate definitions as long as
there are no conflicts between the predicates declared in the directive
and the predicates defined in the object (or category) containing the
directive. A predicate (or its alias if defined) cannot be listed in
more than one ``uses/2`` directive. In addition, a ``uses/2`` directive
cannot list a predicate (or its alias if defined) which is defined in
the object (or category) containing the directive. Any conflicts are
reported by Logtalk as compilation errors.

The object identifier argument can also be a :term:`parameter variable`
when using the directive in a parametric object or a parametric category.
In this case, dynamic binding will necessarily be used for all listed
predicates (and non-terminals). The parameter variable must be instantiated
at runtime when the messages are sent. This feature simplifies experimenting
with multiple implementations of the same protocol (for example, to evaluate
the performance of each implementation for a particular case). It also
simplifies writing tests that check multiple implementations of the same
protocol.

.. _predicates_alias:

Alias directive
~~~~~~~~~~~~~~~

Logtalk allows the definition of an alternative name for an inherited or
imported predicate (or for an inherited or imported grammar rule
non-terminal) through the use of the :ref:`directives_alias_2` directive:

::

   :- alias(Entity, [
       Predicate1 as Alias1,
       Predicate2 as Alias2,
       ...
   ]).

This directive can be used in objects, protocols, or categories. The
first argument, ``Entity``, must be an entity referenced in the opening
directive of the entity containing the ``alias/2`` directive. It can be
an extended or implemented protocol, an imported category, an extended
prototype, an instantiated class, or a specialized class. The second
argument is a list of pairs of predicate indicators (or grammar rule
non-terminal indicators) using the ``as`` infix operator as connector.

A common use for the ``alias/2`` directive is to give an alternative
name to an inherited predicate in order to improve readability. For
example:

::

   :- object(square,
       extends(rectangle)).

       :- alias(rectangle, [width/1 as side/1]).

       ...

   :- end_object.

The directive allows both ``width/1`` and ``side/1`` to be used as
messages to the object ``square``. Thus, using this directive, there is
no need to explicitly declare and define a "new" ``side/1`` predicate.
Note that the ``alias/2`` directive does not rename a predicate, only
provides an alternative, additional name; the original name continues to
be available (although it may be masked due to the default inheritance
conflict mechanism).

Another common use for this directive is to solve conflicts when two
inherited predicates have the same name and arity. We may want to
call the predicate which is masked out by the Logtalk lookup algorithm
(see the :ref:`inheritance_inheritance` section) or we may need to
call both predicates. This is simply accomplished by using the
``alias/2`` directive to give alternative names to masked out or
conflicting predicates. Consider the following example:

::

   :- object(my_data_structure,
       extends(list, set)).

       :- alias(list, [member/2 as list_member/2]).
       :- alias(set,  [member/2 as set_member/2]).

       ...

   :- end_object.

Assuming that both ``list`` and ``set`` objects define a ``member/2``
predicate, without the ``alias/2`` directives, only the definition of
``member/2`` predicate in the object ``list`` would be visible on the
object ``my_data_structure``, as a result of the application of the
Logtalk predicate lookup algorithm. By using the ``alias/2`` directives,
all the following messages would be valid (assuming a public scope for
the predicates):

.. code-block:: text

   % uses list member/2
   | ?- my_data_structure::list_member(X, L).

    % uses set member/2
   | ?- my_data_structure::set_member(X, L).

   % uses list member/2
   | ?- my_data_structure::member(X, L).

When used this way, the ``alias/2`` directive provides functionality
similar to programming constructs of other object-oriented languages
that support multi-inheritance (the most notable example probably being
the renaming of inherited features in Eiffel).

Note that the ``alias/2`` directive never hides a predicate which is
visible on the entity containing the directive as a result of the
Logtalk lookup algorithm. However, it may be used to make visible a
predicate which otherwise would be masked by another predicate, as
illustrated in the above example.

The ``alias/2`` directive may also be used to give access to an
inherited predicate, which otherwise would be masked by another
inherited predicate, while keeping the original name as follows:

::

   :- object(my_data_structure,
       extends(list, set)).

       :- alias(list, [member/2 as list_member/2]).
       :- alias(set,  [member/2 as set_member/2]).

       member(X, L) :-
           ^^set_member(X, L).

       ...

   :- end_object.

Thus, when sending the message ``member/2`` to ``my_data_structure``,
the predicate definition in ``set`` will be used instead of the one
contained in ``list``.

.. _predicates_info:

Documenting directive
~~~~~~~~~~~~~~~~~~~~~

A predicate can be documented with arbitrary user-defined information by
using the :ref:`directives_info_2` directive:

::

   :- info(Name/Arity, List).

The second argument is a list of ``Key is Value`` terms. See the
:ref:`documenting_documenting` section for details.

.. _predicates_multifile:

Multifile directive
~~~~~~~~~~~~~~~~~~~

A predicate can be declared *multifile* by using the
:ref:`directives_multifile_1` directive:

::

   :- multifile(Name/Arity).

This allows clauses for a predicate to be defined in several objects
and/or categories. This is a directive that should be used with care.
It's commonly used in the definition of :term:`hook predicates <hook predicate>`.
Multifile predicates (and non-terminals) may also be declared dynamic
using the same predicate (or non-terminal) notation (multifile predicates
are static by default).

Logtalk precludes using a multifile predicate for breaking object
encapsulation by checking that the object (or category) declaring the
predicate (using a scope directive) defines it also as multifile.
This entity is said to contain the *primary declaration* for the multifile
predicate. Entities containing primary multifile predicate declarations
must always be compiled before entities defining clauses for those multifile
predicates. The Logtalk compiler will print a warning if the scope
directive is missing. Note also that the ``multifile/1`` directive
is mandatory when defining multifile predicates.

Consider the following simple example:

::

   :- object(main).

       :- public(a/1).
       :- multifile(a/1).
       a(1).

   :- end_object.

After compiling and loading the ``main`` object, we can define other
objects (or categories) that contribute with clauses for the multifile
predicate. For example:

::

   :- object(other).

       :- multifile(main::a/1).
       main::a(2).
       main::a(X) :-
           b(X).

       b(3).
       b(4).

   :- end_object.

After compiling and loading the above objects, you can use queries such
as:

.. code-block:: text

   | ?- main::a(X).

   X = 1 ;
   X = 2 ;
   X = 3 ;
   X = 4
   yes

Note that the order of multifile predicate clauses depend on several factors,
including loading order and compiler implementation details. Therefore, your
code should never assume or rely on a specific order of the multifile predicate
clauses.

When a clause of a multifile predicate is a rule, its body is compiled
within the context of the object or category defining the clause. This
allows clauses for multifile predicates to call local object or category
predicates. But the values of the *sender*, *this*, and *self* in the
implicit execution context are passed from the clause head to the clause
body. This is necessary to ensure that these values are always valid and
to allow multifile predicate clauses to be defined in categories. A call
to the ``parameter/2`` execution context methods, however, retrieves
parameters of the entity defining the clause, not from the entity for
which the clause is defined. The parameters of the entity for which the
clause is defined can be accessed by simple unification at the clause
head.

Multifile predicate rules should not contain cuts as these may prevent
other clauses for the predicate for being used by callers. The compiler
prints by default a warning when a cut is found in a multifile predicate
definition.

Local calls to the database methods from multifile predicate clauses
defined in an object take place in the object own database instead of
the database of the entity holding the multifile predicate primary
declaration. Similarly, local calls to the ``expand_term/2`` and
``expand_goal/2`` methods from a multifile predicate clause look for
clauses of the ``term_expansion/2`` and ``goal_expansion/2`` hook
predicates starting from the entity defining the clause instead of the
entity holding the multifile predicate primary declaration. Local calls
to the ``current_predicate/1``, ``predicate_property/2``, and
``current_op/3`` methods from multifile predicate clauses defined in an
object also lookup predicates and their properties in the object own
database instead of the database of the entity holding the multifile
predicate primary declaration.

.. _predicates_coinductive:

Coinductive directive
~~~~~~~~~~~~~~~~~~~~~

A predicate can be declared *coinductive* by using the
:ref:`directives_coinductive_1` directive. For example:

::

   :- coinductive(comember/2).

Logtalk support for coinductive predicates is experimental and requires a
:term:`backend Prolog compiler` with minimal support for cyclic terms. The
value of the read-only :ref:`coinduction flag <flag_coinduction>` is set to
``supported`` for the backend Prolog compilers providing that support.

.. _predicates_synchronized:

Synchronized directive
~~~~~~~~~~~~~~~~~~~~~~

A predicate can be declared *synchronized* by using the
:ref:`directives_synchronized_1` directive. For example:

::

   :- synchronized(write_log_entry/2).
   :- synchronized([produce/1, consume/1]).

See the section on
:ref:`synchronized predicates <threads_synchronized_predicates>`
for details.

.. _predicates_defining:

Defining predicates
-------------------

.. _predicates_objects:

Object predicates
~~~~~~~~~~~~~~~~~

We define object predicates as we have always defined Prolog predicates,
the only difference be that we have four more control structures (the
three message sending operators plus the external call operator) to play
with. For example, if we wish to define an object containing common
utility list predicates like ``append/2`` or ``member/2`` we could write
something like:

::

   :- object(list).

       :- public(append/3).
       :- public(member/2).

       append([], L, L).
       append([H| T], L, [H| T2]) :-
           append(T, L, T2).

       member(H, [H| _]).
       member(H, [_| T]) :-
           member(H, T).

   :- end_object.

Note that, abstracting from the opening and closing object directives
and the scope directives, what we have written is also valid Prolog code.
Calls in a predicate definition body default to the local predicates,
unless we use the message sending operators or the external call operator.
This enables easy conversion from Prolog code to Logtalk objects: we just
need to add the necessary encapsulation and scope directives to the old
code.

.. _predicates_categories:

Category predicates
~~~~~~~~~~~~~~~~~~~

Because a category can be imported by multiple objects, dynamic private
predicates must be called either in the context of :term:`self`, using the
:term:`message to self` control structure, :ref:`control_send_to_self_1`, or
in the context of :term:`this` (i.e. in the context of the object importing
the category). For example, if we want to define a category implementing
variables using destructive assignment where the variable values are stored
in *self* we could write:

::

   :- category(variable).

       :- public(get/2).
       :- public(set/2).

       :- private(value_/2).
       :- dynamic(value_/2).

       get(Var, Value) :-
           ::value_(Var, Value).

       set(Var, Value) :-
           ::retractall(value_(Var, _)), 
           ::asserta(value_(Var, Value).

   :- end_category.

In this case, the ``get/2`` and ``set/2`` predicates will always
access/update the correct definition, contained in the object receiving
the messages. The alternative, storing the variable values in *this*,
such that each object importing the category will have its own
definition for the ``value_/2`` private predicate is simple: just omit
the use of the ``::/1`` control construct in the code above.

A category can only contain clauses for static predicates. Nevertheless,
as the example above illustrates, there are no restrictions in declaring
and calling dynamic predicates from inside a category.

.. _predicates_metadef:

Meta-predicates
~~~~~~~~~~~~~~~

Meta-predicates may be defined inside objects and categories as any other
predicate. A meta-predicate is declared using the
:ref:`directives_meta_predicate_1` directive as described earlier on
this section. When defining a meta-predicate, the arguments in the 
clause heads corresponding to the meta-arguments must be variables.
All meta-arguments are called in the context of the object or category
calling the meta-predicate. In particular, when sending a message that
corresponds to a meta-predicate, the meta-arguments are called in the
context of the object or category sending the message.

The most simple example is a meta-predicate with a meta-argument that is
called as a goal. E.g. the :ref:`methods_ignore_1` built-in predicate could
be defined as:

::

   :- public(ignore/1).
   :- meta_predicate(ignore(0)).

   ignore(Goal) :-
      (Goal -> true; true).

The ``0`` in the meta-predicate template tells us that the argument will be
called as-is.

Some meta-predicates have meta-arguments which are not goals but
:term:`closures <closure>`. Logtalk supports the definition of meta-predicates
that are called with closures instead of goals as long as the definition uses
the :ref:`methods_call_N` built-in predicate to call the closure with the
additional arguments. A classical example is a list mapping predicate:

::

   :- public(map/2).
   :- meta_predicate(map(1, *)).

   map(_, []).
   map(Closure, [Arg| Args]) :-
       call(Closure, Arg),
       map(Closure, Args).

Note that in this case the meta-predicate directive specifies that the
closure will be extended with exactly one additional argument. When
calling a meta-predicate, a closure can correspond to a user-defined
predicate, a built-in predicate, a :term:`lambda expression`, or a
control construct.

In some cases, is not a meta-argument but one of its sub-terms that is
called as a goal or used as a closure. For example:

::

   :- public(call_all/1).
   :- meta_predicate(call_all(::)).

   call_all([]).
   call_all([Goal| Goals]) :-
       call(Goal),
       call_all(Goals).

The ``::`` mode indicator in the meta-predicate template allows the
corresponding argument in the meta-predicate definiton to be a
non-variable term and instructs the compiler to look into the argument
sub-terms for goal and closure meta-variables.

.. _predicates_lambdas:

Lambda expressions
~~~~~~~~~~~~~~~~~~

The use of `lambda
expressions <https://en.wikipedia.org/wiki/Lambda_calculus>`_ as
meta-predicate goal and :term:`closure` arguments often saves writing
auxiliary predicates for the sole purpose of calling the meta-predicates.
A simple example of a lambda expression is:

.. code-block:: text

   | ?- meta::map([X,Y]>>(Y is 2*X), [1,2,3], Ys).
   Ys = [2,4,6]
   yes

In this example, a lambda expression, ``[X,Y]>>(Y is 2*X)``, is used as
an argument to the ``map/3`` list mapping predicate, defined in the
library object ``meta``, in order to double the elements of a list of
integers. Using a lambda expression avoids writing an auxiliary
predicate for the sole purpose of doubling the list elements. The lambda
parameters are represented by the list ``[X,Y]``, which is connected to
the lambda goal, ``(Y is 2*X)``, by the ``(>>)/2`` operator.

Currying is supported. I.e. it is possible to write a lambda expression
whose goal is another lambda expression. The above example can be
rewritten as:

.. code-block:: text

   | ?- meta::map([X]>>([Y]>>(Y is 2*X)), [1,2,3], Ys).
   Ys = [2,4,6]
   yes

Lambda expressions may also contain lambda free variables. I.e.
variables that are global to the lambda expression. For example, using
GNU Prolog as the backend compiler, we can write:

.. code-block:: text

   | ?- meta::map({Z}/[X,Y]>>(Z#=X+Y), [1,2,3], Zs).
   Z = _#22(3..268435455)
   Zs = [_#3(2..268435454),_#66(1..268435453),_#110(0..268435452)]
   yes

The ISO Prolog construct ``{}/1`` for representing the lambda free
variables as this representation is often associated with set
representation. Note that the order of the free variables is of no
consequence (on the other hand, a list is used for the lambda parameters
as their order does matter).

Both lambda free variables and lambda parameters can be any Prolog term.
Consider the following example by Markus Triska:

.. code-block:: text

   | ?- meta::map([A-B,B-A]>>true, [1-a,2-b,3-c], Zs).
   Zs = [a-1,b-2,c-3]
   yes

Lambda expressions can be used, as expected, in non-deterministic
queries as in the following example using SWI-Prolog as the backend
compiler and Markus Triska's CLP(FD) library:

.. code-block:: text

   | ?- meta::map({Z}/[X,Y]>>(clpfd:(Z#=X+Y)), Xs, Ys).
   Xs = [],
   Ys = [] ;
   Xs = [_G1369],
   Ys = [_G1378],
   _G1369+_G1378#=Z ;
   Xs = [_G1579, _G1582],
   Ys = [_G1591, _G1594],
   _G1582+_G1594#=Z,
   _G1579+_G1591#=Z ;
   Xs = [_G1789, _G1792, _G1795],
   Ys = [_G1804, _G1807, _G1810],
   _G1795+_G1810#=Z,
   _G1792+_G1807#=Z,
   _G1789+_G1804#=Z ;
   ...

As illustrated by the above examples, lambda expression syntax reuses
the ISO Prolog construct ``{}/1`` and the standard operators ``(/)/2``
and ``(>>)/2``, thus avoiding defining new operators, which is always
tricky for a portable system such as Logtalk. The operator ``(>>)/2``
was chosen as it suggests an arrow, similar to the syntax used in other
languages such as OCaml and Haskell to connect lambda parameters with
lambda functions. This syntax was also chosen in order to simplify
parsing, error checking, and compilation of lambda expressions. The
full specification of the lambda expression syntax can be found in
the the :ref:`language grammar <grammar_lambdas>`.

The compiler checks whenever possible that all variables in a lambda
expression are either classified as free variables or as lambda
parameters. Non-classified variables in a lambda expression should be
regarded as a programming error. The compiler also checks if a variable
is classified as both a free variable and a lambda parameter. There
are a few cases where a variable playing a dual role is intended but,
in general, this also results from a programming error. A third check
verifies that no lambda parameter variable is used elsewhere in a
clause. Such cases are either programming errors, when the variable
appears before the lambda expression, or bad programming style, when
the variable is used after the lambda expression. These linter warnings
are controlled by the :ref:`lambda_variables <flag_lambda_variables>`
flag. Note, however, that the dynamic features of the language and lack
of sufficient information at compile time may prevent the compiler of
checking all uses of lambda expressions. 

.. warning::

   Variables listed in lambda parameters must not be shared with
   other goals in a clause.

An optimizing meta-predicate and lambda expression compiler, based on
the :ref:`term-expansion mechanism <expansion_expansion>`, is provided
as a standard library for practical performance.

.. _predicates_redefining:

Redefining built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk built-in predicates and Prolog built-in predicates can be redefined
inside objects and categories. Although the redefinition of Logtalk built-in
predicates should be avoided, the support for redefining Prolog built-in
predicates is a practical requirement given the different sets of proprietary
built-in predicates provided by backend Prolog systems.

The compiler supports a :ref:`redefined_built_ins <flag_redefined_built_ins>`
flag, whose default value is `silent`, that can be set to `warning`
to alert the user of any redefined Logtalk or Prolog built-in predicate.

The redefinition of Prolog built-in predicates can be combined with the
:ref:`conditional compilation directives <conditional_compilation_directives>`
when writing portable applications where some of the supported backends
don't provide a built-in predicate found in the other backends. As an example,
consider the de facto standard list length predicate, ``length/2``. This
predicate is provided as a built-in predicate in most but not all backends.
The ``list`` library object includes the code:

::

   :- if(predicate_property(length(_, _), built_in)).
   
       length(List, Length) :-
           {length(List, Length)}.
   
   :- else.
   
       length(List, Length) :-
           ...
   
   :- endif.

I.e. the object will use the built-in predicate when available. Otherwise,
it will use the object provided predicate definition.

The redefinition of built-in predicates can also be accomplished using
:term:`predicate shorthands <predicate shorthand>`. This can be useful
when porting code while minimizing the changes. For example, assume
that existing code uses the ``format/2`` de facto standard predicate
for writing messages. To convert the code to use the
:ref:`message printing mechanism <printing_printing>` we could write:

::

   :- uses(logtalk, [
       print_message(comment, core, Format+Arguments) as format(Format, Arguments)
   ]).
   
   process(Crate, Contents) :-
       format('Processing crate ~w...', [Crate]),
       ...,
       format('Filing with ~w...', [Contents]),
       ....

The predicate shorthand instructs the compiler to rewrite all ``format/2``
goals as ``logtalk::print_message/3`` goals, thus allowing us to reuse
the code without changes.

.. _predicates_dcgs:

Definite clause grammar rules
-----------------------------

Definite clause grammar rules provide a convenient notation to represent
the rewrite rules common of most grammars in Prolog. In Logtalk,
definite clause grammar rules can be encapsulated in objects and
categories. Currently, the ISO/IEC WG17 group is working on a draft
specification for a definite clause grammars Prolog standard. Therefore,
in the mean time, Logtalk follows the common practice of Prolog
compilers supporting definite clause grammars, extending it to support
calling grammar rules contained in categories and objects. A common
example of a definite clause grammar is the definition of a set of rules
for parsing simple arithmetic expressions:

::

   :- object(calculator).

       :- public(parse/2).

       parse(Expression, Value) :-
           phrase(expr(Value), Expression).

       expr(Z) --> term(X), "+", expr(Y), {Z is X + Y}.
       expr(Z) --> term(X), "-", expr(Y), {Z is X - Y}.
       expr(X) --> term(X).

       term(Z) --> number(X), "*", term(Y), {Z is X * Y}.
       term(Z) --> number(X), "/", term(Y), {Z is X / Y}.
       term(Z) --> number(Z).

       number(C) --> "+", number(C).
       number(C) --> "-", number(X), {C is -X}.
       number(X) --> [C], {0'0 =< C, C =< 0'9, X is C - 0'0}.

   :- end_object. 

The predicate :ref:`methods_phrase_2` called
in the definition of predicate ``parse/2`` above is a Logtalk built-in
method, similar to the predicate with the same name found on most Prolog
compilers that support definite clause grammars. After compiling and
loading this object, we can test the grammar rules with calls such as
the following one:

.. code-block:: text

   | ?- calculator::parse("1+2-3*4", Result).

   Result = -9
   yes

In most cases, the predicates resulting from the translation of the
grammar rules to regular clauses are not declared. Instead, these
predicates are usually called by using the built-in methods
:ref:`methods_phrase_2` and :ref:`methods_phrase_3` as shown in the
example above. When we want to use the built-in methods ``phrase/2`` and
``phrase/3``, the non-terminal used as first argument must be within the
scope of the *sender*. For the above example, assuming that we want the
predicate corresponding to the ``expr//1`` non-terminal to be public,
the corresponding scope directive would be:

::

   :- public(expr//1). 

The ``//`` infix operator used above tells the Logtalk compiler that the
scope directive refers to a grammar rule non-terminal, not to a
predicate. The idea is that the predicate corresponding to the
translation of the ``expr//1`` non-terminal will have a number of
arguments equal to one plus the number of additional arguments necessary
for processing the implicit difference list of tokens.

In the body of a grammar rule, we can call rules that are inherited from
ancestor objects, imported from categories, or contained in other
objects. This is accomplished by using non-terminals as messages. Using
a non-terminal as a message to *self* allows us to call grammar rules in
categories and ancestor objects. To call grammar rules encapsulated in
other objects, we use a non-terminal as a message to those objects.
Consider the following example, containing grammar rules for parsing
natural language sentences:

::

   :- object(sentence,
       imports(determiners, nouns, verbs)).

       :- public(parse/2).

       parse(List, true) :-
           phrase(sentence, List).
       parse(_, false).

       sentence --> noun_phrase, verb_phrase.

       noun_phrase --> ::determiner, ::noun.
       noun_phrase --> ::noun.

       verb_phrase --> ::verb.
       verb_phrase --> ::verb, noun_phrase.

   :- end_object.

The categories imported by the object would contain the necessary
grammar rules for parsing determiners, nouns, and verbs. For example:

::

   :- category(determiners).

       :- private(determiner//0).

       determiner --> [the].
       determiner --> [a].

   :- end_category.

Along with the message sending operators (``::/1``, ``::/2``, and ``^^/1``),
we may also use other control constructs such as ``\+/1``, ``!/0``, ``;/2``,
``->/2``, and ``{}/1`` in the body of a grammar. When using a backend Prolog
compiler that supports modules, we may also use the ```:/2`` control construct.
In addition, grammar rules may contain meta-calls (a variable taking the place
of a non-terminal), which are translated to calls of the built-in method
``phrase/3``.

You may have noticed that Logtalk defines :ref:`control_external_call_1`
as a control construct for bypassing the compiler when compiling a clause body
goal. As exemplified above, this is the same control construct that is used in
grammar rules for bypassing the expansion of rule body goals when a rule is
converted into a clause. Both control constructs can be combined in order to
call a goal from a grammar rule body, while bypassing at the same time the
Logtalk compiler. Consider the following example:

::

   bar :-
       write('bar predicate called'), nl.


   :- object(bypass).

       :- public(foo//0).

       foo --> {{bar}}.

   :- end_object.

After compiling and loading this code, we may try the following query:

.. code-block:: text

   | ?- logtalk << phrase(bypass::foo, _, _).

   bar predicate called
   yes

This is the expected result as the expansion of the grammar rule into a
clause leaves the ``{bar}`` goal untouched, which, in turn, is converted
into the goal ``bar`` when the clause is compiled.

A grammar rule non-terminal may be declared as dynamic or discontiguous,
as any object predicate, using the same ``Name//Arity`` notation
illustrated above for the scope directives. In addition, grammar rule
non-terminals can be documented using the :ref:`directives_info_2`
directive, as in the following example:

::

   :- public(sentence//0).

   :- info(sentence//0, [
       comment is 'Rewrites sentence into noun and verb phrases.']).

.. _predicates_methods:

Built-in methods
----------------

Built-in methods are built-in object and category predicates. These include
methods to access message execution context, to find sets of solutions, to
inspect objects, for database handling, for term and goal expansion, and
for printing messages. Some of them are counterparts to standard Prolog
built-in predicates that take into account Logtalk semantics. Similar to
Prolog built-in predicates, built-in methods cannot not be redefined.

.. _predicates_context:

Execution context methods
~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk defines five built-in private methods to access an object
execution context. These methods are in the common usage scenarios
translated to a single unification performed at compile time with a
clause head context argument. Therefore, they can be freely used without
worrying about performance penalties. When called from inside a
category, these methods refer to the execution context of the object
importing the category. These methods are private and cannot be used as
messages to objects.

To find the object that received the message under execution we may use
the :ref:`methods_self_1` method. We may also
retrieve the object that has sent the message under execution using the
:ref:`methods_sender_1` method.

The method :ref:`methods_this_1` enables us to
retrieve the name of the object for which the predicate clause whose
body is being executed is defined instead of using the name directly.
This helps to avoid breaking the code if we decide to change the object
name and forget to change the name references. This method may also be
used from within a category. In this case, the method returns the object
importing the category on whose behalf the predicate clause is being
executed.

Here is a short example including calls to these three object execution
context methods:

::

   :- object(test).

       :- public(test/0).

       test :-
           this(This), 
           write('Calling predicate definition in '),
           writeq(This), nl,
           self(Self),
           write('to answer a message received by '),
           writeq(Self), nl,
           sender(Sender),
           write('that was sent by '),
           writeq(Sender), nl, nl.

   :- end_object.


   :- object(descendant,
       extends(test)).

   :- end_object.

After compiling and loading these two objects, we can try the following
goal:

.. code-block:: text

   | ?- descendant::test.

   Calling predicate definition in test
   to answer a message received by descendant
   that was sent by user
   yes

Note that the goals ``self(Self)``, ``sender(Sender)``, and
``this(This)``, being translated to unifications with the clause head
context arguments at compile time, are effectively removed from the
clause body. Therefore, a clause such as:

::

   predicate(Arg) :-
       self(Self),
       atom(Arg),
       ... .

is compiled with the goal ``atom(Arg)`` as the first condition on the
clause body. As such, the use of these context execution methods do not
interfere with the optimizations that some Prolog compilers perform when
the first clause body condition is a call to a built-in type-test
predicate or a comparison operator.

For parametric objects and categories, the method :ref:`methods_parameter_2`
enables us to retrieve current parameter values (see the section on
:ref:`parametric objects <objects_parametric>` for a detailed description).
For example:

::

   :- object(block(_Color)).

       :- public(test/0).

       test :-
           parameter(1, Color), 
           write('Color parameter value is '),
           writeq(Color), nl.

   :- end_object.

An alternative to the ``parameter/2`` predicate is to use
:term:`parameter variables <parameter variable>`:

::

   :- object(block(_Color_)).

       :- public(test/0).

       test :-
           write('Color parameter value is '),
           writeq(_Color_), nl.

   :- end_object.

After compiling and loading either version of the object, we can try the
following goal:

.. code-block:: text

   | ?- block(blue)::test.

   Color parameter value is blue
   yes

Calls to the ``parameter/2`` method are translated to a compile time
unification when the second argument is a variable. When the second
argument is bound, the calls are translated to a call to the built-in
predicate ``arg/3``.

When type-checking predicate arguments, it is often useful to include
the predicate execution context when reporting an argument error. The
:ref:`methods_context_1` method provides
access to that context. For example, assume a predicate ``foo/2`` that
takes an atom and an integer as arguments. We could type-check the
arguments by writing (using the library ``type`` object):

::

   foo(A, N) :-
       % type-check arguments
       context(Context),
       type::check(atom, A, Context),
       type::check(integer, N, Context),
       % arguments are fine; go ahead
       ... .

.. _predicates_errors:

Error handling and throwing methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Besides the :ref:`methods_catch_3` and :ref:`methods_throw_1` methods inherited from
Prolog, Logtalk also provides a set of convenience methods to throw
standard ``error/2`` exception terms:
:ref:`methods_instantiation_error_0`,
:ref:`methods_type_error_2`,
:ref:`methods_domain_error_2`,
:ref:`methods_existence_error_2`,
:ref:`methods_permission_error_3`,
:ref:`methods_representation_error_1`,
:ref:`methods_evaluation_error_1`,
:ref:`methods_resource_error_1`,
:ref:`methods_syntax_error_1`, and
:ref:`methods_system_error_0`.

.. _predicates_database:

Database methods
~~~~~~~~~~~~~~~~

Logtalk provides a set of built-in methods for :term:`object database` handling
similar to the usual database Prolog predicates:
:ref:`methods_abolish_1`,
:ref:`methods_asserta_1`,
:ref:`methods_assertz_1`,
:ref:`methods_clause_2`,
:ref:`methods_retract_1`, and
:ref:`methods_retractall_1`. These
methods always operate on the database of the object receiving the corresponding
message. When called locally, these predicates take into account any
:ref:`directives_uses_2` or :ref:`directives_use_module_2` directives that refer
to the dynamic predicate being handled. For example, in the following object, the
clauses for the ``data/1`` predicate are retracted and asserted in ``user`` due to
the ``uses/2`` directive:

::

   :- object(an_object).
   
       :- uses(user, [data/1]).
   
       :- public(some_predicate/1).
       some_predicate(Arg) :-
           retractall(data(_)),
           assertz(data(Arg)).
       
   :- end_object.

When working with dynamic grammar rule non-terminals, you may use the
built-in method :ref:`methods_expand_term_2` convert a
grammar rule into a clause that can then be used with the database
methods.

.. _predicates_metacalls:

Meta-call methods
~~~~~~~~~~~~~~~~~

Logtalk supports the generalized :ref:`methods_call_N` meta-predicate. This
built-in private meta-predicate must be used in the implementation of
meta-predicates which work with :term:`closures <closure>` instead of goals.
In addition, Logtalk supports the built-in private meta-predicates
:ref:`methods_ignore_1`, :ref:`methods_once_1`, and
:ref:`methods_not_1`. These methods cannot be used as messages to objects.

.. _predicates_solutions:

All solutions methods
~~~~~~~~~~~~~~~~~~~~~

The usual all solutions meta-predicates are built-in private methods in
Logtalk: :ref:`methods_bagof_3`, :ref:`methods_findall_3`,
:ref:`methods_findall_4`, and :ref:`methods_setof_3`. There is also a
:ref:`methods_forall_2` method that implements generate-and-test loops.
These methods cannot be used as messages to objects.

.. _predicates_reflection:

Reflection methods
~~~~~~~~~~~~~~~~~~

Logtalk provides a comprehensive set of built-in predicates and built-in
methods for querying about entities and predicates. Some of the information,
however, requires that the source files are compiled with the
:ref:`source_data <flag_source_data>` flag turned on.

The :ref:`reflection API <reflection_reflection>` supports two different views
on entities and their contents, which we may call the *transparent box view*
and the *black box view*. In the transparent box view, we look into an entity
disregarding how it will be used and returning all information available
on it, including predicate declarations and predicate definitions. This
view is supported by the entity property built-in predicates. In the
black box view, we look into an entity from a usage point-of-view using
built-in methods for inspecting object operators and predicates that are
within scope from where we are making the call:
:ref:`methods_current_op_3`, which returns operator specifications,
:ref:`methods_predicate_property_2`, which returns predicate properties,
and :ref:`methods_current_predicate_1`, which enables us to query about
user-defined predicate definitions. See below for a more detailed description
of these methods.

.. _predicates_parsing:

Definite clause grammar parsing methods and non-terminals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk supports two definite clause grammar parsing built-in private
methods, :ref:`methods_phrase_2` and :ref:`methods_phrase_3`, with definitions
similar to the predicates with the same name found on most Prolog
compilers that support definite clause grammars. These methods cannot be
used as messages to objects.

Logtalk also supports :ref:`methods_phrase_1`, :ref:`methods_call_1`, and
:ref:`methods_eos_0` built-in non-terminals.
The ``call//1-N`` non-terminals takes a :term:`closure` (which can be a lambda
expression) plus zero or more additional arguments and are processed by
appending the input list of tokens and the list of remaining tokens to
the arguments.

.. _predicates_properties:

Predicate properties
--------------------

We can find the properties of visible predicates by calling the
:ref:`methods_predicate_property_2` built-in method. For example:

.. code-block:: text

   | ?- bar::predicate_property(foo(_), Property).

Note that this method takes into account the predicate's scope declarations.
In the above example, the call will only return properties for public
predicates.

An object's set of visible predicates is the union of all the predicates
declared for the object with all the built-in methods and all the
Logtalk and Prolog built-in predicates.

The following predicate properties are supported:

``scope(Scope)``
   The predicate scope (useful for finding the predicate scope with a
   single call to ``predicate_property/2``)
``public``, ``protected``, ``private``
   The predicate scope (useful for testing if a predicate have a
   specific scope)
``static``, ``dynamic``
   All predicates are either static or dynamic (note, however, that a
   dynamic predicate can only be abolished if it was dynamically
   declared)
``logtalk``, ``prolog``, ``foreign``
   A predicate can be defined in Logtalk source code, Prolog code, or in
   foreign code (e.g. in C)
``built_in``
   The predicate is a built-in predicate
``multifile``
   The predicate is declared multifile (i.e. it can have clauses defined
   in multiple files or entities)
``meta_predicate(Template)``
   The predicate is declared as a meta-predicate with the specified
   template
``coinductive(Template)``
   The predicate is declared as a coinductive predicate with the
   specified template
``declared_in(Entity)``
   The predicate is declared (using a scope directive) in the specified
   entity
``defined_in(Entity)``
   The predicate definition is looked up in the specified entity (note
   that this property does not necessarily imply that clauses for the
   predicate exist in ``Entity``; the predicate can simply be false as
   per the :term:`closed-world assumption`)
``redefined_from(Entity)``
   The predicate is a redefinition of a predicate definition inherited
   from the specified entity
``non_terminal(NonTerminal//Arity)``
   The predicate resulted from the compilation of the specified grammar
   rule non-terminal
``alias_of(Predicate)``
   The predicate (name) is an alias for the specified predicate
``alias_declared_in(Entity)``
   The predicate alias is declared in the specified entity
``synchronized``
   The predicate is declared as synchronized (i.e. it's a deterministic
   predicate synchronized using a mutex when using a backend Prolog
   compiler supporting a compatible multi-threading implementation)

Some properties are only available when the entities are defined in
source files and when those source files are compiled with the
:ref:`source_data <flag_source_data>` flag turned on:

``inline``
   The predicate definition is inlined
``auxiliary``
   The predicate is not user-defined but rather automatically generated
   by the compiler or the :ref:`term-expansion mechanism <expansion_expansion>`
``mode(Mode, Solutions)``
   Instantiation, type, and determinism mode for the predicate (which
   can have multiple modes)
``info(ListOfPairs)``
   Documentation key-value pairs as specified in the user-defined
   ``info/2`` directive
``number_of_clauses(N)``
   The number of clauses for the predicate existing at compilation time
   (note that this property is not updated at runtime when asserting and
   retracting clauses for dynamic predicates)
``number_of_rules(N)``
   The number of rules for the predicate existing at compilation time
   (note that this property is not updated at runtime when asserting and
   retracting clauses for dynamic predicates)
``declared_in(Entity, Line)``
   The predicate is declared (using a scope directive) in the specified
   entity in a source file at the specified line (if applicable)
``defined_in(Entity, Line)``
   The predicate is defined in the specified entity in a source file at
   the specified line (if applicable)
``redefined_from(Entity, Line)``
   The predicate is a redefinition of a predicate definition inherited
   from the specified entity, which is defined in a source file at the
   specified line (if applicable)
``alias_declared_in(Entity, Line)``
   The :term:`predicate alias` is declared in the specified entity in a
   source file at the specified line (if applicable)

The properties ``declared_in/1-2``, ``defined_in/1-2``, and
``redefined_from/1-2`` do not apply to built-in methods and Logtalk or
Prolog built-in predicates. Note that if a predicate is declared in a
category imported by the object, it will be the category name — not the
object name — that will be returned by the property ``declared_in/1``.
The same is true for protocol declared predicates.

.. _predicates_finding:

Finding declared predicates
---------------------------

We can find, by backtracking, all visible user predicates by calling the
:ref:`methods_current_predicate_1` built-in method. This method takes into
account predicate scope declarations. For example, the following call will
only return user predicates that are declared public:

.. code-block:: text

   | ?- some_object::current_predicate(Name/Arity).

The predicate property ``non_terminal/1`` may be used to retrieve all
grammar rule non-terminals declared for an object. For example:

::

   current_non_terminal(Object, Name//Args) :-
       Object::current_predicate(Name/Arity),
       functor(Predicate, Functor, Arity),
       Object::predicate_property(Predicate, non_terminal(Name//Args)).

Usually, the non-terminal and the corresponding predicate share the same
functor but users should not rely on this always being true.

.. _predicates_prolog:

Calling Prolog predicates
-------------------------

Logtalk is designed for both *robustness* and *portability*. In the context
of calling Prolog predicates, robustness requires that the compilation of
Logtalk source code must not have *accidental* dependencies on Prolog code that
happens to be loaded at the time of the compilation. One immediate consequence
is that only Prolog *built-in* predicates are visible from within objects and
categories. But Prolog systems provide a widely diverse set of built-in
predicates, easily rising portability issues. Relying on non-standard
predicates is often unavoidable, however, due to the narrow scope of Prolog
standards. Logtalk applications may also require calling user-defined Prolog
predicates, either in ``user`` or in Prolog modules. 

Calling Prolog built-in predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In predicate clauses and object ``initialization/1`` directives, predicate
calls that are not prefixed with a message sending, super call, or module
qualification operator (``::``, ``^^``, or ``:``), are compiled to either
calls to local predicates or as calls to Logtalk/Prolog built-in predicates.
A predicate call is compiled as a call to a local predicate if the object (or
category) contains a scope directive, a definition for the called predicate,
or a dynamic declaration for it. When that is not the case, the compiler
checks if the call corresponds to a Logtalk or Prolog built-in predicate.
Consider the following example:

::

   foo :-
       ...,
       write(bar),
       ...

The call to the ``write/1`` predicate will be compiled as a call to the
corresponding Prolog standard built-in predicate unless the object (or
category) containing the above definition also contains a predicate
named ``write/1`` or a dynamic directive for the predicate.

When calling non-standard Prolog built-in predicates or using non-standard
Prolog arithmetic functions, we may run into portability problems while
trying your applications with different backend Prolog compilers. We can
use the compiler :ref:`portability flag <flag_portability>` to generate
warnings for calls to non-standard predicates and arithmetic functions.
We can also help document those calls using the :ref:`directives_uses_2`
directive. For example, a few Prolog systems provide an ``atom_string/2``
non-standard predicate. We can write (in the object or category calling the
predicate):

::

   :- uses(user, [atom_string/2])

This directive is based on the fact that built-in predicates are visible in
plain Prolog (i.e. in ``user``). Besides helping to document the dependency
on a non-standard built-in predicate, this directive will also silence the
compiler portability warning.

.. _predicates_prolog_meta:

Calling Prolog non-standard built-in meta-predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prolog built-in meta-predicates may only be called locally within
objects or categories, i.e. they cannot be used as messages. Compiling
calls to non-standard, Prolog built-in meta-predicates can be tricky,
however, as there is no standard way of checking if a built-in predicate
is also a meta-predicate and finding out which are its meta-arguments.
But Logtalk supports overriding the original meta-predicate template
when not programmatically available or usable. For example, assume a
``det_call/1`` Prolog built-in meta-predicate that takes a goal as
argument. We can add to the object (or category) calling it the
directive:

::

   :- meta_predicate(user::det_call(0)).

Another solution is to explicitly declare all non-standard built-in Prolog
meta-predicates in the corresponding adapter file using the internal
predicate ``'$lgt_prolog_meta_predicate'/3``. For example:

::

   '$lgt_prolog_meta_predicate'(det_call(_), det_call(0), predicate).

The third argument can be either the atom ``predicate`` or the atom
``control_construct``, a distinction that is useful when compiling in
debug mode.

.. _predicates_prolog_user:

Calling Prolog user-defined plain predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

User-defined Prolog plain predicates (i.e. predicates that are not defined
in a Prolog module) can be called from within objects or categories by
sending the corresponding message to ``user``. For example:

::

   foo :-
       ...,
       user::bar,
       ...

In alternative, we can use the :ref:`directives_uses_2` directive and write:

::

   :- uses(user, [bar/0]).

   foo :-
       ...,
       bar,
       ...

Note that ``user`` is a pseudo-object in Logtalk containing all predicate
definitions that are not encapsulated (either in a Logtalk entity or a
Prolog module).

When the Prolog predicate is not a meta-predicate, we can also use the
:ref:`control_external_call_1` compiler bypass control construct. For
example:

::

   foo :-
       ...,
       {bar},
       ...

But note that in this case the :ref:`reflection API <reflection_reflection>`
will not record the dependency of the ``foo/0`` predicate on the Prolog
``bar/0`` predicate as we are effectively bypassing the compiler.

.. _predicates_prolog_module:

Calling Prolog module predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prolog module predicates can be called from within objects or categories by
using explicit qualification. For example:

::

   foo :-
       ...,
       module:bar,
       ...

You can also use in alternative the :ref:`directives_use_module_2` directive
to call the module predicates using implicit qualification:

::

   :- use_module(module, [bar/0]).

   foo :-
       ...,
       bar,
       ...

Note that the first argument of the ``use_module/2`` directive, when used
within an object or a category, is a *module name*, not a *file specification*
(also be aware that Prolog modules are sometimes defined in files with names
that differ from the module names).

As loading a Prolog module varies between Prolog systems, the actual loading
directive or goal is preferably done from the application :term:`loader file`.
An advantage of this approach is that it contributes to a clean separation
between *loading* and *using* a resource with the loader file being the
central point that loads all application resources (complex applications
often use a *hierarchy* of loader files but the main idea remains the same).

As an example, assume that we need to call predicates defined in a CLP(FD)
Prolog library, which can be loaded using ``library(clpfd)`` as the file
specification. In the loader file, we would add:

::

   :- use_module(library(clpfd), []).

Specifying an empty import list is often used to avoid adding the module
exported predicates to plain Prolog. In the objects and categories we can
then call the library predicates, using implicit or explicit qualification,
as explained. For example:

::

   :- object(puzzle).

       :- public(puzzle/1).

       :- use_module(clpfd, [
           all_different/1, ins/2, label/1,
           (#=)/2, (#\=)/2,
           op(700, xfx, #=), op(700, xfx, #\=)
       ]).

       puzzle([S,E,N,D] + [M,O,R,E] = [M,O,N,E,Y]) :-
           Vars = [S,E,N,D,M,O,R,Y],
           Vars ins 0..9,
           all_different(Vars),
                     S*1000 + E*100 + N*10 + D +
                     M*1000 + O*100 + R*10 + E #=
           M*10000 + O*1000 + N*100 + E*10 + Y,
           M #\= 0, S #\= 0,
           label([M,O,N,E,Y]).

   :- end_object.

.. warning::

   The actual module code **must** be loaded prior to compilation of Logtalk
   source code that uses it. In particular, programmers should not expect
   that the module be auto-loaded (including when using a backend Prolog
   compiler that supports an auto-loading mechanism).

The module identifier argument can also be a :term:`parameter variable`
when using the directive in a parametric object or a parametric category.
In this case, dynamic binding will necessarily be used for all listed
predicates (and non-terminals). The parameter variable must be instantiated
at runtime when the calls are made.

Calling Prolog module meta-predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Logtalk library provides implementations of common meta-predicates,
which can be used in place of module meta-predicates (e.g. list mapping
meta-predicates). If that is not the case the Logtalk compiler may need
help to understand the module meta-predicate templates. Despite some recent
progress in standardization of the syntax of ``meta_predicate/1`` directives
and of the ``meta_predicate/1`` property returned by the ``predicate_property/2``
reflection predicate, portability is still a major problem. Thus, Logtalk
allows the original ``meta_predicate/1`` directive to be **overridden**
with a local directive that Logtalk can make sense of. Note that Logtalk
is not based on a predicate prefixing mechanism as found in module systems.
This fundamental difference precludes an automated solution at the Logtalk
compiler level.

As an example, assume that you want to call from an object (or a category)
a module meta-predicate with the following meta-predicate directive:

::

   :- module(foo, [bar/2]).

   :- meta_predicate(bar(*, :)).

The ``:`` meta-argument specifier is ambiguous. It tell us that the second
argument of the meta-predicate is module sensitive but it does not tell us
*how*. Some legacy module libraries and some Prolog systems use ``:`` to
mean ``0`` (i.e. a meta-argument that will be meta-called). Some others
use ``:`` for meta-arguments that are not meta-called but that still need
to be augmented with module information. Whichever the case, the Logtalk
compiler doesn't have enough information to unambiguously parse the
directive and correctly compile the  meta-arguments in the meta-predicate
call. Therefore, the Logtalk compiler will generate an error stating that
``:`` is not a valid meta-argument specifier when trying to compile a
``foo:bar/2`` goal. There are two alternative solutions for this problem.
The advised solution is to override the meta-predicate directive by writing,
inside the object (or category) where the meta-predicate is called:

::

   :- meta_predicate(bar(*, *)).

or:

::

   :- meta_predicate(bar(*, 0)).

depending on the true meaning of the second meta-argument. The second
alternative, only usable when the meta-argument can be handled as a
normal argument, is to simply use the :ref:`control_external_call_1`
compiler bypass control construct to call the meta-predicate as-is:

::

   ... :- {foo:bar(..., ...)}, ...

The downside of this alternative is that it hides the dependency on the
module library from the reflection API and thus from the developer tools.

.. _predicates_prolog_multifile:

Defining Prolog multifile predicates
------------------------------------

Some Prolog module libraries, e.g. constraint packages, expect clauses
for some library predicates to be defined in other modules. This is
accomplished by declaring the library predicate *multifile* and by
explicitly prefixing predicate clause heads with the library module
identifier. For example:

::

   :- multifile(clpfd:run_propagator/2).
   clpfd:run_propagator(..., ...) :-
       ...

Logtalk supports the definition of Prolog module multifile predicates in
objects and categories. While the clause head is compiled as-is, the clause
body is compiled in the same way as a regular object or category predicate,
thus allowing calls to local object or category predicates. For example:

::

   :- object(...).

       :- multifile(clpfd:run_propagator/2).
       clpfd:run_propagator(..., ...) :-
           % calls to local object predicates
           ...

   :- end_object.

The Logtalk compiler will print a warning if the ``multifile/1``
directive is missing. These multifile predicates may also be declared
dynamic using the same ``Module:Name/Arity`` notation.

.. _predicates_prolog_dynamic:

Asserting and retracting Prolog predicates
------------------------------------------

To assert and retract clauses for Prolog dynamic predicates, we can use an
explicitly qualified module argument. For example:

::

   :- object(...).

       :- dynamic(m:bar/1).

       foo(X) :-
           retractall(m:bar(_)),
           assertz(m:bar(X)),
           ...

   :- end_object.

In alternative, we can use :ref:`directives_use_module_2`
directives to declare the module predicates. For example:

::

   :- object(...).

       :- use_module(m, [bar/1]).
       :- dynamic(m:bar/1).

       foo(X) :-
           % retract and assert bar/1 clauses in module m
           retractall(bar(_)),
           assertz(bar(X)),
           ...

   :- end_object.

When the Prolog dynamic predicates are defined in ``user``, the recommended
and most portable practice (as not all backends support a module system) is
to use a :ref:`directives_uses_2` directive:

::

   :- object(...).

       :- uses(user, [bar/1]).
       :- dynamic(user::bar/1).

       foo(X) :-
           % retract and assert bar/1 clauses in user
           retractall(bar(_)),
           assertz(bar(X)),
           ...

   :- end_object.

Note that in the alternatives using ``uses/2`` or ``use_module/2`` directives,
the argument of the database handling predicates must be know at compile time.
If that is not the case, you must use instead either an explicitly-qualified
argument or the :ref:`control_external_call_1` control construct. For example:

::

   :- object(...).

       add(X) :-
           % assert clause X in module m
           assertz(m:X),
           ...

       remove(Y) :-
           % retract all clauses in user whose head unifies with Y
           {retractall(Y)},
           ...

   :- end_object.
