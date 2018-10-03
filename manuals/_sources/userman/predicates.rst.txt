
.. _predicates_predicates:

==========
Predicates
==========

Predicate directives and clauses can be encapsulated inside objects and
categories. Protocols can only contain predicate directives. From the
point-of-view of an object-oriented language, predicates allows both
object state and object behavior to be represented. Mutable object state
can be represented using dynamic object predicates.

.. _predicates_reserved:

Reserved predicate names
------------------------

For performance reasons, a few predicates have a fixed interpretation.
These predicates are declared in the built-protocols. They are:
:ref:`methods_goal_expansion_2` and :ref:`methods_term_expansion_2`,
declared in the ``expanding`` protocol; :ref:`methods_before_3` and
:ref:`methods_after_3`, declared in the ``monitoring`` protocol; and
:ref:`methods_forward_1`, declared in the ``forwarding`` protocol.
By default, the compiler prints a warning when
a definition for one of these predicates is found but the reference to
the corresponding built-in protocol is missing.

.. _predicates_declaring:

Declaring predicates
--------------------

All object (or category) predicates that we want to access from other
objects (or categories) must be explicitly declared. A predicate
declaration must contain, at least, a *scope* directive. Other
directives may be used to document the predicate or to ensure proper
compilation of the predicate definitions.

.. _predicates_scope:

Scope directives
~~~~~~~~~~~~~~~~

A predicate scope directive specifies *from where* the predicate can be
called, i.e. its *visibility*. Predicates can be *public*, *protected*,
*private*, or *local*. Public predicates can be called from any object.
Protected predicates can only be called from the container object or
from a container descendant. Private predicates can only be called from
the container object. Local predicates, like private predicates, can
only be called from the container object (or category) but they are
*invisible* to the reflection built-in methods (:ref:`methods_current_op_3`,
:ref:`methods_current_predicate_1`, and :ref:`methods_predicate_property_2`)
and to the message error handling mechanisms (i.e. sending a message corresponding
to a local predicate results in a ``predicate_declaration`` existence
error instead of a scope error).

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
four possible values are described in [ISO95]_). The remaining two can be
found in use in some Prolog systems.

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
   Argument must be unbound.

These four mode atoms are also declared as prefix operators by the
Logtalk compiler. This makes it possible to include type information for
each argument like in the example above. Some of the possible type
values are: ``event``, ``object``, ``category``, ``protocol``,
``callable``, ``term``, ``nonvar``, ``var``, ``atomic``, ``atom``,
``number``, ``integer``, ``float``, ``compound``, and ``list``. The
first four are Logtalk specific. The remaining are common Prolog types.
We can also use our own types that can be either atoms or ground
compound terms.

The second directive argument documents the number of proofs (not
necessarily distinct solutions) for the specified mode. Note that
different modes for the same predicate often have different determinism.
The possible values are:

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
   Predicate will throw an error (see below).

Mode declarations can also be used to document that some call modes will
throw an error. For instance, regarding the ``arg/3`` ISO Prolog
built-in predicate, we may write:

::

   :- mode(arg(-, -, +), error).

Note that most predicates have more than one valid mode implying several
mode directives. For example, to document the possible use modes of the
``atom_concat/3`` ISO built-in predicate we would write:

::

   :- mode(atom_concat(?atom, ?atom, +atom), one_or_more).
   :- mode(atom_concat(+atom, +atom, -atom), zero_or_one).

Some old Prolog compilers supported some sort of mode directives to
improve performance. To the best of my knowledge, there is no modern
Prolog compiler supporting this kind of directive. The current version
of the Logtalk compiler just parses and than discards this directive
(however, see the description on :ref:`synchronized
predicates <threads_synchronized_predicates>` in the
:ref:`multi-threading programming <threads_threads>` section). Nevertheless,
the use of mode directives is a good starting point for documenting your
predicates.

.. _predicates_meta:

Meta-predicate directive
~~~~~~~~~~~~~~~~~~~~~~~~

Some predicates may have arguments that will be called as goals or
closures that will be used for constructing goals. To ensure that these
goals will be executed in the correct context (i.e. in the *calling
context*, not in the meta-predicate *definition context*) we need to use
the :ref:`directives_meta_predicate_1` directive. For example:

::

   :- meta_predicate(findall(*, 0, *)).

The meta-predicate mode arguments in this directive have the following
meaning:

``0``
   Meta-argument that will be called as a goal.
``N``
   Meta-argument that will be a closure used to construct a call by
   appending ``N`` arguments. The value of ``N`` must be a non-negative
   integer.
``::``
   Argument that is context-aware but that will not be called as a goal.
``^``
   Goal that may be existentially quantified (``Vars^Goal``).
``*``
   Normal argument.

The following meta-predicate mode arguments are for use only when
writing backend Prolog adapter files to deal with proprietary built-in
meta-predicates and meta-directives:

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

As each Logtalk entity is compiled independently from other entities,
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

Dynamic predicates can be used to represent persistant mutable object
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

When the same operators are used on several entities within the same
source file, the corresponding directives must appear before any entity
that uses them. However, this results in a global scope for the
operators. If you prefer the operators to be local to the source file,
just *undefine* them at the end of the file. For example:

::

   % before any entity that uses the operator
   :- op(400, xfx, results).

   ...

   % after all entities that used the operator
   :- op(0, xfx, results).

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
simplify the code above. The usage template for this directive is:

::

   :- uses(Object, [Functor1/Arity1, Functor2/Arity2, ...]).

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
the declaration of predicate aliases using the notation
``Predicate as Alias`` (or the alternative notation
``Predicate::Alias``). For example:

::

   :- uses(btrees, [new/1 as new_btree/1]).
   :- uses(queues, [new/1 as new_queue/1]).

You may use this extended version for solving conflicts between
predicates declared on several ``uses/2`` directives or just for giving
new names to the predicates that will be more meaningful on their using
context.

The ``uses/2`` directive allows simpler predicate definitions as long as
there are no conflicts between the predicates declared in the directive
and the predicates defined in the object (or category) containing the
directive. A predicate (or its alias if defined) cannot be listed in
more than one ``uses/2`` directive. In addition, a ``uses/2`` directive
cannot list a predicate (or its alias if defined) which is defined in
the object (or category) containing the directive. Any conflicts are
reported by Logtalk as compilation errors.

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
inherited predicates have the same functor and arity. We may want to
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
           ::set_member(X, L).

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
Support for this directive have been added to Logtalk primarily to
support migration of Prolog module code. Spreading clauses for a
predicate among several Logtalk entities can be handy in some cases but
can also make your code difficult to understand. Logtalk precludes using
a multifile predicate for breaking object encapsulation by checking that
the object (or category) declaring the predicate (using a scope
directive) defines it also as multifile. This entity is said to contain
the *primary declaration* for the multifile predicate. In addition, note
that the ``multifile/1`` directive is mandatory when defining multifile
predicates.

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

Entities containing primary multifile predicate declarations must always
be compiled before entities defining clauses for those multifile
predicates. The Logtalk compiler will print a warning if the scope
directive is missing.

Multifile predicates may also be declared dynamic using the same
``Entity::Name/Arity`` notation (multifile predicates are static by
default).

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

Logtalk support for coinductive predicates is experimental and requires
a back-end Prolog compiler with minimal support for cyclic terms.

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
and the scope directives, what we have written is plain Prolog. Calls in
a predicate definition body default to the local predicates, unless we
use the message sending operators or the external call operator. This
enables easy conversion from Prolog code to Logtalk objects: we just
need to add the necessary encapsulation and scope directives to the old
code.

.. _predicates_categories:

Category predicates
~~~~~~~~~~~~~~~~~~~

Because a category can be imported by multiple objects, dynamic private
predicates must be called either in the context of *self*, using the
*message to self* control structure, :ref:`control_send_to_self_1`, or
in the context of *this* (i.e. in the context of the object importing the
category). For example, if we want to define a category implementing variables
using destructive assignment where the variable values are stored in
*self* we could write:

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

Meta-predicates may be defined inside objects (and categories) as any
other predicate. A meta-predicate is declared using the
``meta_predicate/1`` directive as described earlier on this section.
When defining a meta-predicate, the arguments in the clause heads
corresponding to the meta-arguments must be variables. All
meta-arguments are called in the context of the entity calling the
meta-predicate.

Some meta-predicates have meta-arguments which are not goals but
closures. Logtalk supports the definition of meta-predicates that are
called with closures instead of goals as long as the definition uses the
Logtalk built-in predicate :ref:`methods_call_N` to call the closure with
the additional arguments. For example:

::

   :- public(all_true/2).
   :- meta_predicate(all_true(1, *)).

   all_true(_, []).
   all_true(Closure, [Arg| Args]) :-
       call(Closure, Arg),
       all_true(Closure, Args).

Note that in this case the meta-predicate directive specifies that the
closure will be extended with exactly one extra argument.

When calling a meta-predicate, a closure can correspond to a
user-defined predicate, a built-in predicate, a lambda expression, or a
control construct.

.. _predicates_lambdas:

Lambda expressions
~~~~~~~~~~~~~~~~~~

The use of `lambda
expressions <https://en.wikipedia.org/wiki/Lambda_calculus>`_ as
meta-predicate goal and closure arguments often saves writing auxiliary
predicates for the sole purpose of calling the meta-predicates. A simple
example of a lambda expression is:

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
GNU Prolog as the back-end compiler, we can write:

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
queries as in the following example using SWI-Prolog as the back-end
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
parsing, error checking, and compilation of lambda expressions. The full
specification of the lambda expression syntax can be found in the
:ref:`grammar_lambdas` section of the language grammar.

The compiler checks whenever possible that all variables in a lambda
expression are either classified as free variables or as lambda
parameters. The use of non-classified variables in a lambda expression
should be regarded as a programming error. Unfortunately, the dynamic
features of the language and lack of sufficient information at compile
time may prevent the compiler of checking all uses of lambda
expressions. The compiler also checks if a variable is classified as
both a free variable and a lambda parameter. An optimizing
meta-predicate and lambda expression compiler, based on the
term-expansion mechanism, is provided for practical performance.

.. _predicates_dcgs:

Definite clause grammar rules
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
for processing the subjacent lists of tokens.

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

Along with the message sending operators (``::/1``, ``::/2``, and
``^^/1``), we may also use other control constructs such as ``\+/1``,
``!/0``, ``;/2``, ``->/2``, and ``{}/1`` in the body of a grammar. In
addition, grammar rules may contain meta-calls (a variable taking the
place of a non-terminal), which are translated to calls of the built-in
method ``phrase/3``.

You may have noticed that Logtalk defines :ref:`control_external_call_1`
as a control construct for bypassing the compiler when compiling a clause body goal.
As exemplified above, this is the same control construct that is used in
grammar rules for bypassing the expansion of rule body goals when a rule
is converted into a clause. Both control constructs can be combined in
order to call a goal from a grammar rule body, while bypassing at the
same time the Logtalk compiler. Consider the following example:

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

Built-in object predicates (methods)
------------------------------------

Logtalk defines a set of built-in object predicates or methods to access
message execution context, to find sets of solutions, to inspect
objects, for database handling, for term and goal expansion, and for
printing messages. Similar to Prolog built-in predicates, these built-in
methods should not be redefined.

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
           write('Calling predicate definition in '), writeq(This), nl,
           self(Self),
           write('to answer a message received by '), writeq(Self), nl,
           sender(Sender),
           write('that was sent by '), writeq(Sender), nl, nl.

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

For parametric objects and categories, the method
:ref:`methods_parameter_2` enables us to
retrieve current parameter values (see the section on `parametric
objects <objects.html#objects_parametric>`__ for a detailed
description). For example:

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

Logtalk provides a set of built-in methods for object database handling
similar to the usual database Prolog predicates:
:ref:`methods_abolish_1`,
:ref:`methods_asserta_1`,
:ref:`methods_assertz_1`,
:ref:`methods_clause_2`,
:ref:`methods_retract_1`, and
:ref:`methods_retractall_1`. These
methods always operate on the database of the object receiving the
corresponding message.

When working with dynamic grammar rule non-terminals, you may use the
built-in method :ref:`methods_expand_term_2` convert a
grammar rule into a clause that can than be used with the database
methods.

.. _predicates_metacalls:

Meta-call methods
~~~~~~~~~~~~~~~~~

Logtalk supports the generalized :ref:`methods_call_N` meta-predicate. This
built-in private meta-predicate must be used in the implementation of
meta-predicates which work with closures instead of goals. In addition,
Logtalk supports the built-in private meta-predicates
:ref:`methods_ignore_1`, :ref:`methods_once_1`, and
:ref:`methods_not_1`. These methods cannot be used as messages to objects.

.. _predicates_solutions:

All solutions methods
~~~~~~~~~~~~~~~~~~~~~

The usual all solutions meta-predicates are built-in private methods in
Logtalk: :ref:`methods_bagof_3`, :ref:`methods_findall_3`, :ref:`methods_findall_4`, and
:ref:`methods_setof_3`. There is also a :ref:`methods_forall_2` method that
implements generate and test loops. These methods cannot be used as
messages to objects.

.. _predicates_reflection:

Reflection methods
~~~~~~~~~~~~~~~~~~

Logtalk provides a comprehensive set of built-in predicates and built-in
methods for querying about entities and predicates. Some of information,
however, requires that the source files are compiled with the
``source_data`` flag turned on.

The reflection API supports two different views on entities and their
contents, which we may call the *transparent box view* and the *black
box view*. In the transparent box view, we look into an entity
disregarding how it will be used and returning all information available
on it, including predicate declarations and predicate definitions. This
view is supported by the entity property built-in predicates. In the
black box view, we look into an entity from an usage point-of-view using
built-in methods for inspecting object operators and predicates that are
within scope from where we are making the call:
:ref:`methods_current_op_3`, which returns operator specifications,
:ref:`methods_predicate_property_2`,
which returns predicate properties, and
:ref:`methods_current_predicate_1`,
which enables us to query about predicate definitions. See below for a
more detailed description of these methods.

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
The ``call//1-N`` non-terminals takes a closure (which can be a lambda
expression) plus zero or more additional arguments and are processed by
appending the input list of tokens and the list of remaining tokens to
the arguments.

.. _predicates_expanding:

Term and goal expansion methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk supports a :ref:`methods_expand_term_2` built-in
method for expanding a term into another term or a list of terms. This
method is mainly used to translate grammar rules into Prolog clauses. It
can be customized, e.g. for bypassing the default Logtalk grammar rule
translator, by defining clauses for the :ref:`methods_term_expansion_2` hook
predicate. Logtalk also supports a :ref:`methods_expand_goal_2` built-in
method for expanding a goal. This method can also be customized by
defining clauses for the :ref:`methods_goal_expansion_2` hook
predicate.

Term and goal expansion may be performed either by calling the
``expand_term/2`` and ``expand_goal/2`` built-in methods explicitly or
by using *hook objects*. An hook object is simply an object defining
clauses for the term- and goal-expansion hook predicates. To compile a
source file using a hook object for expanding its terms and goals, you
can use the :ref:`hook/1 <programming_flags>` compiler
flag in the second argument of the :ref:`predicates_logtalk_compile_2`
or :ref:`predicates_logtalk_load_2` built-in predicates. In alternative,
you can use a :ref:`directives_set_logtalk_flag_2`
directive in the source file itself. When compiling a source file, the
compiler will first try the source file specific hook object, if
defined. If that fails, it tries the default hook object, if defined. If
that also fails, the compiler tries the Prolog dialect specific
expansion predicate definitions if defined in the adapter file.

Sometimes we have multiple hook objects that we need to use in the
compilation of a source file. The Logtalk library includes support for
two basic expansion workflows: a pipeline of hook objects, where the
expansion results from an hook object are feed to the next hook object
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

.. _predicates_messages:

Printing messages
~~~~~~~~~~~~~~~~~

Logtalk features a *structured message printing* mechanism. This
mechanism gives the programmer full control of message printing,
allowing it to filter, rewrite, or redirect any message. The origins of
the message printing mechanism that inspired the Logtalk implementation
goes back to Quintus Prolog, where it was apparently implemented by Dave
Bowen (thanks to Richard O'Keefe for the historical bits). Variations of
this mechanism can also be found currently on e.g. SICStus Prolog,
SWI-Prolog, and YAP.

Why a mechanism for printing messages? Consider the different components
in a Logtalk application development and execution. At the bottom level,
you have the Logtalk compiler and runtime. The Logtalk compiler writes
messages related to e.g. compiling and loading files, compiling
entities, compilation warnings and errors. The Logtalk runtime may write
banner messages and handles execution errors that may result in printing
human-level messages. The development environment can be console-based
or you may be using a GUI tool such as PDT. In the latter case, PDT
needs to intercept the Logtalk compiler and runtime messages to present
the relevant information using its GUI. Then you have all the other
components in a typical application. For example, your own libraries and
third-party libraries. The libraries may want to print messages on its
own, e.g. banners, debugging information, or logging information. As you
assemble all your application components, you want to have the final
word on which messages are printed, where, an in what conditions.
Uncontrolled message printing by libraries could potentially e.g.
disturb application flow, expose implementation details, spam the user
with irrelevant details, or break user interfaces.

The Logtalk message printing mechanism provides you with a set of
predicates, defined in the ``logtalk`` built-in object, and some hook
predicates. Two of the most important predicates are
:ref:`print_message(Kind, Component, Term) <methods_print_message_3>`,
which is used for printing a message, and
:ref:`message_hook(Term, Kind, Component, Tokens) <methods_message_hook_4>`,
a user-defined hook predicate used for intercepting messages. The
``Kind`` argument is used to represent the nature of the message being
printed. It can be e.g. a logging message, a warning message or an error
message, In Logtalk this argument can be either an atom, e.g. ``error``,
or a compound term, e.g. ``comment(loading)``. Using a compound term
allows easy partitioning of messages of the same kind in different
groups. The following kinds of message are recognized by default:

``banner``
   banner messages (used e.g. when loading tools or main application
   components; can be suppressed by setting the ``report`` flag to
   ``warnings`` or ``off``)
``help``
   messages printed in reply for the user asking for help (mostly for
   helping port existing Prolog code)
``information`` and ``information(Group)``
   messages printed usually in reply to a user request for information
``silent`` and ``silent(Group)``
   not printed by default (but can be intercepted using the
   ``message_hook/4`` predicate)
``comment`` and ``comment(Group)``
   useful but usually not essential messages (can be suppressed by
   setting the ``report`` flag to ``warnings`` or ``off``)
``warning`` and ``warning(Group)``
   warning messages (generated e.g. by the compiler; can be suppressed
   by turning off the ``report`` flag)
``error`` and ``error(Group)``
   error messages (generated e.g. by the compiler)

Note that you can define your own alternative message kind identifiers,
for your own components, together with suitable definitions for their
associated prefixes and output streams.

Messages are represented by atoms or compound terms, handy for
machine-processing, and converted into a list of tokens, for human
consumption. This conversion is performed using the multifile
non-terminal
:ref:`message_tokens(Term, Component) <methods_message_tokens_2>`.
A simple example is:

::

   :- multifile(logtalk::message_tokens//2).
   :- dynamic(logtalk::message_tokens//2).

   logtalk::message_tokens(loaded_settings_file(Path), core) -->
       ['Loaded settings file found on directory ~w'-[Path], nl, nl].

The ``Component`` argument is new in the Logtalk implementation and is
useful to filter messages belonging to a specific component (e.g. the
Logtalk compiler and runtime is identified by the atom ``core``) and
also to avoid conflicts when two components define the same message term
(e.g. ``banner``).

The following tokens can be used when translating a message:

``at_same_line``
   Signals a following part to a multi-part message with no line break
   in between; this token is ignored when it's not the first in the list
   of tokens
``flush``
   Flush the output stream (by calling the ``flush_output/1`` standard
   predicate)
``nl``
   Change line in the output stream
``Format-Arguments``
   ``Format`` must be an atom and ``Arguments`` must be a list of format
   arguments (the token arguments are passed to a call to the
   ``format/3`` de facto standard predicate)
``term(Term, Options)``
   ``Term`` can be any term and ``Options`` must be a list of valid
   ``write_term/3`` output options (the token arguments are passed to a
   call to the ``write_term/3`` standard predicate)
``ansi(Attributes, Format, Arguments)``
   Taken from SWI-Prolog; by default, do nothing; can be used for styled
   output
``begin(Kind, Var)``
   Taken from SWI-Prolog; by default, do nothing; can be used together
   with ``end(Var)`` to wrap a sequence of message tokens
``end(Var)``
   Taken from SWI-Prolog; by default, do nothing

There are also predicates for printing a list of tokens,
:ref:`print_message_tokens(Stream, Prefix, Tokens) <methods_print_message_tokens_3>`,
for hooking into printing an individual token,
:ref:`print_message_token(Stream, Prefix, Token, Tokens) <methods_print_message_token_4>`,
and for setting default output stream and message prefixes,
:ref:`message_prefix_stream(Kind, Component, Prefix, Stream) <methods_message_prefix_stream_4>`.
For example, the SWI-Prolog adapter file uses the
``print_message_token/4`` hook predicate to enable coloring of messages
printed on a console.

Using the message printing mechanism in your applications and libraries
is easy. Simply chose a unique component name for your application (e.g.
the name of the application itself), call ``logtalk::print_message/3``
for every message that you may want to print, and define default
translations for your message terms using the multifile non-terminal
``logtalk::message_tokens/2``. For a full programming example, see e.g.
the ``lgtunit`` tool source code.

.. _predicates_questions:

Asking questions
~~~~~~~~~~~~~~~~

Logtalk features a *structured question asking* mechanism that
complements the message printing mechanism. This feature provides an
abstraction for the common task of asking a user a question and reading
back its reply. By default, this mechanism writes the question, writes a
prompt, and reads the answer from the current user input and output
streams but allows both steps to be intercepted, filtered, rewritten,
and redirected. Two typical examples are using a GUI dialog for asking
questions and automatically providing answers to specific questions.

The question asking mechanism works in tandem with the message printing
mechanism, using it to print the question text and a prompt. It provides
a asking predicate and a hook predicate, both declared and defined in
the ``logtalk`` built-in object. The asking predicate,
:ref:`ask_question(Kind, Component, Question, Check, Answer) <methods_ask_question_5>`,
is used for ask a question and read the answer. The hook predicate,
:ref:`question_hook(Question, Kind, Component, Tokens, Check, Answer) <methods_question_hook_6>`,
is used for intercepting questions. The ``Kind`` argument is used to
represent the nature of the question being asked. Its default value is
``question`` but it can be any atom or compound term, e.g.
``question(parameters)``. Using a compound term allows easy partitioning
of messages of the same kind in different groups. The ``Check`` argument
is a closure that is converted into a checking goal taking as argument
the user answer. The ``ask_question/5`` implements a read loop that
terminates when this checking predicate is true. The question itself is
a term that is translated into printing tokens using the
``message_tokens/2`` multifile predicate described above.

There is also a user-defined multifile predicate for setting default
prompt and input streams,
``question_prompt_stream(Kind, Component, Prompt, Stream)`` :ref:`methods_question_prompt_stream_4`.

An usage example of this mechanism can be found in the ``debugger`` tool
where it's used to abstract the user interaction when tracing a goal
execution in debug mode.

.. _predicates_properties:

Predicate properties
--------------------

We can find the properties of visible predicates by calling the
:ref:`methods_predicate_property_2` built-in method. For example:

.. code-block:: text

   | ?- bar::predicate_property(foo(_), Property).

Note that this method respects the predicate's scope declarations. For
instance, the above call will only return properties for public
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
   in several entities)
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
   per the closed-world assumption)
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
``source_data`` flag turned on:

``inline``
   The predicate definition is inlined
``auxiliary``
   The predicate is not user-defined but rather automatically generated
   by the compiler or the term-expansion mechanism
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
   The predicate alias is declared in the specified entity in a source
   file at the specified line (if applicable)

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
:ref:`methods_current_predicate_1` built-in method. This method respects
the predicate's scope declarations. For instance, the following call:

.. code-block:: text

   | ?- some_object::current_predicate(Name/Arity).

will only return user predicates that are declared public. The predicate
property ``non_terminal/1`` may be used to retrieve all grammar rule
non-terminals declared for an object. For example:

::

   current_non_terminal(Object, NonTerminal//Args) :-
       Object::current_predicate(Name/Arity),
       functor(Predicate, Functor, Arity),
       Object::predicate_property(Predicate, non_terminal(NonTerminal//Args)).

Usually, the non-terminal and the corresponding predicate share the same
functor but users should not rely on this always being true.

.. _predicates_prolog:

Calling Prolog built-in predicates
----------------------------------

In predicate definitions, predicate calls which are not prefixed with a
message sending operator (either ``::`` or ``^^``), are compiled to
either calls to local predicates or as calls to Logtalk/Prolog built-in
predicates. A predicate call is compiled as a call to a local predicate
if the object (or category) contains a scope directive, a definition for
the called predicate, or a dynamic declaration for it. When the object
(or category) does not contain either a definition of the called
predicate or a corresponding dynamic declaration, Logtalk tests if the
call corresponds to a Logtalk or Prolog built-in predicate. Calling a
predicate which is neither a local predicate nor a Logtalk/Prolog
built-in predicate results in a compile time warning. This means that,
in the following example:

::

   foo :-
       ...,
       write(bar),
       ...

the call to the predicate ``write/1`` will be compiled as a call to the
corresponding Prolog built-in predicate unless the object (or category)
encapsulating the above definition also contains a predicate named
``write/1`` or a dynamic declaration for the predicate.

When calling non-standard Prolog built-in predicates or using
non-standard Prolog arithmetic functions, you may run into portability
problems while trying your applications with different back-end Prolog
compilers (non-standard predicates and non-standard arithmetic functions
are often specific to a Prolog compiler). You may use the Logtalk
compiler flag ``portability/1`` at :ref:`programming_flags`
to help check for problematic calls in your code.

.. _predicates_prolog_meta:

Calling Prolog non-standard meta-predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prolog built-in meta-predicates may only be called locally within
objects or categories, i.e. they cannot be used as messages. Compiling
calls to non-standard, Prolog built-in meta-predicates can be tricky,
however, as there is no standard way of checking if a built-in predicate
is also a meta-predicate and finding out which are its meta-arguments.
But Logtalk supports override the original meta-predicate template if
not programmatically available or usable. For example, assume a
``det_call/1`` Prolog built-in meta-predicate that takes a goal as
argument. We can add to the object (or category) calling it the
directive:

::

   :- meta_predicate(user:det_call(0)).

Another solution is to explicitly declare all non-standard Prolog
meta-predicates in the corresponding adapter file using the internal
predicate ``'$lgt_prolog_meta_predicate'/3``. For example:

::

   '$lgt_prolog_meta_predicate'(det_call(_), det_call(0), predicate).

The third argument can be either the atom ``predicate`` or the atom
``control_construct``, a distinction that is useful when compiling in
debug mode.

.. _predicates_prolog_user:

Calling Prolog user-defined predicates
--------------------------------------

Prolog user-defined predicates can be called from within objects or
categories by using the :ref:`control_external_call_1` compiler bypass
control construct. For example:

::

   foo :-
       ...,
       {bar},
       ...

In alternative, you can also use the :ref:`directives_uses_2` directive
and write:

::

   :- uses(user, [bar/0]).

   foo :-
       ...,
       bar,
       ...

Note that ``user`` is a pseudo-object in Logtalk containing all
predicate definitions that are not encapsulated (either in a Logtalk
entity or a Prolog module).

.. _predicates_prolog_module:

Calling Prolog module predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To call Prolog module predicates from within objects or categories you
can use simply write:

::

   foo :-
       ...,
       module:bar,
       ...

You can also use in alternative the :ref:`directives_use_module_2` directive:

::

   :- use_module(module, [bar/0]).

   foo :-
       ...,
       bar,
       ...

Note that the first argument of the ``use_module/2``, when used within
an object or a category, is a module name, not a file name. The actual
module code should be loaded prior to compilation of Logtalk that uses
it. In particular, programmers should not expect that the module be
auto-loaded (when using back-end Prolog compilers supporting an
autoloading mechanism).

When the module predicate is a meta-predicate but for some reason you
don't want its calls to be compiled as such, you can use the
:ref:`control_external_call_1` compiler bypass
control construct as before:

::

   foo :-
       ...,
       {module:bar},
       ...

This workaround is sometimes necessary when calling module
meta-predicates whose meta-predicate templates are ambiguous and cannot
be processed by the Logtalk compiler (note, however, that it's often
possible to specify an overriding meta-predicate directive within the
object or category making the call as explained above).
