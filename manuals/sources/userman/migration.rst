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


.. _migration_migration:

Prolog integration and migration guide
======================================

An application may include plain Prolog files, Prolog modules, and
Logtalk objects. This is a perfectly valid way of developing a complex
application and, in some cases, it might be the most appropriated
solution. Modules may be used for legacy code or when a simple
encapsulation mechanism is adequate. Logtalk objects may be used when
more powerful encapsulation, abstraction, and reuse features are
necessary.

Logtalk supports the compilation of source files containing
both plain Prolog and Prolog modules. This guide provides tips for
integrating and migrating plain Prolog code and Prolog module
code to Logtalk. Step-by-step instructions are provided for
encapsulating plain Prolog code in objects, converting Prolog modules
into objects, and compiling and reusing Prolog modules as objects from
inside Logtalk. An interesting application of the techniques described
in this guide is a solution for running a Prolog application which uses
modules on a Prolog compiler with no module system. The ``wrapper`` tool
can be used to help in migrating Prolog code.

.. _migration_hybrid:

Source files with both Prolog code and Logtalk code
---------------------------------------------------

Logtalk source files may contain plain Prolog code intermixed with
Logtalk code. The Logtalk compiler simply copies the plain Prolog code
as-is to the generated Prolog file. With Prolog modules, it is assumed
that the module code starts with a ``module/1-2`` directive and ends at
the end of the file. There is no module ending directive which would
allowed us to define more than one module per file. In fact, most if not
all Prolog module systems always define a single module per file. Some
of them mandate that the ``module/1-2`` directive be the first term on a
source file. As such, when the Logtalk compiler finds a ``module/1-2``
directive, it assumes that all code that follows until the end of the
file belongs to the module.

.. _migration_encapsulating:

Encapsulating plain Prolog code in objects
------------------------------------------

Most applications consist of several plain Prolog source files, each one
defining a few top-level predicates and auxiliary predicates that are
not meant to be directly called by the user. Encapsulating plain Prolog
code in objects allows us to make clear the different roles of each
predicate, to hide implementation details, to prevent auxiliary
predicates from being called outside the object, and to take advantage
of Logtalk advanced code encapsulating and reusing features.

Encapsulating Prolog code using Logtalk objects is simple. First, for
each source file, add an opening object directive,
:ref:`directives_object_1_5`, to the
beginning of the file and an ending object directive,
:ref:`directives_end_object_0`, to end of
the file. Choose an object name that reflects the purpose of source file
code (this is a good opportunity for code refactoring if necessary).
Second, add :ref:`directives_public_1` predicate directives for the
top-level predicates that are used directly by the user or called from
other source files. Third, we need to be able to call from inside an object
predicates defined in other source files/objects. The easiest solution,
which has the advantage of not requiring any changes to the predicate
definitions, is to use the :ref:`directives_uses_2` directive. If your
Prolog compiler supports cross-referencing tools, you may use them to
help you make sure that all calls to predicates on other source
files/objects are listed in the :ref:`directives_uses_2` directives.
The Logtalk ``wrapper`` tool can also help in detecting cross predicate
calls. Compiling the resulting objects with the Logtalk
:ref:`unknown_predicates <flag_unknown_predicates>` and
:ref:`portability <flag_portability>` flags set to ``warning`` will
help you identify calls to predicates defined on other converted source
files and possible portability issues.

.. _migration_multifile:

Prolog multifile predicates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Prolog *multifile* predicates are used when clauses for the same
predicate are spread among several source files. When encapsulating
plain Prolog code that uses multifile predicates, is often the case that
the clauses of the multifile predicates get spread between different
objects and categories but conversion is straight-forward. In the
Logtalk object (or category) holding the multifile predicate
:term:`primary declaration <primary predicate declaration>`, add a
:ref:`predicate scope directive <predicates_scope>` and a
:ref:`multifile/1 <predicates_multifile>` directive. In
all other objects (or categories) defining clauses for the multifile
predicate, add a ``multifile/1`` directive and predicate clauses using
the format:

::

   :- multifile(Entity::Name/Arity).

   Entity::Functor(...) :-
       ...

See the User Manual section on the ``multifile/1`` predicate directive
for more information. An alternative solution is to simply keep the
clauses for the multifile predicates as plain Prolog code and define, if
necessary, a parametric object to encapsulate all predicates working
with the multifile predicate clauses. For example, assume the following
``multifile/1`` directive:

::

   % city(Name, District, Population, Neighbors)
   :- multifile(city/4).

We can define a parametric object with ``city/4`` as its identifier:

::

   :- object(city(_Name, _District, _Population, _Neighbors)).

       % predicates for working with city/4 clauses

   :- end_object.

This solution is preferred when the multifile predicates are used to
represent large tables of data. See the section on :ref:`objects_parametric`
for more details.

.. _migration_converting:

Converting Prolog modules into objects
--------------------------------------

Converting Prolog modules into objects may allow an application to run
on a wider range of Prolog compilers, overcoming compatibility problems.
Some Prolog compilers don't support a module system. Among those Prolog
compilers which support a module system, the lack of standardization
leads to several issues, specially with semantics, operators, and
meta-predicates. In addition, the conversion allows you to take
advantage of Logtalk more powerful abstraction and reuse mechanisms such
as separation between interface from implementation, inheritance,
parametric objects, and categories.

Converting a Prolog module into an object is easy as long as the
directives used in the module are supported by Logtalk (see below).
Assuming that this is the case, apply the following steps:

#. Convert the module ``module/1`` directive into an opening object
   directive, :ref:`directives_object_1_5`,
   using the module name as the object name. For ``module/2`` directives
   apply the same conversion and convert the list of exported predicates
   into Logtalk :ref:`directives_public_1`
   predicate directives.
#. Add a closing object directive,
   :ref:`directives_end_object_0`, at the
   end of the module code.
#. Convert any ``export/1`` directives into ``public/1`` predicate
   directives.
#. Convert any ``use_module/1`` directives into ``use_module/2``
   directives (see next section).
#. Convert any ``use_module/2`` directives referencing other modules
   also being converted to objects into Logtalk
   :ref:`directives_uses_2` directives. If the
   referenced modules are not being converted into objects, simply keep
   the ``use_module/2`` directives unchanged.
#. Convert any ``meta_predicate/1`` directives into Logtalk
   :ref:`directives_meta_predicate_1`
   directives by replacing the module meta-argument indicator, ``:``,
   with the Logtalk meta-argument indicator, ``0``. Closures must be
   represented using an integer denoting the number of additional
   arguments that will be appended to construct a goal. Arguments which
   are not meta-arguments are represented by the ``*`` character.
#. Convert any explicit qualified calls to module predicates to messages
   by replacing the ``:/2`` operator with the
   :ref:`control_send_to_object_2` message
   sending operator, assuming that the referenced modules are also being
   converted into objects. Calls in the pseudo-module ``user`` can
   simply be encapsulated using the
   :ref:`control_external_call_1` Logtalk
   external call control construct. You can also use instead an
   :ref:`directives_uses_2` directive where the
   first argument would be the atom ``user`` and the second argument a
   list of all external predicates. This alternative has the advantage
   of not requiring changes to the code making the predicate calls.
#. If your module uses the database built-in predicates to implement
   module local mutable state using dynamic predicates, add both
   :ref:`directives_private_1` and
   :ref:`directives_dynamic_1` directives
   for each dynamic predicate.
#. If your module declares or defines clauses for multifile module
   predicates, replace the ``:/2`` functor by ``::/2`` in the
   ``multifile/1`` directives and in the clause heads (assuming that all
   modules defining the multifile predicates are converted into objects;
   if that is not the case, just keep the ``multifile/1`` directives and
   the clause heads as-is).
#. Compile the resulting objects with the Logtalk
   :ref:`unknown_predicates <flag_unknown_predicates>`, and
   :ref:`portability <flag_portability>` flags set to ``warning``
   to help you locate possible issues and calls to proprietary Prolog
   built-in predicates and to predicates defined on other converted
   modules. In order to improve code portability, check the Logtalk
   library for possible alternatives to the use of proprietary Prolog
   built-in predicates.

Before converting your modules to objects, you may try to compile them
first as objects (using the :ref:`predicates_logtalk_compile_1`
Logtalk built-in predicates) to help identify any issues that must be
dealt with when doing the conversion to objects. Note that Logtalk
supports compiling Prolog files as Logtalk source code without requiring
changes to the file name extensions.

.. _migration_compiling:

Compiling Prolog modules as objects
-----------------------------------

An alternative to convert Prolog modules into objects is to just compile
the Prolog source files using the ``logtalk_load/1-2`` and
``logtalk_compile/1-2`` predicates (set the Logtalk
:ref:`portability <flag_portability>` flag set to ``warning`` to
help you catch any unnoticed cross-module predicate calls). This allows
you to reuse existing module code as objects. This has the advantage of
requiring little if any code changes. There are, however, some
limitations that you must be aware. These limitations are a consequence
of the lack of standardization of Prolog module systems and consequent
proliferation of proprietary extensions.

.. _migration_compatibility:

Supported module directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Currently, Logtalk supports the following module directives:

``module/1``
   The module name becomes the object name.
``module/2``
   The module name becomes the object name. The exported predicates
   become public object predicates. The exported grammar rule
   non-terminals become public grammar rule non-terminals. The exported
   operators become public object operators but are not active elsewhere
   when loading the code.
``use_module/2``
   This directive is compiled as a Logtalk
   :ref:`directives_uses_2` directive in order
   to ensure correct compilation of the module predicate clauses. The
   first argument of this directive must be the module **name** (an
   atom), not a module file specification (the adapter files attempt to
   use the Prolog dialect level term-expansion mechanism to find the
   module name from the module file specification). Note that the module
   is not automatically loaded by Logtalk (as it would be when compiling
   the directive using Prolog instead of Logtalk; the programmer may
   also want the specified module to be compiled as an object). The
   second argument must be a predicate indicator (``Name/Arity``), a
   grammar rule non-terminal indicator (``Name//Arity``), a operator
   declaration, or a list of predicate indicators, grammar rule
   non-terminal indicators, and operator declarations.
``export/1``
   Exported predicates are compiled as public object predicates. The
   argument must be a predicate indicator (``Name/Arity``), a grammar
   rule non-terminal indicator (``Name//Arity``), an operator
   declaration, or a list of predicate indicators, grammar rule
   non-terminal indicators, and operator declarations.
``reexport/2``
   Reexported predicates are compiled as public object predicates. The
   first argument is the module name. The second argument must be a
   predicate indicator (``Name/Arity``), a grammar rule non-terminal
   indicator (``Name//Arity``), an operator declaration, or a list of
   predicate indicators, grammar rule non-terminal indicators, and
   operator declarations.
``meta_predicate/1``
   Module meta-predicates become object meta-predicates. Only predicate
   arguments marked as goals or closures (using an integer) are
   interpreted as meta-arguments. In addition, Prolog module
   meta-predicates and Logtalk meta-predicates don't share the same
   explicit-qualification calling semantics: in Logtalk, meta-arguments
   are always called in the context of the *sender*.

A common issue when compiling modules as objects is the use of the atoms
``dynamic``, ``discontiguous``, and ``multifile`` as operators in
directives. For better portability avoid this usage. For example, write:

::

   :- dynamic([foo/1, bar/2]).

instead of:

.. code-block:: text

   :- dynamic foo/1, bar/2.

Another common issue is missing ``meta_predicate/1``, ``dynamic/1``,
``discontiguous/1``, and ``multifile/1`` predicate directives. The Logtalk
compiler supports detection of missing directives (by setting its
:ref:`missing_directives <flag_missing_directives>` flag to ``warning``).

When compiling modules as objects, you probably don't need event support
turned on. You may use the :ref:`events <flag_events>` compiler flag to
``deny`` with the Logtalk compiling and loading built-in methods for a
small performance gain for the compiled code.

.. _migration_limitations:

Current limitations and workarounds
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``reexport/1`` and ``use_module/1`` directives are not directly
supported by the Logtalk compiler. But most Prolog adapter files provide
support for compiling these directives using Logtalk's first stage of
its :ref:`term-expansion mechanism <expansion_expansion>`. Nevertheless,
these directives can be converted, respectively, into ``reexport/2`` and
``use_module/2`` directives by finding which predicates exported by the
specified modules are reexported or imported into the module containing
the directive. Finding the names of the imported predicates that are
actually used is easy. First, comment out the ``use_module/1`` directives
and compile the file (making sure that the
:ref:`unknown_predicates <flag_unknown_predicates>` compiler flag is set
to ``warning``). Logtalk will print a warning with a list of predicates
that are called but never defined. Second, use these list to replace the
``reexport/1`` and ``use_module/1`` directives by, respectively,
``reexport/2`` and ``use_module/2`` directives. You should then be able
to compile the modified Prolog module as an object.

Although Logtalk supports
:ref:`term and goal expansion mechanisms <expansion_expansion>`, the
semantics are different from similar mechanisms found in some Prolog
compilers. In particular, Logtalk does not support defining term and
goal expansions clauses in a source file for expanding the source file
itself. Logtalk forces a clean separation between expansions clauses and
the source files that will be subject to source-to-source expansions by
using :term:`hook objects <hook object>`.

.. _migration_proprietary:

Dealing with proprietary Prolog directives and predicates
---------------------------------------------------------

Most Prolog compilers define proprietary, non-standard, directives and
predicates that may be used in both plain code and module code.
Non-standard Prolog built-in predicates are usually not problematic, as
Logtalk is usually able to identify and compile them correctly (but see
the notes on built-in meta-predicates for possible caveats). However,
Logtalk will generate compilation errors on source files containing
proprietary directives unless you first specify how the directives
should be handled. Several actions are possible on a per-directive
basis: ignoring the directive (i.e. do not copy the directive, although
a goal can be proved as a consequence), rewriting and copy the directive
to the generated Prolog files, or rewriting and recompiling the
resulting directive. To specify these actions, the adapter files contain
clauses for the ``'$lgt_prolog_term_expansion'/2`` predicate. For
example, assume that a given Prolog compiler defines a ``comment/2``
directive for predicates using the format:

::

   :- comment(foo/2, "Brief description of the predicate").

We can rewrite this predicate into a Logtalk ``info/2`` directive by
defining a suitable clause for the ``'$lgt_prolog_term_expansion'/2``
predicate:

::

   '$lgt_prolog_term_expansion'(
           comment(F/A, String),
           info(F/A, [comment is Atom])
   ) :-
       atom_codes(Atom, String).

This Logtalk feature can be used to allow compilation of legacy Prolog
code without the need of changing the sources. When used, is advisable
to set the :ref:`portability <flag_portability>` compiler flag to
``warning`` in order to more easily identify source files that are
likely non-portable across Prolog compilers.

A second example, where a proprietary Prolog directive is discarded
after triggering a side effect:

::

   '$lgt_prolog_term_expansion'(
           load_foreign_files(Files,Libs,InitRoutine),
           []
   ) :-
       load_foreign_files(Files,Libs,InitRoutine).

In this case, although the directive is not copied to the generated
Prolog file, the foreign library files are loaded as a side effect of
the Logtalk compiler calling the ``'$lgt_prolog_term_expansion'/2`` hook
predicate.

.. _migration_calling:

Calling Prolog module predicates
--------------------------------

Prolog module predicates can be called from within objects or categories
by simply using explicit module qualification, i.e. by writing
``Module:Goal`` or ``Goal@Module`` (depending on the module system).
Logtalk also supports the use of ``use_module/2`` directives in object
and categories (with the restriction that the first argument of the
directive must be the actual module name and not the module file name or
the module file path). In this case, these directives are parsed in a
similar way to Logtalk :ref:`directives_uses_2`
directives, with calls to the specified module predicates being
automatically translated to ``Module:Goal`` calls. For example, assume a
``clpfd`` Prolog module implementing a finite domain constraint solver.
You could write:

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

As a general rule, the Prolog modules should be loaded (e.g. in the
auxiliary Logtalk loader files) *before* compiling objects that make use
of module predicates. Moreover, the Logtalk compiler does not generate
code for the automatic loading of modules referenced in
``use_module/1-2`` directives. This is a consequence of the lack of
standardization of these directives, whose first argument can be a
module name, a straight file name, or a file name using some kind of
library notation, depending on the :term:`backend Prolog compiler`. Worse,
modules are sometimes defined in files with names different from the
module names requiring finding, opening, and reading the file in order
to find the actual module name.

Logtalk supports the declaration of :term:`predicate aliases <predicate alias>`
in ``use_module/2`` directives used within object and categories. For
example, the ECLiPSe IC Constraint Solvers define a ``::/2`` variable
domain operator that clashes with the Logtalk ``::/2`` message sending
operator. We can solve the conflict by writing:

::

   :- use_module(ic, [(::)/2 as ins/2]).

With this directive, calls to the ``ins/2`` predicate alias will be
automatically compiled by Logtalk to calls to the ``::/2`` predicate in
the ``ic`` module.

When calling Prolog module meta-predicates, the Logtalk compiler may
need help to understand the corresponding meta-predicate template.
Despite some recent progress in standardization of the syntax of
``meta_predicate/1`` directives and of the ``meta_predicate/1`` property
returned by the ``predicate_property/2`` reflection predicate,
portability is still a problem. Thus, Logtalk allows the original
``meta_predicate/1`` directive to be overridden with a local one that
Logtalk can make sense of. Note that the Logtalk library provides
implementations of common meta-predicates, which can be used in place of
module meta-predicates.

Logtalk allows you to send a message to a module in order to call one of
its predicates. This is usually not advised as it implies a performance
penalty when compared to just using the ``Module:Call`` notation.
Moreover, this works only if there is no object with the same name as
the module you are targeting. This feature is necessary, however, in
order to properly support compilation of modules containing
``use_module/2`` directives as objects. If the modules specified in the
``use_module/2`` directives are not compiled as objects but are instead
loaded as-is by Prolog, the exported predicates would need to be called
using the ``Module:Call`` notation but the converted module will be
calling them through message sending. Thus, this feature ensures that,
on a module compiled as an object, any predicate calling other module
predicates will work as expected either these other modules are loaded
as-is or also compiled as objects.

.. _migration_module:

Compiling Prolog module multifile predicates
--------------------------------------------

Some Prolog module libraries, e.g. constraint packages, expect clauses
for some library predicates to be defined in other modules. This is
accomplished by declaring the library predicate *multifile* and by
explicitly prefixing predicate clause heads with the library module
identifier. For example:

::

   :- multifile(clpfd:run_propagator/2).
   clpfd:run_propagator(..., ...) :-
       ...

Logtalk supports the compilation of such clauses within objects and
categories. While the clause head is compiled as-is, the clause body is
compiled in the same way as a regular object or category predicate, thus
allowing calls to local object or category predicates. For example:

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
