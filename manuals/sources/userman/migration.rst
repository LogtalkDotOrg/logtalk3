..
   This file is part of Logtalk <https://logtalk.org/>  
   Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
   SPDX-License-Identifier: Apache-2.0

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

Prolog integration and migration
================================

An application may include plain Prolog files, Prolog modules, and
Logtalk objects. This is a perfectly valid way of developing a complex
application and, in some cases, it might be the most appropriated
solution. Modules may be used for legacy code or when a simple
encapsulation mechanism is adequate. Logtalk objects may be used when
more powerful encapsulation, abstraction, and reuse features are
required.

This section provides tips for integrating and migrating plain Prolog code
and Prolog module code to Logtalk. Step-by-step instructions are provided
for encapsulating plain Prolog code in objects, converting Prolog modules
into objects, and compiling and reusing Prolog modules as objects from
inside Logtalk. An interesting application of the techniques described
in this section is a solution for running a Prolog application which uses
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
parametric objects, and categories. It also allows you to take full
advantage of Logtalk developer tools for improved productivity.

Converting a Prolog module into an object is simplified when the directives
used in the module are supported by Logtalk (see the listing in the next
section). Assuming that this is the case, apply the following steps:

#. Convert the module ``module/1`` directive into an
   :ref:`object/1 <directives_object_1_5>` opening object directive,
   using the module name as the object name. For ``module/2`` directives
   apply the same conversion and convert the list of exported predicates
   into :ref:`directives_public_1` predicate directives. Add a closing
   object directive, :ref:`directives_end_object_0`, at the end of the
   source code.
#. Convert any ``export/1`` directives into ``public/1`` predicate
   directives.
#. Convert any ``use_module/1`` directives for modules that will not be
   converted to objects into ``use_module/2`` directives (see next section),
   replacing the file spec in the first argument with the module name.
#. Convert any ``use_module/1-2`` directives referencing other modules
   also being converted to objects into Logtalk :ref:`directives_uses_2`
   directives.
#. Convert each ``reexport/1`` directive into a :ref:`directives_uses_2`
   directive and ``public/1`` predicate directives (see next section).
#. Convert any ``meta_predicate/1`` directives into Logtalk
   :ref:`directives_meta_predicate_1`
   directives by replacing the module meta-argument indicator, ``:``,
   with the Logtalk meta-argument indicator, ``0``. Closures must be
   represented using an integer denoting the number of additional
   arguments that will be appended to construct a goal. Arguments which
   are not meta-arguments are represented by the ``*`` character.
#. Convert any explicit qualified calls to module predicates to messages
   by replacing the ``(:)/2`` operator with the
   :ref:`control_send_to_object_2` message
   sending operator when the referenced modules are also being
   converted into objects. Calls in the pseudo-module ``user`` can
   be encapsulated using the :ref:`control_external_call_1` Logtalk
   external call control construct. You can also use instead a
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
   predicates, replace the ``(:)/2`` functor by ``(::)/2`` in the
   ``multifile/1`` directives and in the clause heads for all modules
   defining the multifile predicates that are also being converted into
   objects; if that is not the case, just keep the ``multifile/1``
   directives and the clause heads as-is).
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

A possible alternative to port Prolog code to Logtalk is to compile the Prolog
source files using the ``logtalk_load/1-2`` and ``logtalk_compile/1-2``
predicates. The Logtalk compiler provides partial support for compiling Prolog
modules as Logtalk objects. This support may allow using modules from a backend
Prolog system in a different backend Prolog system although its main purpose is
to help in porting existing Prolog code to Logtalk in order to benefit from its
extended language features and its developer tools. Why partial support?
Although there is a ISO Prolog standard for modules, it is (rightfully)
ignored by most implementers and vendors (due to its flaws and deviation
from common practice). In addition, there is no de facto standard for module
systems, despite otherwise frequent misleading claims. Key system differences
include the set of implemented module directives, the directive semantics, the
handling of operators, the locality of flags, and on the integration of
term-expansion mechanisms (when provided). Another potential issue is that,
when compiling modules as objects, Logtalk assumes that any referenced module
(e.g. using ``use_module/1-2`` directives) is also being compiled as an
object. If that's not the case, the compiled module calls being compiled as
message sending goals will still work for normal predicates but will not
work for meta-predicates called using implicit module qualification. The
reason is that, unlike in Logtalk, calls to implicitly and explicitly
qualified module meta-predicates have different semantics. Follows a
discussion of other limitations of this approach that you should be aware.

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
   non-terminal indicators, and operator declarations. Predicate aliases
   can be declared using the notation ``Name/Arity as Alias/Arity`` or,
   in alternative, the notation ``Name/Arity:Alias/Arity``. Similar for
   non-terminal aliases.
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
   operator declarations. Predicate aliases can be declared using the
   notation ``Name/Arity as Alias/Arity`` or, in alternative, the notation
   ``Name/Arity:Alias/Arity``. Similar for non-terminal aliases.
``meta_predicate/1``
   Module meta-predicates become object meta-predicates. Only predicate
   arguments marked as goals or :term:`closures <closure>` (using an integer)
   are interpreted as meta-arguments. In addition, Prolog module
   meta-predicates and Logtalk meta-predicates don't share the same
   explicit-qualification calling semantics: in Logtalk, meta-arguments
   are always called in the context of the *sender*. Logtalk expects
   ``meta-predicate/1`` directives for all meta-predicates as it is not
   based on the predicate-prefixing mechanism common to most Prolog
   module systems.

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

.. _migration_unsupported_module_directives:

Unsupported module directives
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``reexport/1`` and ``use_module/1`` directives are not directly
supported by the Logtalk compiler. But most Prolog adapter files provide
support for compiling these directives using Logtalk first stage of
its :ref:`term-expansion mechanism <expansion_expansion>`. Nevertheless,
these directives can be converted, respectively, into a sequence of
``:- use_module/2`` and ``export/1`` directives and ``use_module/2``
directives by finding which predicates exported by the
specified modules are reexported or imported into the module containing
the directive. For ``use_module/1`` directives, finding the names of the
imported predicates that are actually used is easy. First, comment out the 
and compile the file (making sure that the
:ref:`unknown_predicates <flag_unknown_predicates>` compiler flag is set
to ``warning``). Logtalk will print a warning with a list of predicates
that are called but never defined. Second, use these list to replace the
``use_module/1`` directives by ``use_module/2`` directives. You should
then be able to compile the modified Prolog module as an object.

.. _migration_module_expansions:

Modules using a term-expansion mechanism
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Although Logtalk supports
:ref:`term and goal expansion mechanisms <expansion_expansion>`, the usage
semantics are different from similar mechanisms found in some Prolog
compilers. In particular, Logtalk does not support defining term and
goal expansions clauses in a source file for expanding the source file
itself. Logtalk forces a clean separation between expansions clauses and
the source files that will be subject to source-to-source expansions by
using :term:`hook objects <hook object>`. But hook objects also provide
a working solution here when the expansion code is separated from the
code to be expanded. Logtalk supports using a module as a hook object
as long as its name doesn't coincide with the name of an object and
that the module uses ``term_expansion/2`` and ``goal_expansion/2``
predicates. Assuming that's the case, before attempting to compile
the modules as objects, the default hook object is set to the module
containing the expansion code. For example, if the expansions stored
in a ``system`` module:

.. code-block:: text

   | ?- set_logtalk_flag(hook, system).
   ...

This, however, may not be enough as some expansions may stored in more
than one module. A common example is to use a module named ``prolog``.
It is also common to store the expansions in ``user``. The Logtalk library
provides a solution for these scenarios. Using the ``hook_flows`` library
we can select multiple hook objects or hook modules. For example,
assuming expansions stored on both ``user`` and ``system`` modules:

.. code-block:: text

   | ?- logtalk_load(hook_flows(loader)).
   ...

   | ?- set_logtalk_flag(hook, hook_set([user, system])).
   ...

After these queries, we can try to compile the modules and look for
other porting or portability issues. A well know issue is Prolog module
term-expansions calling predicates such as ``prolog_load_context/2``,
which will always fail when it's the Logtalk compiler instead of the
Prolog compiler loading a source file. In some of these cases, it may
be possible to rewrite the expansion rules to use the
:ref:`predicates_logtalk_load_context_2` predicate instead.

.. _migration_file_search_paths:

File search paths
~~~~~~~~~~~~~~~~~

Some Prolog systems provide a mechanism for defining file search paths
(this mechanism works differently from Logtalk own suporty for defining
library paths). When porting Prolog code that defines file search paths,
e.g. for finding module libraries, it often helps to load the pristine
Prolog application before attempting to compile its source files as
Logtalk source files. Depending on the Prolog backend, this may allow
the file search paths to be used when compiling modules as objects that
use file directives such as ``use_module/2``.


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
automatically translated to ``Module:Goal`` calls.

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
and :term:`predicate shorthands <predicate shorthand>` in ``use_module/2``
directives used within object and categories. For example, the ECLiPSe IC
Constraint Solvers define a ``(::)/2`` variable domain operator that clashes
with the Logtalk ``(::)/2`` message sending operator. We can solve the conflict
by writing:

::

   :- use_module(ic, [(::)/2 as ins/2]).

With this directive, calls to the ``ins/2`` predicate alias will be
automatically compiled by Logtalk to calls to the ``(::)/2`` predicate in
the ``ic`` module.

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

For more details, see the :ref:`predicates_prolog` section.

.. _migration_loading:

Loading converted Prolog applications
-------------------------------------

Logtalk strongly favors and advises users to provide a main
:ref:`loader file <programming_loaders>` for applications that explicitly
load any required libraries and the application source files. In contrast,
Prolog applications often either scatter loading of source files from multiple
files or use implicit loading of source files via ``use_module/1-2``
directives. Due to this frequent ad-hoc approach, it's common to find Prolog
applications with duplicated loading directives and are loading order ignores
the dependencies between source files. These issues are easily exposed by the
Logtalk linter when compiling Prolog files as Logtalk files. Also common are
Prolog files with multiple circular dependencies. While this should not
affect the *semantics* of the ported code, it may cause some performance
penalties as it prevents the Logtalk compiler of optimizing the message
sending goals using static-binding. It also makes the application architecture
more difficult to understand. The definition of explicit loader files
provides a good opportunity of sorting out loading order and circular
dependencies, with the linter warnings providing hints for possible code
refactoring to eliminate these issues. The :doc:`../devtools/diagrams` tool
supports directory and file loading and dependency diagrams that are also
useful in understanding applications architecture.
