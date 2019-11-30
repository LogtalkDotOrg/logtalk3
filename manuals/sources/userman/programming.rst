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


.. _programming_programming:

Writing and running applications
================================

.. _programming_writing:

Writing applications
--------------------

For a successful programming in Logtalk, you need a good working
knowledge of Prolog and an understanding of the principles of
object-oriented programming. Most guidelines for writing good Prolog
code apply as well to Logtalk programming. To those guidelines, you
should add the basics of good object-oriented design.

One of the advantages of a system like Logtalk is that it enable us to
use the currently available object-oriented methodologies, tools, and
metrics [Champaux92]_ in logic programming. That said, writing applications
in Logtalk is similar to writing applications in Prolog: we define new
predicates describing what is true about our domain objects, about our
problem solution. We encapsulate our predicate directives and definitions
inside new objects, categories, and protocols that we create by hand with
a text editor or by using the Logtalk built-in predicates. Some of the
information collected during the analysis and design phases can be
integrated in the objects, categories and protocols that we define by
using the available entity and predicate documenting directives.

.. _programming_source_files:

Source files
~~~~~~~~~~~~

Logtalk source files may define any number of entities (objects,
categories, or protocols). Source files may also contain Prolog code
interleaved with Logtalk entity definitions. Plain Prolog code is usually
copied as-is to the corresponding Prolog output file (except, of course,
if subject to the :ref:`term-expansion mechanism <expansion_expansion>`).
Prolog modules are compiled as objects. The following Prolog directives are
processed when read (thus affecting the compilation of the source code that
follows): ``ensure_loaded/1``, ``use_module/1-2``, ``op/3``, and
``set_prolog_flag/2``. The ``initialization/1`` Prolog directive may be used
for defining an initialization goal to be executed when loading a source file.
Most calls to Logtalk built-in predicates from file ``initialization/1``
directives are compiled for better performance.

Logtalk source files can include the text of other files by using the
:ref:`directives_include_1` directive. Although there is also a standard
Prolog ``include/1`` directive, any occurrences of this directive in a
Logtalk source file is handled by the Logtalk compiler, not by the
:term:`backend Prolog compiler`.

Multi-pass compiler
^^^^^^^^^^^^^^^^^^^

Logtalk is currently implemented using a multi-pass compiler. In comparison,
some Prolog systems use a multi-pass compiler while others use a single-pass
compiler. While there are pros and cons with each solution, the most relevant
consequence in this context is for the content of source files. In Logtalk,
entities and predicates only become available (for the runtime system) after
the source file is fully compiled and loaded. This may prevent some compiler
optimizations, notably static binding, if some of the referred entities are
defined in the same source file. On the other hand, the order of predicate
directives and predicate definitions is irrelevant. In contrast, in a system
implemented using a single-pass compiler, the order of the source file terms
can and often is significant for proper and succesful compilation. In these
systems, predicates may become available for calling as soon as they are
compiled even if the remaining of the source file is yet to be compiled.
When writing a Logtalk source file the following advice applies:

- When practical and when performance is critical, define each entity on
  its own source file.
- Source file loading order can impact performance (e.g. if an object
  imports a category defined in a source file loaded after the object
  source file, no static binding optimizations will be possible).
- File directives that result in the compilation and loading of other
  source files (e.g. libraries) should preferably be written in the
  application loader file to ensure the availability of the entities
  they define when compiling the application source files.

Naming conventions
^^^^^^^^^^^^^^^^^^

If you prefer to define each
entity in its own source file, then it is recommended that the source
file be named after the entity identifier. For parametric objects, the
identifier arity can be appended to the identifier functor. By default,
all Logtalk source files use the extension ``.lgt`` but this is optional
and can be set in the adapter files. Intermediate Prolog source files
(generated by the Logtalk compiler) have, by default, a ``_lgt`` suffix
and a ``.pl`` extension. Again, this can be set to match the needs of a
particular Prolog compiler in the corresponding adapter file. For
example, we may define an object named ``vehicle`` and save it in a
``vehicle.lgt`` source file that will be compiled to a
``vehicle_lgt.pl`` Prolog file. If we have a ``sort(_)`` parametric
object we can save it on a ``sort_1.lgt`` source file that will be
compiled to a ``sort_1_lgt.pl`` Prolog file. This name scheme helps
avoid file name conflicts (remember that all Logtalk entities share the
same namespace). To further prevent file name conflicts, specially when
embedding applications, and depending on the
:term:`backend compiler <backend Prolog compiler>`, the names
of the intermediate Prolog files may include a directory hash.

Source file text encoding
^^^^^^^^^^^^^^^^^^^^^^^^^

The text encoding used in a source file may be declared using the
:ref:`directives_encoding_1` directive when running Logtalk with
backend Prolog compilers that support multiple encodings (check the
:ref:`encoding_directive <flag_encoding_directive>` flag in the
adapter file of your Prolog compiler).

.. _programming_portability:

Portable applications
~~~~~~~~~~~~~~~~~~~~~

Logtalk is compatible with most modern standards compliant Prolog compilers.
However, this does not necessarily imply that your Logtalk applications will
have the same level of portability. If possible, you should only use in your
applications Logtalk built-in predicates and ISO Prolog specified
built-in predicates and arithmetic functions. If you need to use
built-in predicates (or built-in arithmetic functions) that may not be
available in other Prolog compilers, you should try to encapsulate the
non-portable code in a small number of objects and provide a portable
**interface** for that code through the use of Logtalk protocols. An
example will be code that access operating-system specific features. The
Logtalk compiler can warn you of the use of non-ISO specified built-in
predicates and arithmetic functions by using the
:ref:`portability <flag_portability>` compiler flag.

.. _programming_cc:

Conditional compilation
~~~~~~~~~~~~~~~~~~~~~~~

Logtalk supports conditional compilation within source files using the
:ref:`directives_if_1`, :ref:`directives_elif_1`,
:ref:`directives_else_0`, and :ref:`directives_endif_0` directives. This
support is similar to the support found in several Prolog systems such
as ECLiPSe, GNU Prolog, SICStus Prolog, SWI-Prolog, XSB, and YAP.

.. _programming_errors:

Avoiding common errors
~~~~~~~~~~~~~~~~~~~~~~

Try to write objects and protocol documentation **before** writing any
other code; if you are having trouble documenting a predicate perhaps we
need to go back to the design stage.

Try to avoid lengthy hierarchies. Composition is often a better choice
over inheritance for defining new objects (Logtalk supports
component-based programming through the use of
:ref:`categories <categories_categories>`). In addition, prototype-based
hierarchies are semantically simpler than class-based hierarchies.

Dynamic predicates or dynamic entities are sometimes needed, but we
should always try to minimize the use of non-logical features such as
asserts and retracts.

Since each Logtalk entity is independently compiled, if an object
inherits a dynamic or a meta-predicate predicate, then the respective
directives must be repeated to ensure a correct compilation.

In general, Logtalk does not verify if a user predicate call/return
arguments comply with the declared modes. On the other hand, Logtalk
built-in predicates, built-in methods, and message sending control
structures are fully checked for calling mode errors.

Logtalk error handling strongly depends on the ISO compliance of the
chosen Prolog compiler. For instance, the error terms that are generated
by some Logtalk built-in predicates assume that the Prolog built-in
predicates behave as defined in the ISO standard regarding error
conditions. In particular, if your Prolog compiler does not support a
``read_term/3`` built-in predicate compliant with the ISO Prolog
Standard definition, then the current version of the Logtalk compiler
may not be able to detect misspell variables in your source code.

.. _programming_style:

Coding style guidelines
~~~~~~~~~~~~~~~~~~~~~~~

It is suggested that all code between an entity opening and closing
directives be indented by one tab stop. When defining entity code, both
directives and predicates, Prolog coding style guidelines may be
applied. All Logtalk source files, examples, and standard library
entities use tabs (the recommended setting is a tab width equivalent to
4 spaces) for laying out code. Closed related entities can be defined in
the same source file. However, for best performance, is often necessary
to have an entity per source file. Entities that might be useful in
different contexts (such as library entities) are best defined in their
own source files.

A detailed coding style guide is available at the Logtalk official website.

.. _programming_session:

Compiling and running applications
----------------------------------

We run Logtalk inside a normal Prolog session, after loading the
necessary files. Logtalk extends but does not modify your Prolog
compiler. We can freely mix Prolog queries with the sending of messages
and our applications can be made of both normal Prolog clauses and
object definitions.

.. _programming_starting:

Starting Logtalk
~~~~~~~~~~~~~~~~

Depending on your Logtalk installation, you may use a script or a
shortcut to start Logtalk with your chosen Prolog compiler. On POSIX
operating systems, the scripts should be available from the
command-line; scripts are named upon the used backend Prolog compilers.
On Windows, the shortcuts should be available from the Start Menu.

If no scripts or shortcuts are available for your installation,
operating-system, or Prolog compiler, you can always start a Logtalk
session by performing the following steps:

#. Start your Prolog compiler.
#. Load the appropriate adapter file for your compiler. Adapter files
   for most common Prolog compilers can be found in the ``adapters``
   subdirectory.
#. Load the library paths file corresponding to your Logtalk
   installation contained in the ``paths`` subdirectory.
#. Load the Logtalk compiler/runtime files contained in the ``core``
   subdirectory.

Note that the adapter files, compiler/runtime files, and library paths
file are Prolog source files. The predicate called to load (and compile)
them depends on your Prolog compiler. In case of doubt, consult your
Prolog compiler reference manual or take a look at the definition of the
predicate ``'$lgt_load_prolog_code'/3`` in the corresponding adapter
file.

Most Prolog compilers support automatic loading of an initialization
file, which can include the necessary directives to load both the Prolog
adapter file and the Logtalk compiler. This feature, when available,
allows automatic loading of Logtalk when you start your Prolog compiler.

.. _programming_compiling:

Compiling and loading your applications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Your applications will be made of source files containing your objects,
protocols, and categories. The source files can be compiled to disk by
calling the :ref:`predicates_logtalk_compile_1` built-in predicate:

.. code-block:: text

   | ?- logtalk_compile([source_file1, source_file2, ...]).

This predicate runs the compiler on each file and, if no fatal errors
are found, outputs Prolog source files that can then be consulted or
compiled in the usual way by your Prolog compiler.

To compile to disk and also load into memory the source files we can use
the :ref:`predicates_logtalk_load_1` built-in predicate:

.. code-block:: text

   | ?- logtalk_load([source_file1, source_file2, ...]).

This predicate works in the same way of the predicate
``logtalk_compile/1`` but also loads the compiled files into memory.

Both predicates expect a source file name or a list of source file names
as an argument. The Logtalk source file name extension, as defined in
the adapter file (by default, ``.lgt``), can be omitted.

If you have more than a few source files then you may want to use a
loader helper file containing the calls to the ``logtalk_load/1-2``
predicates. Consulting or compiling the loader file will then compile
and load all your Logtalk entities into memory (see below for details).

With most :term:`backend Prolog compilers <backend Prolog compiler>`, you
can use the shorthands ``{File}`` for ``logtalk_load(File)`` and
``{File1, File2, ...}`` for ``logtalk_load([File1, File2, ...])``. The use
these shorthands should be restricted to the Logtalk/Prolog top-level
interpreter as they are not part of the language specification and may be
commented out in case of conflicts with backend Prolog compiler features.

The built-in predicate :ref:`predicates_logtalk_make_0` can be
used to reload all modified source files. Files are also reloaded when
the compilation mode changes. For example, assume that you have loaded
your application files and found a bug. You can easily recompile the
files in debug mode by using the queries:

.. code-block:: text

   | ?- set_logtalk_flag(debug, on).
   ...

   | ?- logtalk_make.
   ...

After debugging and fixing the bugs, you can reload the files in normal
(or optimized) mode by turning the :ref:`debug <flag_debug>` flag off and
calling the ``logtalk_make/0`` predicate again. With most backend Prolog
compilers, you can also use the ``{*}`` top-level shortcut.

An extended version of this predicate, :ref:`predicates_logtalk_make_1`,
accepts multiple targets including ``all``, ``clean``, ``check``,
``circular``, ``documentation``, and ``caches``. See the Reference Manual
for a complete list of targets and top-level shortcuts. In particular, the
``logtalk_make(clean)`` goal can be specially useful before switching
backend Prolog compilers as the generated intermediate files may not be
compatible. The ``logtalk_make(caches)`` goal is usually used when
benchmarking compiler performance improvements.

.. _programming_loaders:

Loader files
~~~~~~~~~~~~

Most examples directories contain a Logtalk utility file that can be used
to load all included source files. These loader files are usually named
``loader.lgt`` or contain the word "loader" in their name. Loader files
are ordinary source file and thus compiled and loaded like any source file.
For an example loader file named ``loader.lgt`` we would type:

.. code-block:: text

   | ?- logtalk_load(loader).

Usually these files contain a call to the built-in predicates
:ref:`predicates_set_logtalk_flag_2`
(e.g. for setting global, *project-specific*, flag values) and
:ref:`predicates_logtalk_load_1` or :ref:`predicates_logtalk_load_2` (for
loading project files), wrapped inside a Prolog ``initialization/1``
directive. For instance, if your code is split in three Logtalk source
files named ``source1.lgt``, ``source2.lgt``, and ``source3.lgt``, then
the contents of your loader file could be:

::

   :- initialization((
       % set project-specific global flags
       set_logtalk_flag(events, allow),
       % load the project source files
       logtalk_load([source1, source2, source3])
   )).

Another example of directives that are often used in a loader file would
be ``op/3`` directives declaring global operators needed by your
application. Loader files are also often used for setting source
file-specific compiler flags (this is useful even when you only have a
single source file if you always load it with using the same set of
compiler flags). For example:

::

   :- initialization((
       % set project-specific global flags
       set_logtalk_flag(underscore_variables, dont_care),
       set_logtalk_flag(source_data, off),
       % load the project source files
       logtalk_load(
           [source1, source2, source3],
           % source file-specific flags
           [portability(warning)]),
       logtalk_load(
           [source4, source5],
           % source file-specific flags
           [portability(silent)])
   )).

To take the best advantage of loader files, define a clause for the
multifile and dynamic ``logtalk_library_path/2`` predicate for the
directory containing your source files as explained in the next section.

A common mistake is to try to set compiler flags using ``logtalk_load/2``
with a loader file. For example, by writing:

.. code-block:: text

   | ?- logtalk_load(loader, [optimize(on)]).

This will not work as you might expect as the compiler flags will only
be used in the compilation of the ``loader.lgt`` file itself and will
not affect the compilation of files loaded through the
``initialization/1`` directive contained on the loader file.

.. _programming_libraries:

Libraries of source files
~~~~~~~~~~~~~~~~~~~~~~~~~

Logtalk defines a *library* simply as a directory containing source
files. Library locations can be specified by defining or asserting
clauses for the dynamic and multifile predicate
:ref:`predicates_logtalk_library_path_2`. For example:

::

   :- multifile(logtalk_library_path/2).
   :- dynamic(logtalk_library_path/2).

   logtalk_library_path(shapes, '$LOGTALKUSER/examples/shapes/').

The first argument of the predicate is used as an alias for the path on
the second argument. Library aliases may also be used on the second
argument. For example:

::

   :- multifile(logtalk_library_path/2).
   :- dynamic(logtalk_library_path/2).

   logtalk_library_path(lgtuser, '$LOGTALKUSER/').
   logtalk_library_path(examples, lgtuser('examples/')).
   logtalk_library_path(viewpoints, examples('viewpoints/')).

This allows us to load a library source file without the need to first
change the current working directory to the library directory and then
back to the original directory. For example, in order to load a
``loader.lgt`` file, contained in a library named ``viewpoints``, we
just need to type:

.. code-block:: text

   | ?- logtalk_load(viewpoints(loader)). 

The best way to take advantage of this feature is to load at startup a source
file containing clauses for the ``logtalk_library_path/2`` predicate needed
for all available libraries (typically, using a :term:`settings file`). This
allows us to load library source files or entire libraries without worrying
about libraries paths, improving code portability. The directory paths on the
second argument should always end with the path directory separator
character. Most backend Prolog compilers allows the use of environment
variables in the second argument of the ``logtalk_library_path/2``
predicate. Use of POSIX relative paths (e.g. ``'../'`` or ``'./'``) for
top-level library directories (e.g. ``lgtuser`` in the example above) is
not advised as different backend Prolog compilers may start with
different initial working directories, which may result in portability
problems of your loader files.

This :term:`library notation` provides functionality inspired by the
``file_search_path/2`` mechanism introduced by Quintus Prolog and later
adopted by some other Prolog compilers.

.. _programming_linter:

Compiler linter
~~~~~~~~~~~~~~~

The compiler includes a linter that checks for a wide range of possible
problems in source files. Notably, the compiler checks for unknown
entities, unknown predicates, undefined predicates (i.e. predicates that
are declared but not defined), missing directives (including missing
``dynamic/1`` and ``meta_predicate/1`` directives), redefined built-in
predicates, calls to non-portable predicates, singleton variables,
tautology and falsehood goals (i.e. goals that are can be replaced by
``true`` or ``fail``), and trivial fails (i.e. calls to predicates with
no match clauses). Some of the linter warnings are controlled by
compiler flags. See the next section for details.

.. _programming_flags:

Compiler flags
~~~~~~~~~~~~~~

The :ref:`predicates_logtalk_load_1` and :ref:`predicates_logtalk_compile_1`
always use the current set of default compiler flags as specified in
your settings file and the Logtalk adapter files or changed for the
current session using the built-in predicate
:ref:`predicates_set_logtalk_flag_2`.
Although the default flag values cover the usual cases, you may want to
use a different set of flag values while compiling or loading some of
your Logtalk source files. This can be accomplished by using the
:ref:`predicates_logtalk_load_2` or the :ref:`predicates_logtalk_compile_2`
built-in predicates. These two predicates accept a list of options
affecting how a Logtalk source file is compiled and loaded:

.. code-block:: text

   | ?- logtalk_compile(Files, Options).

or:

.. code-block:: text

   | ?- logtalk_load(Files, Options).

In fact, the ``logtalk_load/1`` and ``logtalk_compile/1`` predicates are
just shortcuts to the extended versions called with the default compiler
flag values. The options are represented by a compound term where the
functor is the flag name and the sole argument is the flag value.

We may also change the default flag values from the ones loaded from the
adapter file by using the :ref:`predicates_set_logtalk_flag_2`
built-in predicate. For example:

.. code-block:: text

   | ?- set_logtalk_flag(unknown_entities, silent).

The current default flags values can be enumerated using the
:ref:`predicates_current_logtalk_flag_2` built-in predicate:

.. code-block:: text

   | ?- current_logtalk_flag(unknown_entities, Value).

   Value = silent
   yes

Logtalk also implements a :ref:`directives_set_logtalk_flag_2`
directive, which can be used to set flags within a source file or within
an entity. For example:

::

   % compile objects in this source file with event support
   :- set_logtalk_flag(events, allow).

   :- object(foo).

       % compile this object with support
       % for dynamic predicate declarations
       :- set_logtalk_flag(dynamic_declarations, allow).
       ...

   :- end_object.

   ...

Note that the scope of the ``set_logtalk_flag/2`` directive is local to
the entity or to the source file containing it.

.. note::

   Applications should never rely on default flag values for working
   properly.  Whenever the compilation of a source file or an entity
   requires a specific flag value, the flag should be set explicitly
   in the file, in the entity, or in the loader file.

Version flags
^^^^^^^^^^^^^

.. _flag_version_data:
.. index:: pair: version_data; Flag

``version_data(Value)``
   Read-only flag whose value is the compound term
   ``logtalk(Major,Minor,Patch,Status)``. The first three arguments are
   integers and the last argument is an atom, possibly empty,
   representing version status: ``aN`` for alpha versions, ``bN`` for
   beta versions, ``rcN`` for release candidates (with ``N`` being a
   natural number), and ``stable`` for stable versions. The
   ``version_data`` flag is also a de facto standard for Prolog
   compilers.

Lint flags
^^^^^^^^^^

.. _flag_unknown_entities:
.. index:: pair: unknown_entities; Flag

``unknown_entities(Option)``
   Controls the unknown entity warnings, resulting from loading an
   entity that references some other entity that is not currently
   loaded. Possible option values are ``warning`` (the usual default)
   and ``silent``. Note that these warnings are not always avoidable,
   specially when using reflective designs of class-based hierarchies.

.. _flag_unknown_predicates:
.. index:: pair: unknown_predicates; Flag

``unknown_predicates(Option)``
   Defines the compiler behavior when calls to unknown predicates (or
   non-terminals) are found. An unknown predicate is a called predicate
   that is neither locally declared or defined. Possible option values
   are ``error``, ``warning`` (the usual default), and ``silent`` (not
   recommended).

.. _flag_undefined_predicates:
.. index:: pair: undefined_predicates; Flag

``undefined_predicates(Option)``
   Defines the compiler behavior when calls to declared but undefined
   predicates (or non-terminals) are found. Note that calls to declared
   but undefined predicates (or non-terminals) fail as per closed-world
   assumption. Possible option values are ``error``, ``warning`` (the
   usual default), and ``silent`` (not recommended).

.. _flag_steadfastness:
.. index:: pair: steadfastness; Flag

``steadfastness(Option)``
   Controls warnings about *possible* non :term:`steadfast <steadfastness>`
   predicate definitions due to variable aliasing at a clause head and a cut
   in the clause body. Possible option values are ``warning`` and ``silent``
   (the usual default due to the possibility of false positives).

.. _flag_portability:
.. index:: pair: portability; Flag

``portability(Option)``
   Controls the non-ISO specified Prolog built-in predicate and non-ISO
   specified Prolog built-in arithmetic function calls warnings plus use
   of non-standard Prolog flags and/or flag values. Possible option
   values are ``warning`` and ``silent`` (the usual default).

.. _flag_missing_directives:
.. index:: pair: missing_directives; Flag

``missing_directives(Option)``
   Controls the missing predicate directive warnings. Possible option
   values are ``warning`` (the usual default) and ``silent`` (not
   recommended).

.. _flag_duplicated_directives:
.. index:: pair: duplicated_directives; Flag

``duplicated_directives(Option)``
   Controls the duplicated predicate directive warnings. Possible option
   values are ``warning`` (the usual default) and ``silent`` (not
   recommended). Note that conflicting directives for the same predicate
   are handled as errors, not as duplicated directive warnings.

.. _flag_trivial_goal_fails:
.. index:: pair: trivial_goal_fails; Flag

``trivial_goal_fails(Option)``
   Controls the printing of warnings warnings for calls to local static
   predicates with no matching clauses. Possible option values are
   ``warning`` (the usual default) and ``silent`` (not recommended).

.. _flag_always_true_or_false_goals:
.. index:: pair: always_true_or_false_goals; Flag

``always_true_or_false_goals(Option)``
   Controls the printing of warnings for goals that are always true or
   false. Possible option values are ``warning`` (the usual default) and
   ``silent`` (not recommended).

.. _flag_lambda_variables:
.. index:: pair: lambda_variables; Flag

``lambda_variables(Option)``
   Controls the printing of lambda variable related warnings. Possible
   option values are ``warning`` (the usual default) and ``silent`` (not
   recommended).

.. _flag_suspicious_calls:
.. index:: pair: suspicious_calls; Flag

``suspicious_calls(Option)``
   Controls the printing of suspicious call warnings. Possible option
   values are ``warning`` (the usual default) and ``silent`` (not
   recommended).

.. _flag_redefined_built_ins:
.. index:: pair: redefined_built_ins; Flag

``redefined_built_ins(Option)``
   Controls the Logtalk and Prolog built-in predicate redefinition
   warnings. Possible option values are ``warning`` (the usual default)
   and ``silent``. Warnings about redefined Prolog built-in predicates
   are often the result of running a Logtalk application on several
   Prolog compilers as each Prolog compiler defines its set of built-in
   predicates.

.. _flag_singleton_variables:
.. index:: pair: singleton_variables; Flag

``singleton_variables(Option)``
   Controls the singleton variable warnings. Possible option values are
   ``warning`` (the usual default) and ``silent`` (not recommended).

.. _flag_underscore_variables:
.. index:: pair: underscore_variables; Flag

``underscore_variables(Option)``
   Controls the interpretation of variables that start with an
   underscore (excluding the anonymous variable) that occur once in a
   term as either don't care variables or singleton variables. Possible
   option values are ``dont_care`` and ``singletons`` (the usual
   default). Note that, depending on your Prolog compiler, the
   ``read_term/3`` built-in predicate may report variables that start
   with an underscore as singleton variables. There is no standard
   behavior, hence this option.

.. _flag_naming:
.. index:: pair: naming; Flag

``naming(Option)``
   Controls warnings about entity, predicate, and variable names per
   official coding guidelines (which advise using underscores for entity
   and predicate names and camel case for variable names). Additionally,
   variable names should not differ only on case. Possible option values
   are ``warning`` and ``silent`` (the usual default due to the current
   limitation to ASCII names and the computational cost of the checks).

.. _flag_duplicated_clauses:
.. index:: pair: duplicated_clauses; Flag

``duplicated_clauses(Option)``
   Controls warnings of duplicated entity clauses (and duplicated entity
   grammar rules). Possible option values are ``warning`` and ``silent``
   (the usual default due to the required heavy computations). When the
   term-expansion mechanism is used and results in duplicated clauses,
   the reported line numbers are for lines of the original clauses that
   were expanded.

Optional features compilation flags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _flag_complements:
.. index:: pair: complements; Flag

``complements(Option)``
   Allows objects to be compiled with support for complementing
   categories turned off in order to improve performance and security.
   Possible option values are ``allow`` (allow complementing categories
   to override local object predicate declarations and definitions),
   ``restrict`` (allow complementing categories to add predicate
   declarations and definitions to an object but not to override them),
   and ``deny`` (ignore complementing categories; the usual default).
   This option can be used on a per-object basis. Note that changing
   this option is of no consequence for objects already compiled and
   loaded.

.. _flag_dynamic_declarations:
.. index:: pair: dynamic_declarations; Flag

``dynamic_declarations(Option)``
   Allows objects to be compiled with support for dynamic declaration of
   new predicates turned off in order to improve performance and
   security. Possible option values are ``allow`` and ``deny`` (the
   usual default). This option can be used on a per-object basis. Note
   that changing this option is of no consequence for objects already
   compiled and loaded. This option is only checked when sending an
   :ref:`methods_asserta_1` or :ref:`methods_assertz_1` message to an
   object. Local asserting of new predicates is always allowed.

.. _flag_events:
.. index:: pair: events; Flag

``events(Option)``
   Allows message sending calls to be compiled with or without
   :ref:`event-driven programming <events_events>` support. Possible
   option values are ``allow`` and ``deny`` (the usual default). Objects
   (and categories) compiled with this option set to ``deny`` use
   optimized code for message-sending calls that does not trigger
   events. As such, this option can be used on a per-object (or
   per-category) basis. Note that changing this option is of no
   consequence for objects already compiled and loaded.

.. _flag_context_switching_calls:
.. index:: pair: context_switching_calls; Flag

``context_switching_calls(Option)``
   Allows context switching calls (``<</2``) to be either allowed or
   denied. Possible option values are ``allow`` and ``deny``. The
   default flag vale is ``allow``. Note that changing this option is of
   no consequence for objects already compiled and loaded.

Back-end Prolog compiler and loader flags
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. _flag_prolog_compiler:
.. index:: pair: prolog_compiler; Flag

``prolog_compiler(Flags)``
   List of compiler flags for the generated Prolog files. The valid
   flags are specific to the used Prolog backend compiler. The usual
   default is the empty list. These flags are passed to the backend
   Prolog compiler built-in predicate that is responsible for compiling
   to disk a Prolog file. For Prolog compilers that don't provide
   separate predicates for compiling and loading a file, use instead
   the :ref:`prolog_loader <flag_prolog_loader>` flag.

.. _flag_prolog_loader:
.. index:: pair: prolog_loader; Flag

``prolog_loader(Flags)``
   List of loader flags for the generated Prolog files. The valid flags
   are specific to the used Prolog backend compiler. The usual default
   is the empty list. These flags are passed to the backend Prolog
   compiler built-in predicate that is responsible for loading a
   (compiled) Prolog file.

Other flags
^^^^^^^^^^^

.. _flag_scratch_directory:
.. index:: pair: scratch_directory; Flag

``scratch_directory(Directory)``
   Sets the directory to be used to store the temporary files generated
   when compiling Logtalk source files. This directory can be specified
   using an atom or using :term:`library notation`. The directory must
   always end with a slash. The default value is a sub-directory of the
   source files directory, either ``'./lgt_tmp/'`` or ``'./.lgt_tmp/'``
   (depending on the backend Prolog compiler and operating-system).
   Relative directories must always start with ``'./'`` due to the lack
   of a portable solution to check if a path is relative or absolute.

.. _flag_report:
.. index:: pair: report; Flag

``report(Option)``
   Controls the default printing of messages. Possible option values are
   ``on`` (by usual default, print all messages that are not intercepted
   by the user), ``warnings`` (only print warning and error messages
   that are not intercepted by the user), and ``off`` (do not print any
   messages that are not intercepted by the user).

.. _flag_code_prefix:
.. index:: pair: code_prefix; Flag

``code_prefix(Character)``
   Enables the definition of prefix for all functors of Prolog code
   generated by the Logtalk compiler. The option value must be a single
   character atom. Its default value is ``'$'``. Specifying a code
   prefix provides a way to solve possible conflicts between Logtalk
   compiled code and other Prolog code. In addition, some Prolog
   compilers automatically hide predicates whose functor start with a
   specific prefix such as the character ``$``. Although this is not a
   read-only flag, it should only be changed at startup time and before
   loading any source files. When changing this flag (e.g. from a
   :term:`settings file`), restart with the :ref:`clean <flag_clean>`
   flag turned on to ensure that any compiled files using the old
   `code_prefix` value will be recompiled.

.. _flag_optimize:
.. index:: pair: optimize; Flag

``optimize(Option)``
   Controls the compiler optimizations. Possible option values are
   ``on`` (used by default for deployment) and ``off`` (used by default
   for development). Compiler optimizations include the use of static
   binding whenever possible, the removal of redundant calls to
   ``true/0`` from predicate clauses, the removal of redundant
   unifications when compiling grammar rules, and inlining of predicate
   definitions with a single clause that links to a local predicate, to
   a plain Prolog built-in (or foreign) predicate, or to a Prolog module
   predicate with the same arguments. Care should be taken when
   developing applications with this flag turned on as changing and
   reloading a file may render :term:`static binding` optimizations
   invalid for code defining in other loaded files. Turning on this
   flag automatically turns off the :ref:`debug <flag_debug>` flag.

.. _flag_source_data:
.. index:: pair: source_data; Flag

``source_data(Option)``
   Defines how much information is retained when compiling a source
   file. Possible option values are ``on`` (the usual default for
   development) and ``off``. With this flag set to ``on``, Logtalk will
   keep the information represented using documenting directives plus
   source location data (including source file names and line numbers).
   This information can be retrieved using the
   :ref:`reflection API <reflection_reflection>` and is useful for
   documenting, debugging, and integration with third-party development
   tools. This flag can be turned off in order to generate more compact
   code.

.. _flag_debug:
.. index:: pair: debug; Flag

``debug(Option)``
   Controls the compilation of source files in debug mode (the Logtalk
   default debugger can only be used with files compiled in this mode).
   Also controls, by default, printing of ``debug>`` and
   ``debug(Topic)`` messages. Possible option values are ``on`` and
   ``off`` (the usual default). Turning on this flag automatically turns
   off the :ref:`optimize <flag_optimize>` flag.

.. _flag_reload:
.. index:: pair: reload; Flag

``reload(Option)``
   Defines the reloading behavior for source files. Possible option
   values are ``skip`` (skip loading of already loaded files; this value
   can be used to get similar functionality to the Prolog directive
   ``ensure_loaded/1`` but should be used only with fully debugged
   code), ``changed`` (the usual default; reload files only when they
   are changed since last loaded provided that the any explicit flags
   and the compilation mode are the same as before), and ``always``
   (always reload files).

.. _flag_relative_to:
.. index:: pair: relative_to; Flag

``relative_to(Directory)``
   Defines a base directory for resolving relative source file paths.
   The default value is the directory of the source file being compiled.

.. _flag_hook:
.. index:: pair: hook; Flag

``hook(Object)``
   Allows the definition of an object (which can be the pseudo-object
   :ref:`user <apis:user/0>`) implementing the
   :ref:`expanding <apis:expanding/0>` built-in
   protocol. The hook object must be compiled and loaded when this option
   is used. It's also possible to specify a Prolog module instead of a
   Logtalk object but the module must be pre-loaded and its identifier
   must be different from any object identifier.

.. _flag_clean:
.. index:: pair: clean; Flag

``clean(Option)``
   Controls cleaning of the intermediate Prolog files generated when
   compiling Logtalk source files. Possible option values are ``off``
   and ``on`` (the usual default). When turned on, this flag also forces
   recompilation of all source files, disregarding any existing
   intermediate files. Thus, it is strongly advised to turn on this flag
   when switching backend Prolog compilers or changing flags such as
   :ref:`code_prefix <flag_code_prefix>` as the intermediate files
   generated by the compilation of source files may not be portable (due to
   differences in the implementation of the standard ``write_canonical/2``
   predicate) or valid (due to changes to the intermediate code format).

User-defined flags
^^^^^^^^^^^^^^^^^^

Logtalk provides a :ref:`predicates_create_logtalk_flag_3`
predicate that can be used for defining new flags.

.. _programming_smart:

Reloading and smart compilation of source files
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As a general rule, reloading source files should never occur in
production code and should be handled with care in development code.
Reloading a Logtalk source file usually requires reloading the
intermediate Prolog file that is generated by the Logtalk compiler. The
problem is that there is no standard behavior for reloading Prolog
files. For static predicates, almost all Prolog compilers replace the
old definitions with the new ones. However, for dynamic predicates, the
behavior depends on the Prolog compiler. Most compilers replace the old
definitions but some of them simply append the new ones, which usually
leads to trouble. See the compatibility notes for the backend Prolog
compiler you intend to use for more information. There is an additional
potential problem when using multi-threading programming. Reloading a
threaded object does not recreate from scratch its old message queue,
which may still be in use (e.g. threads may be waiting on it).

When using library entities and stable code, you can avoid reloading the
corresponding source files (and, therefore, recompiling them) by setting
the :ref:`reload <flag_reload>` compiler flag to ``skip``. For code under
development, you can turn off the :ref:`clean <flag_clean>` flag to avoid
recompiling files that have not been modified since last compilation
(assuming that backend Prolog compiler that you are using supports
retrieving of file modification dates). You can disable deleting the
intermediate files generated when compiling source files by changing the
default flag value in your settings file, by using the corresponding
compiler flag with the compiling and loading built-in predicates, or,
for the remaining of a working session, by using the call:

.. code-block:: text

   | ?- set_logtalk_flag(clean, off).

Some caveats that you should be aware. First, some warnings that might
be produced when compiling a source file will not show up if the
corresponding object file is up-to-date because the source file is not
being (re)compiled. Second, if you are using several Prolog compilers
with Logtalk, be sure to perform the first compilation of your source
files with smart compilation turned off: the intermediate Prolog files
generated by the Logtalk compiler may be not compatible across Prolog
compilers or even for the same Prolog compiler across operating systems
(e.g. due to the use of different character encodings or end-of-line
characters).

.. _programming_batch:

Using Logtalk for batch processing
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you use Logtalk for batch processing, you probably want to turn off
the :ref:`report <flag_report>` flag to suppress all messages of type
``banner``, ``comment``, ``comment(_)``, ``warning``, and ``warning(_)``
that are normally printed. Note that error messages and messages providing
information requested by the user will still be printed.

.. _programming_performance:

Optimizing performance
~~~~~~~~~~~~~~~~~~~~~~

The default compiler flag settings are appropriated for the
**development** but not necessarily for the **deployment** of
applications. To minimize the generated code size, turn the
:ref:`source_data <flag_source_data>` flag off. To optimize runtime
performance, turn on the :ref:`optimize <flag_optimize>` flag.
Your chosen backend Prolog compiler may also provide performance
related flags; check its documentation.

Pay special attention to file compilation/loading order. Whenever
possible, compile/load your files taking into account file dependencies
to enable :term:`static binding` optimizations. The easiest way to find
the dependencies and thus the best compilation/loading order is to use
the `diagrams <https://logtalk.org/tools.html#diagrams>`_ tool to generate
a file dependency diagram for your application.

Minimize the use of dynamic predicates. Parametric objects can often be
used in alternative. When dynamic predicates cannot be avoided, try to
make them private. Declaring a dynamic predicate also as a private
predicate allows the compiler to optimize local calls to the database
methods (e.g. :ref:`methods_assertz_1` and :ref:`methods_retract_1`) that
modify the predicate.

Sending a :term:`message to self` implies :term:`dynamic binding` but
there are often cases where :ref:`control_send_to_self_1` is misused
to call an imported or inherited predicate that is never going to be
redefined in a descendant. In these cases, a :term:`super call`,
:ref:`control_call_super_1`, can be used instead with
the benefit of often enabling static binding. Most of the guidelines for
writing efficient Prolog code also apply to Logtalk code. In particular,
define your predicates to take advantage of first-argument indexing. In
the case of recursive predicates, define them as tail-recursive predicates
whenever possible.

See the :ref:`section on performance <performance_performance>`
for a detailed discussion on Logtalk performance.
