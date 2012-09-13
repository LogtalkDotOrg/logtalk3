________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Additional licensing terms apply per Section 7 of the GNU General
Public License 3. Consult the `LICENSE.txt` file for details.
________________________________________________________________________


RELEASE NOTES
=============


3.00.0 Alpha 2 - September ??, 2012
===================================

Logtalk distribution
--------------------

* RENAMED: The `wenv` directory is renamed to `coding`.

Logtalk compiler and runtime
----------------------------

* NEW: Implemented support for a `coinductive_success_hook/1` predicate,
following the idea of `finally/1` clauses presented by Davide Ancona and
Elena Zucca in their CO-LP 2012 paper.

* FIXED: Revert the new dynamic binding cache handling code (introduced
in the previous alpha version) to the previous version as the new one can
break `bagof/3` and `setof/3` calls.

* CHANGED: Ensure that a call to the `parameter/2` built-in method doesn't
fail when called from within a category that is not imported by any object.
This may happen e.g. when a parametric category is used to hold definitions
for multifile predicates.

* FIXED: When the `report` flag is `off`, only suppress `core` component
messages. Likewise, when the `report` flag is set to `warnings`, only
suppress `information/0-1` messages from the `core` component.

* NEW: Accept `phrase//1` as a built-in non-terminal in the body of grammar
rules. From Richard O'Keefe feedback on the SWI-Prolog mailing list.

Prolog adapter and integration files
------------------------------------

* FIXED: Missing import of the `format/3` predicate in the XSB-MT integration
files.

* NEW: Support for the extended `table/1` directive in XSB.

Examples
--------

* FIXED: `nested` coinduction example and added corresponding unit tests.

* NEW: alternative definition of the coinductive predicate `comember/2` that
doesn't require tabling. Contributed by Davide Ancona. This definition enables
the `counter` example to work. Also added corresponding unit tests.

* NEW: coinductive example `arithmetic`, adapted from Davide Ancona paper
"Regular corecursion in Prolog".

IDEs, text editors, and syntax highlighters support
---------------------------------------------------

* NEW: Logtalk language bundle for TextMate 2.

* CHANGED: The TextMate Logtalk language bundles now use the default TextMate
license for language bundles instead of the Logtalk distribution license.


3.00.0 Alpha 1 - August 21, 2012
================================

Logtalk license
---------------

* CHANGED: Logtalk is now distributed under the GNU General Public License 3,
plus additional terms as per Section 7 of this license. See the `LICENSE.txt`
file for details.

Logtalk distribution
--------------------

* NEW: Added a `tools` directory for programming tools previously embedded
in the Logtalk compiler/runtime file or found on the `library` and `examples`
directories. When performing a system-wide Logtalk installation, each user
gets its own copy of the `tools` directory thus facilitating per-user
customization.

* CHANGED: Renamed the sample `settings.lgt` file to `settings-sample.lgt`,
thus simplifying backing up and upgrading the Logtalk user directory.

* CHANGED: The `paths` directory is now symbolically linked instead
of copied to the Logtalk user folder when using the installers or the
install scripts. User-defined library paths should be declared in the
user `settings.lgt` file.

Logtalk compiler and runtime
----------------------------

* NEW: Support for structured message printing. Added `print_message/3`
and `print_message_tokens/3` predicates plus `message_prefix_stream/4`,
`message_tokens//2`, `message_hook/4`, and `print_message_token/2` hook
predicates to the `logtalk` built-in object. The implementation supports
a subset of the message tokens supported by SWI-Prolog and YAP: `flush`,
`Format-Arguments`, `at_same_line`, and `nl`. Despite being based on the
support for structured message printing found originally on Quintus Prolog,
Logtalk's implementation allows tagging of messages not only by kind but
also by component. This allows libraries to define their own messages
without fear of conflicts with messages from other libraries. It also
allows library clients a simple way to refer to all messages from a
specific library.

* NEW: Support for relative and absolute source file paths in the compiling
and loading built-in predicates.

* MOVED: The functionality for debugging and for generating XML documenting
files have been moved into stand-alone Logtalk applications, `debugger` and
`lgtdoc`, in the `tools` directory.

* NEW: Code compiled in debug mode calls a `logtalk::debug_handler/2`
predicate that can be defined by the programmer to implement a debugger.
When this predicate is not defined, the Logtalk runtime simple calls the
translated goals using `call/1`. In addition, there's also a hook predicate,
`logtalk::trace_event/2`, that can be defined to e.g. collect information
on the fired clauses. The provided `debugger` and `lgtunit` tools use these
predicates in their implementation.

* CHANGED: The Logtalk compiler now uses a term-expansion predicate and a
goal-expansion predicate for dealing with Prolog dialect specific features.
Both predicates are defined in the back-end Prolog compiler adapter files.

* ADDED: Support for returning public, protected, and private operators in
the `public/1`, `protected/1`, and `private/1` entity properties when the
`source_data` flag is on.

* ADDED: Support for a `number_of_clauses/1` entity property when the
`source_data` flag is on. The number of clauses includes all clauses for
multifile predicates that an entity defines on behalf of another entity.

* RENAMED: The following compiler flags are renamed for clarity:
	* `unknown` to `unknown_entities`
	* `misspelt` to `misspelt_calls`
	* `singletons` to `singleton_variables`
	* `tmpdir` to `scratch_directory`

* REMOVED: The following compiler flags have been removed:`xmldir`, `xmldocs`,
`xmlspec`, `xmlsref`, `xslfile`, `break_predicate`, `redefined_built_ins`,
`missing_directives`, `altdirs`, and `startup_message`.

* RENAMED: The entity property `built_in` have been renamed to `final`. Besides
built-in entities, all entities compiled using the flag `reload(skip)` have the
property `final`.

* CHANGED: Use entity flags instead of a separated entity dynamic predicate to
represent entity compilation in debug mode.

* ADDED: `execution_context/6` predicate to the `logtalk` built-in object.

* IMPROVED: Also check the compiler flags used when a file was first loaded
when deciding to skip reloading it.

* IMPROVED: Save and try to restore the current working directory in case of
error when calling the `logtalk_load/1-2` and `logtalk_compile/1-2` built-in
predicates.

* FIXED: Spurious (re)loaded source file message when loading a loader file
(or a file that causes other files to be loaded) using the compiler option
`report(off)`.

* ADDED: Allow the `(*->)/2` soft-cut control construct, when available, to be
used as a message broadcasting control construct.

* NEW: When compiling a coinductive predicate, save the extended coinductive
template as a predicate property.

* CHANGED: Intercept the `current_predicate/1` and `predicate_property/2`
messages sent to the pseudo-object `user`. This allows consistent behavior
across back-end Prolog compilers.

* NEW: Added a `loaded_file/4` predicate to the `logtalk` built-in object,
which complements the existing `loaded_file/2-3` predicates and gives access
to the stream options (`encoding/1` and `bom/1`) used when the file was
compiled and loaded.

* IMPROVED: More accurate warning report by printing, in most cases, the line
numbers where the warning occurs. This change, however, forced warnings to be
reported individually instead of grouped by kind.

* IMPROVED: Compiler checking of `current_logtalk_flag/2` and `set_logtalk_flag/2`
goals.

* ADDED: Support for `predicate_property/2` properties `number_of_clauses/1`,
`declared_in/2`, `defined_in/2`, and `redefined_from/2`. These properties are
only available, however, when the `source_data` flag is turned on.

* FIXED: Bug in the compilation of modules as objects when the `source_data`
flag is turned on that resulted in missing module properties and also a
non-instantiated argument in the `number_of_clauses/2` property for the
module predicates.

* FIXED: Implementation of the `current_logtalk_flag/2` built-in predicate,
ensuring that it returns correct results when called during file compilation
and dynamic entity creation.

* FIXED: Line number reported by the compiler when a syntax error is found.

* FIXED: Reporting of the coinduction stack when debugging the preflight
goals.

* IMPROVED: Use the Logtalk extension specified in the used adapter file when
loading a settings file.

Prolog adapter and integration files
------------------------------------

* IMPROVED: When the `LOGTALKHOME` environment variable is not defined and
we cannot locate the Logtalk installation directory at the usual places,
try to use a location relative to the directory where the integration script
is found.

* CHANGED: SWI-Prolog adapter file to make it easier to generate and use `.qlf`
files for the Logtalk source files. See the default settings file for an usage
example.

* IMPROVED: Updated the SWI-Prolog and YAP adapter files to only goal-expand
`::/2` calls within modules other than `user`. Also added `goal_expansion/2`
rules for optimizing `::/2` calls made from within modules.

* CHANGED: Set the default scratch directory for SICStus Prolog to the current
directory to avoid broken example unit tests due to differences regarding other
Prolog compilers in the management of the working directory.

* UPDATED: CxProlog adapter file with a tentative hack for expanding file system
paths.

* UPDATED: Lean Prolog adapter file to require version 3.7.10 (or later), which
implements the missing `callable/1` and `subsumes_term/2` ISO Prolog standard
built-in predicates.

* IMPROVED: YAP integration files to better hide Logtalk compiler and runtime
internal predicates.

* IMPROVED: Updated the XSB integration files to use `index/2` directives for
the dynamic predicates implementing the dynamic binding lookup predicate
caches.

* ADDED: File system predicates for decomposing file paths and for converting
between Prolog internal file paths and operating-system paths.

* ADDED: Predicate for converting Prolog dialect specific exception terms into
standard exception terms.

* REMOVED: Auxiliary list and term pretty-printing predicates (the compiler
and runtime now use portable, standard-based definitions of these predicates).

Documentation
-------------

* CHANGED: The release notes and most informative text files are now formatted
using GitHub Flavored Markdown syntax for easy conversion to e.g. HTML.

* IMPROVED: Change sets in release notes are now prefixed with the change
type, following the example of the SWI-Prolog release notes.

Library
-------

* MOVED: The unit test framework have been moved into the `tools` directory.

Tools
-----

* IMPROVED: The unit test framework can now print entity predicate clause
coverage information for the tested entities. In addition, it uses structured
message printing for all output, for easy integration with other development
tools.

* IMPROVED: The unit test framework is now more resilient to failed unit tests
that redirect the standard output stream.

* UPDATED: The `lgtdoc` tool now uses reflection to access all relevant entity
information (requires compilation of source files with the `source_data` flag
turned on).

* ADDED: Protocols for the `debugger` and `lgtdoc` tool interfaces to facilitate
defining alternative but compatible debugging and documentation tools.

Examples
--------

* MOVED: The `diagrams`, `help`, and `profilers` examples have been moved to
the `tools` directory.

* UPDATED: The unit tests of several examples to allow printing of predicate
clause coverage.

* UPDATED: Added an usage example of the `meta_non_terminal/1` directive plus
the `call//N` built-in non-terminal to the `dcgs` example. Based on a Richard
O'Keefe post on the SWI-Prolog mailing list.

Installers and installation scripts
-----------------------------------

* UPDATED: The MacPorts portfile now turns off the option for building flat
packages that was introduced as a new default in recent MacPorts versions.

* UPDATED: The Windows installer script to generate an integration shortcut
for the window-based YAP executable introduced in version 6.3.2.

* ADDED: RTF versions of the README and LICENSE files and an icon for use on
the Windows installer.

IDEs, text editors, and syntax highlighters support
---------------------------------------------------

* NEW: Notes on PDT Logtalk support and configuration parameters.

* NEW: Notes on paste bin web services that support Logtalk and Prolog syntax
highlighting.

* NEW: Notes on the Ultraviolet syntax highlighting engine.

* NEW: Notes on the Chocolat MacOS X text editor.

* CHANGED: Updated the SyntaxHighlighter support to match the contribution to
the original software.


OLD RELEASE NOTES FOR LOGTALK 2.x
=================================

(with minimal Markdown reformatting)


Corrected a bug in the compilation of Prolog modules as objects when the
"source_data" flag is turned on that resulted in a non-instantiated
argument in the "number_of_clauses/2" property for the module predicates.


2.44.1 - May 28, 2012
=====================

Modified the message sending mechanism to call the "before" event handlers
before performing the method lookup. This change should have no impact on
existing code but allows some interesting applications (e.g. fail instead
of throwing an exception when a message is not understood).

More consistent handling of compiler flags between setting flag values
when compiling/loading a file and when using the set_logtalk_flag/2
built-in predicate: in both cases, setting the "smart_compilation" flag
on will turn off the "clean" flag and setting the "clean" flag on will
turn off the "smart_compilation" flag. Also, setting the "debug" flag
on will turn off both the "smart_compilation" and "clean" flags.

Allows the soft-cut control construct, (*->)/2, when natively supported
by the back-end Prolog compiler, to be used in the body of grammar rules
(after a remark by Daniel Diaz while discussing GNU Prolog support for
the soft-cut control construct).

Corrected a Logtalk compiler bug when the "source_data" flag is on with
back-end Prolog compilers where is not possible to retrieve the position
of a read term.

Corrected a bug in the compilation of the meta_non_terminal/1 directive
where only the first meta-argument would be correctly processed.

Updated the ECLiPSe config file to detect when running on Windows 64
bits systems and to ignore non-predicate and non-operator exports when
pre-processing lib/1 directives.

Updated the unit test framework to print more informative messages for
failed unit tests.

Added predicate split/4 (for splitting a list into sublists with a given
length) to the "listp", "list", and "difflist" library entities.

Updated the "cc" example with a definition for the shell/2 predicate for
CxProlog.

Changed the names of the objects in the "mi" example to avoid a name
conflict when running the example unit tests.

Corrected a bug in the Windows installer that prevented detection of
YAP 64 bits versions. Thanks to Stepan Ipatov for the bug report.

Corrected a bug in the Windows installer that prevented users from
customizing the installation directory. Thanks to Daniel Moniz for
the bug report and to Gavin Lambert for diagnosing the bug.

Updated the Windows installer script for improved detection of XSB and to
default to install only the per-user files when run by a non-admin user on
a computer where Logtalk is already installed.

Added basic support for the LaTeX "listings "package. Added notes on the
"minted" and "texmates" LaTeX packages.

Added notes on the Textastic iPad text editor.

Added basic syntax coloring and code folding support for the UltraEdit
text editor.

Updated the TextMate bundle to disable the automatic indent corrections
introduced in the development version of TextMate 2.0.


2.44.0 - March 7, 2012
======================

Changed the semantics of complementing categories to allow patching both
predicate declarations and predicate definitions of complemented objects.
This is accomplished by ensuring that predicate declaration and definition
lookups start at the complementing categories before looking into the
complemented object itself or into its related entities.

Updated the Logtalk compiler in order to generate a warning when compiling
a complementing category for a complemented object that was compiled with
support for complementing categories turned off.

Allow the use of static binding when compiling messages for statically
declared dynamic methods where both the method declaration and the method
definition are found on static binding entities.

Allow the use of static binding when compiling hook object definitions of
the term_expansion/2 and goal_expansion/2 predicates.

Corrected a bug in the processing of meta-calls for meta-predicates where
the calling context would be set to "this" instead of "sender" whenever a
meta-argument was one of the ::/2, {}/1, <</2, or :/2 control constructs.
Thanks to Daniel Lyons for the bug report.

Corrected a bug in the processing of meta-calls when using wrappers for
the bagof/3 and setof/3 built-in methods where the meta-argument contains
multiple existentially qualified variables.

Removed support for the deprecated "startup_message" flag flags/1 value.

Allow open lists of terminals in the body of grammar rules.

Added (+)/1, min/2, max/2, acos/1, and asin/1 to the list of ISO Prolog
built-in functions used by the portability flag. Added term_variables/2
to the list of ISO Prolog built-in predicates.

Added an example, "patching", of using complementing categories to patch
broken predicate declarations and predicate definitions of complemented
classes and prototypes.

Added an example of combining constraints defined in different objects
to the "chr" example based on sample code posted by Gergö Barany on the
SWI-Prolog mailing list.

Added syntax coloring support for the min/2, max/2, acos/1, and asin/1
built-in functions and the term_variables/2 built-in predicate to all
supported text editors and syntax highlighters. Updated the syntax
highlighting test source file to include de facto standard Prolog
module directives.

Updated the support for Exuberant ctags. Declared predicates are now
listed using the Functor/Arity notation.

Updated the Vim syntax file for Logtalk, applying a patch contributed
by Thilo Six.


2.43.3 - December 21, 2011
==========================

Changed the compiler to take into account the uses/2 directive when
compiling calls to the reflection built-in methods. This change extends
the semantics of the uses/2 directive and allows easier migration from
plain Prolog applications to Logtalk.

Updated the implementation of the predicate_property/2 built-in method,
adding a scope/1 property.

Updated the Logtalk compiler to add information about a source file name,
source file directory, and source file compiler options to the generated
Prolog files. In previous versions, this information was registered only
when loading a source file. The changes simplify building applications
where Logtalk libraries are pre-compiled and pre-loaded.

Updated the Logtalk compiler to print the name of the hook object used in
the compilation of source files when the "hook" compiler flag is defined.

Updated the built-in debugger in order to avoid unnecessary choice points
created by back-end Prolog compilers whose retract/1 implementation fails
to take advantage of logical update semantics to make calls deterministic.

Corrected a bug in the Logtalk compiler runtime handler where it failed
to decompile internal entity identifiers when throwing entity existence
exceptions.

Updated the Lean Prolog config file, adding index/1 directives for some
of the dynamic predicates used by the internal Logtalk runtime tables.

Updated the SWI-Prolog integration files to test for the availability
of the index/1 directive, which is deprecated by the new experimental
SWI-Prolog support for just-in-time multi-argument indexing.

Added predicates subsequence/4 (for generating subsequences of a list
with a given length), substitute/4 (for replacing elements in a list),
and hamming_distance/3 to the library "listp" and "list" entities.

Added euclidean_norm/2, chebyshev_norm/2, manhattan_norm/2,
euclidean_distance/3, chebyshev_distance/3, manhattan_distance/3, and
scalar_product/3 predicates to the library "numberlistp" and "numberlist"
entities.

Corrected some silly bugs in the definition of the "character" library
object predicates is_bin_digit/1, is_octal_digit/1, is_dec_digit/1, and
is_hex_digit/1.

Updated the library "all_loader.lgt" loader file to also load the
"meta_compiler_loader.lgt" loader file.


2.43.2 - October 4, 2011
========================

Optimized the processing of operator declarations when compiling source
files and dynamically creating new entities.

Updated the Logtalk compiler to use the "modules" compiler flag when
testing for Prolog module support, thus avoid compilation issues with
back-end Prolog compilers that don't support a module system but still
define (:)/2 as a built-in control construct or predicate.

Removed support for the "startup_message" compiler flag "flags(compact)"
and "flags(verbose)" values, replaced by a single value, "flags", which
corresponds to the old "flags(compact)" value.

Corrected a bug when using op/3 directives with the create_protocol/3,
create_object/4, and create_category/4 built-in predicates where the
local operator declarations would be visible outside the new entities.

Corrected a bug where creating a new dynamic entity at runtime would
define a no longer used bookkeeping dynamic predicate that would not
be abolished when the entity is abolished.

Corrected a compiler bug where a "super" call from within an object that
only instantiates itself and doesn't specialize any object would throw an
exception instead of failing.

Updated the "benchmarks" example in order to protect against arithmetic
exceptions when computing the total number of calls per second for each
test. Reduced the number of benchmark test repetitions when running the
unit tests to avoid lengthy computing times with slow back-end Prolog
compilers.


2.43.1 - September 12, 2011

Changed the compiler to take into account the uses/2 directive when
compiling calls to the database built-in methods. This change extends
the semantics of the uses/2 directive and allows easier migration from
plain Prolog applications to Logtalk.

Added experimental support for using a predicate template as argument to
the coinductive/1 directive. This template allows the specification of
which arguments are meaningful for coinductive success. Based on similar
functionality found on on Feliks Kluzniak's DRA meta-interpreter.

Improved support for debugging coinductive predicates when using the
Logtalk built-in debugger.

Removed coinduction support for CxProlog. The current version of this
compiler lacks a soft-cut built-in predicate or control construct, which
is now required by the experimental coinduction implementation.

Lifted a long-standing limitation that prevented the user definition of
wrapper predicates for the bagof/3 and setof/3 built-in methods whenever
the goal argument may use the ^/2 existential quantifier.

Added support for the "^" meta-predicate meta-argument mode specifier
suggested by Jan Wielemaker and found on SWI-Prolog 5.11.25. This mode
specifier is useful when defining wrappers for the bagof/3 and setof/3
built-in methods whenever the goal argument may use the ^/2 existential
quantifier.

Added "prolog_compiler" and "prolog_loader" compiler flags, which allow
passing options to the back-end Prolog compiler built-in predicates that
compile to disk and load a (compiled) Prolog flag. Updated the ECLiPSe,
Qu-Prolog, SICStus Prolog, and XSB config files to take advantage of these
new flags. For the other supported back-end Prolog compilers, the default
value for both flags is the empty list.

Corrected a bug when compiling calls to Prolog proprietary built-in meta-
predicates where the Logtalk compiler could attempt to process non meta-
arguments as goals. This bug only manifested itself with some back-end
Prolog compilers such as ECLiPSe.

Corrected a bug in the processing of conditional compilation directives.

Corrected a bug that prevented passing alias/3 directives on calls to
the create_object/4, create_category/4, and create_protocol/3 built-in
predicates.

Removed from the built-in object "logtalk" the decompile_predicate_head/4
and decompile_predicate_indicator/4 deprecated methods.

Removed support for the long deprecated Logtalk metapredicate/1 directive.

Corrected a performance bug when caching calls to category predicates where
the cached entries could be more specific than necessary when working with
parametric categories.

Improved caching of "super" calls from within prototypes. Previously, all
cached entries would be specialized by the value of "self" in order to
properly deal with the special cases where an object both instantiates and
specializes other objects.

Updated the Logtalk compiler to activate any operator declarations found
on predicate scope directives during the compilation of the entities that
contain the directives.

Improved the performance of coinductive predicates when using back-end
Prolog compilers such as SICStus Prolog that don't provide the soft-cut
control construct but provide the soft-cut built-in meta-predicate (if/3).

Corrected a bug where a spurious report of compilation/loading warnings
could be printed when a source file was compiled/loaded with the compiler
flag "report" turned off.

Added a "streamvars" library object, derived from the existing "assignvars"
library category, featuring support for adding and retrieving terms (which
may be variables) from a "stream" variable.

Updated the "coinduction" example, taking advantage of the enhanced
coinductive/1 directive and broadening the number of individual examples
that can be run using SICStus Prolog. Added a model checking example, using
coinduction, coroutining, and constraints, contributed by Neda Saeedloei.

Updated the "metapredicates" example with sample code illustrating how
to define wrappers for the bagof/3 and setof/3 built-in methods when the
meta-argument may contain existentially qualified variables.

Updated the "cc" example, correcting predicate definitions for several
back-end Prolog compilers and adding some basic unit tests.

Updated the "encodings" example with limited support for Lean Prolog.

Added missing entries for the meta_non_terminal/1 directive and for the
call//N non-terminal to the "help" example.

Added a parser for PDDL 3.0 files, contributed by Robert Sasak.

Added code folding support to the SubEthaEdit text editor. Added code
folding support for conditional compilation blocks to the Kate, Vim,
and TextMate text editors.

Updated the integration scripts in order to load the library paths file
before loading the Logtalk compiler/runtime. This change aims to simplify
loading of library and example files from the settings files. In addition,
also look for the Logtalk installation directory in $HOME/share when the
LOGTALKHOME environment variable is not defined. Thanks To Vítor Santos
Costa for the suggestion.

Updated the Windows installer in order to support Lean Prolog and detect
YAP 64 bits versions.


2.43.0 - July 31, 2011
======================

Added experimental support for Lean Prolog.

Changed error reporting by the Logtalk compiler and runtime. All exception
terms now use, whenever possible, the error(Error, logtalk(Goal, Entity))
format. Changed the internal error checking to use, whenever practical,
the new '$lgt_must_be'/2-3 internal predicates.

Added an "optimize" flag, whose default value is "on". This flag allows
the user to turn off existing optimizations for e.g. debugging tasks (the
currently implemented optimizations include the removal of redundant calls
to true/0 from the body of compiled predicate clauses and the removal of
redundant calls to (=)/2 from the body of clauses generated by the
expansion of grammar rules).

Added a "source_data" flag, whose default value is "on". With this flag
set to "on", Logtalk will keep the information represented using mode/2,
info/1-2, uses/2, and use_module/2 directives plus source location data
(including source file names and line numbers), useful both for debugging
and for integration with development tools. This flag can be turned off
in order to generate more compact code. The parameter_names/1 object and
category property is removed in order to avoid redundancy.

Implemented protected/1 and private/1 properties for objects, protocols,
and categories. Together with the public/1 property, these properties
allows the list of predicates declared in the entity to be accessed for
reflective computations. Implemented declares/2, defines/2, includes/3,
and provides/3 properties for accessing the properties of predicates
declared and defined within an entity. Declaration properties include
line_count/1, the line number of the predicate scope directive. Definition
properties include line_count/1, the line number of the head of the first
clause for the predicate, and number_of_clauses/1, the number of clauses
for the predicate. Also implemented experimental uses/3 and use_module/3
properties.

Added a predicate property, redefined_from/1, allowing querying about the
object or category that contains a predicate definition that is redefined.

Added a loaded_file/3 predicate to the "logtalk" built-in object, which
complements the existing loaded_file/2 predicate and gives access to the
explicit options used when the file was compiled and loaded.

Implemented compiler support for the meta_non_terminal/1 directive and the
call//N built-in non-terminal.

Simplified the compilation of grammar rules by removing the support for
dealing with non-terminal scope and existence errors at the grammar rule
level instead of the predicate level. Although this feature could be
helpful when debugging grammar rules, it complicated the integration of
Logtalk with Prolog-native development tools.

Updated the Logtalk compiler in order to save the entity clause locations
and make this information available to the back-end Prolog compilers.

Updated the Logtalk compiler in order to avoid generating unnecessary
internal catchall clauses for the predicate declaration and definition
tables for dynamic entities.

Updated the Logtalk compiler in order to preclude using multifile
predicates for breaking object encapsulation. A primary declaration
entity must exist, containing both a scope and a multifile directive
for the predicate. In addition, defining clauses for a multifile
Entity::Functor/Arity without a corresponding multifile/1 directive
will now result in a compilation error instead of simply a warning.

Removed the Logtalk compiler restriction that forced discontiguous/1
and dynamic/1 directives to precede calls to the declared predicates.

Improved the Logtalk compiler report of misspelt calls to non-terminals
in grammar rules.

Allow the use of the :/1 control construct on the body of grammar rules.

When debugging messages specified in uses/2 directives, the message is now
printed implicitly qualified as in the source code written by the user.

Changed the internal predicates used in the implementation of the built-in
multi-threading predicates in order to facilitate the integration with
high-level debugging tools that require decompilation of the generated
Prolog code.

Corrected a bug introduced in Logtalk 2.42.4 on the compilation of entity
predicates that call synchronized predicates. This bug resulted in calls
being made instead to the non-synchronized forms of the predicates, which
would manifest in random failures of thread goals.

Corrected a long standing bug where a runtime-compiled meta-call within
a category would be compiled as a call to a predicate of the object
importing the category instead as a call to a local category predicate.

Corrected a long standing bug where entity flag values set locally using
the set_logtalk_flag/2 directive would fail to be saved into the entity
compiled code.

Corrected a long standing bug when abolish a dynamic predicate in a
non-prototype object where the abolish/1 method would succeed without
the predicate being actually abolished.

Corrected a bug in setting the correct execution context when calling
a meta-argument that requires a runtime compilation in the context of
"sender".

Corrected a bug in the compilation of explicit-qualified calls to module
meta-predicates where the module qualification would be dropped from the
compiled call. Note that these calls will use Logtalk meta-predicate
semantics (i.e. the meta-arguments will be called in the context of the
caller) instead of the usual Prolog module meta-predicate semantics.

Corrected a bug where the coinduction stacks was not initialized when
using the <</2 control construct.

Corrected a bug that prevented using the coinductive/1 directive with a
list of predicate indicators as argument.

Corrected a bug in the implementation of the threaded_peek/1 built-in
predicate when called from within categories.

Corrected a bug when compiling a Prolog module as an object when the
module/2 directive is used to export operator declarations.

Updated the built-in debugger command that prints the execution context
to also print the meta-call context and the coinduction stack.

Changed the semantics of the coinductive/1 directive to use the same
semantics as the coinductive1/1 directive found on the U.T.Dallas
coinduction implementation. Thanks to Feliks Kluzniak for the examples
that helped clarified the issues with the previous implementation. In
addition, take advantage of the soft-cut control construct when supported
by the back-end Prolog compiler for better performance.

Refined error checking for the built-in predicates category_property/2,
object_property/2, and protocol_property/2, throwing a type_error/2 when
the second argument is neither a variable or a callable term.

When compiling a module as an object, warn instead of throwing an error
when compiling a call to a built-in predicate used as a query.

Attempting to load a settings file containing syntax errors will now
print a warning instead of failing silently.

Allow using a Prolog module as an hook object. The module must be loaded
and its identifier must be different from any object identifier.

Changed reporting of declared but not defined static predicates to exclude
multifile predicates.

Fixed a compilation performance issue with objects and categories
containing a large number of clauses for the same predicate.

Improved portability of calls from within objects and categories of calls
to Prolog database and reflection built-in predicates whenever the first
argument is an explicit module-qualified term.

Turning on the "smart_compilation" compiler flag will now automatically
switch off the "clean" flag.

Reimplemented the reflection built-in methods predicate_property/2,
object_property/2, protocol_property/2, and category_property/2 in
order to make them deterministic for most calls where the second
argument is bound.

Added Logtalk compiler support for default annotations, defined in the
back-end Prolog compiler config files.

For all config files, renamed the predicate '$lgt_is_proper_list'/1 to
'$lgt_is_list'/1 and and replaced the old '$lgt_is_list'/1 predicate by
a new '$lgt_is_list_or_partial_list'/1 predicate.

Updated the SWI-Prolog config file in order to delete QLF files when the
"clean" flag is "on" and to avoid preventing loading of a Logtalk source
file when a Prolog file with the same name exists on the same lookup
directory. In addition, add source location information to the generated
Prolog files resulting from the compilation of Logtalk source files. This
location information is necessary for supporting some of the SWI-Prolog
development tools.

Updated the SWI-Prolog integration files in order to hide the source code
of the Logtalk (::)/2 and (<<)/2 built-in predicates from this compiler
debugging tools. Also corrected an index/1 directive for an internal
predicate lookup caching predicate.

Updated the SWI-Prolog hooks file in order to support tracing Logtalk
source code using the SWI-Prolog graphical debugger and using the edit/1
predicate with library notation and with the Logtalk "altdirs" flag on.

Updated the ECLiPSe config file, simplifying some of the file handling
predicates, turning off ISO overly restrictions on operator usage, and
making use of the recently implemented call/N built-in control constructs
when available.

Updated the GNU Prolog config file for the stable 1.4.0 release.

Updated the SICStus Prolog 4 config file in order to workaround some of
the incompatible changes to absolute_file_name/2 and open/3-4 introduced
on version 4.1.0.

Updated the B-Prolog config file to require version 7.5#5 or later as
this release fixes some of the bugs on previous versions that show up
when using Logtalk. Added default annotations for matching clauses.

Added predicates select/4 and selectchk/4 to the library entities "listp",
"list", and "difflist".

Corrected a bug in the library support for unit tests, "lgtunit", where
testing for exceptions would accept any exception term as conforming to
the unit test specifications.

Updated the "cc" example with bug fixes and improvements for ECLiPSe
contributed by Joachim Schimpf, with a bug fix for the SWI-Prolog
implementation of the expand_path/2 predicate (resulting from recent
changes to the SWI-Prolog absolute_file_name/3 built-in predicate),
and with a definition for the command_line_arguments/1 predicate for
B-Prolog.

Updated the "coinduction" example, adding some more individual examples
by Gopal Gupta and Feliks Kluzniak.

Updated the "xml_parser" contribution in order to workaround portability
issues with GNU Prolog.

Correct a bug in some shell scripts where "#/bin/bash" was used instead
of "#!/bin/bash". Thanks to Yiorgos Adamopoulos for the bug report.

Updated the MacOS X command-files (on the "scripts/macosx/command_files/"
folder) in order to depend on the value of the LOGTALKHOME environment
variable instead of using a fixed path to the Prolog integration scripts.

Updated the Inno Setup GUI Windows installer script in order to allow full
installation by non-admin users.

Added syntax coloring support for the ISO Prolog directive include/1 to
all supported text editors and syntax highlighters.

Corrected some bugs on the tab triggers for the Gedit Logtalk snippets
that prevented their use.

Changed back to using XEP instead of FOP when generating the PDF versions
of the User and Reference manuals as FOP truncates the contents of some of
the pages. Thanks to Feliks Kluzniak for the bug report.


2.42.4 - April 4, 2011
======================

Added an entity_prefix/2 predicate and two sets of utility predicates,
decompile_predicate_heads/2-4 and decompile_predicate_indicators/2-4,
to the "logtalk" built-in object. Marked the decompile_predicate_head/4
and decompile_predicate_indicator/4 as deprecated.

Added support for preserving the scope information for operators declared
using scope directives during compile time. Added information on entity
declared operators to the XML documenting files. Updated the text, PDF,
and (X)HTML conversion scripts to print out operator information.

Applied several low-level optimizations to the Logtalk compiler/runtime.

Moved the internal predicates that construct and access the predicate
execution context from the compiler to the Prolog config files in order
to support fine performance tuning that is Prolog compiler dependent.

Updated the implementation of the threaded/1 multi-threading built-in
predicate in order to use a small performance optimization based on the
term_variables/2 built-in predicate suggested by Timon Van Overveldt.
This optimization aims to reduce unnecessary copy and unification of
terms. In addition, replace the use of the setup_call_cleanup/3 built-in
predicate with the catch/3 built-in predicate by simplifying handling of
both master and slave threads, which also improves performance.

Corrected a bug in the implementation of the threaded/1 multi-threading
built-in predicate where failure by the user to specify independent goals
when attempting to use independent and-parallelism would result in zombie
threads.

More informative exception terms when attempting to declare a synchronized
predicate as dynamic or a dynamic predicate as synchronized.

Updated the XSB config file in order to support the compilation of modules
as objects. Added a definition for the missing setup_call_catcher_cleanup/4
built-in predicate. XSB 3.3 (or a later version) is now required for
compatibility with Logtalk.

Updated the SWI-Prolog config file in order to provide limited support
for the arithmetic_function/1 proprietary directive (arithmetic functions
are compile as global instead of as local to the objects or categories
defining them).

Updated the YAP config file in order to ignore the style_check/1 directive,
to interpret the yap_flag/2 directive as a set_prolog_flag/2 directive, and
to support the current development version of YAP 6.3 (which includes some
incompatible changes to the absolute_file_name/3 built-in predicate).

Changed the default XSL-FO processor (used to generate the PDF versions of
the User and Reference manuals) from XEP to FOP.

Updated the "xml_parser" contribution in order to workaround portability
issues with Qu-Prolog, SICStus Prolog, and XSB.

Updated the "encodings" example in order to ensure that the there is no
whitespace before the encoding/1 directive.

Updated the "attvars" example in order to support B-Prolog and XSB. Also
added support for using attributed variables within Logtalk parametric
objects and parametric categories. Thanks to Jan Wielemaker, Vítor Santos
Costa, and Bart Demoen for feedback and implementation suggestions.

Updated the "profilers" example with preliminary support for the changes
to execution profiling in the SICStus Prolog 4.2 version.

Updated the "expansion" example with more sample code on how to combine
term- and goal-expansion definitions from different hooks objects.

Updated the "coinduction" example, adding an experimental keysort/2
coinductive predicate. Thanks to Jan Burse for the suggestion.

Updated the "tak" multi-threading example where a missing cut would result
in quickly exhausting the local stack when using SWI-Prolog due to the
creation of a very large number of unnecessary choice points. Thanks to
Timon Van Overveldt for the bug report.

Updated the "mtbatch" multi-threading example in order to provide expanded
benchmarking of the "tak" example.

Updated the "coinduction" example with individual examples of finding the
cyclic paths in graphs. Thanks to Feliks Kluzniak and Gopal Gupta for the
examples.

Updated the MacPorts portfile and the script that updates it in order to
remove the calculation of the MD5 checksum, which is being phased out by
MacPorts.

Updated the Inno Setup GUI Windows installer script in order to workaround
a bug on the SWI-Prolog Windows installers where the uninstall procedure
fails to delete the SWI-Prolog keys from the Windows registry.


2.42.3 - February 21, 2011
==========================

Allow dynamic predicates declared in a category to be called in "this"
from within the category. In previous versions, such calls would result
in a compile-time error, forcing these predicates to be called in "self"
(using the ::/1 message sending control construct).

Added Logtalk compiler support for pre-compiled clause heads (using the
{}/1 control construct). This feature is mostly useful when using the
term-expansion mechanism.

Corrected a bug on the code that simplifies the predicate clauses
generated by the compilation of grammar rules. The code would loop
when a grammar rule contained a meta-variable, which the Logtalk
compiler should (but currently don't) report as illegal (the call/1
control construct should be used instead on this case). Thanks to
Nicolas Pelletier for the bug report.

Simplified the compilation of calls in the context of the pseudo-object
"user".

Added a debugger command "p" to the Logtalk built-in debugger. This
command prints the current goal using the print/1 predicate when
available. Renamed the debugger command "q" to "Q". Corrected a bug
where the debugger command "w" would not be recognized. Changed the
debugger command "d" to quote terms when necessary.

Added a partial implementation of the ISO Prolog predicate_property/2
predicate to the ECLiPSe config file.

Updated the SWI-Prolog config file in order to remove the erroneous
declaration of the library meta-predicates dde_register_service/2,
time/1, and when/2 as built-in meta-predicates.

Applied a bug fix to the "flags" contribution made available by its
author, Theofrastos Mantadelis. Workaround a compatibility issue with
GNU Prolog.

Added a memberchk/2 predicate to the library "setp" protocol and "set"
object. Added predicates is_control/1, is_newline/1, and is_end_of_line/1
to the library "characterp" and "character" entities.

Added an experimental example of using attributed variables within
Logtalk objects and categories. Currently requires the use of YAP
or SWI-Prolog as the back-end Prolog compiler.

Added support for indexicals to the "constraints/sicstus" example.

Extended the "delegates" example with a sample implementation of a
delegator using a parametric object.

Extended the "dynpred" example in order to illustrate how to define
category predicates that handle dynamic predicates in the context of
"this" and in the context of "self".

Added a wall_time/1 predicate to the "cc" example.

Updated the syntax highlighting test files, adding missing true/0 and
fail/0 control constructs.


2.42.2 - January 25, 2011
=========================

Updated the Logtalk compiler in order to allow checking for module
predicate properties without requiring importing the predicates into
"user" (note, however, that the module must still be loaded prior to
the compilation of source files calling the module predicates).

Updated the Logtalk compiler in order to check compile-time instantiated
closure arguments on call/2-N goals for common programming errors.

Updated the Logtalk compiler in order to generate warnings for lambda
expressions with variables that are not declared as either free variables
or lambda parameters or with variables that are declared as both free
variables and lambda parameters. Thanks to Jan Burse for suggesting
these sanity checks.

Updated the Logtalk runtime in order to support using the <</2 control
construct in closure meta-arguments of meta-predicates.

Added support for compiling Free/Goal lambda expression calls (by
automatically generating an auxiliary predicate whenever possible).

Added a compile_aux_clauses/1 public method, based on a predicate with
the same name in SWI-Prolog, to the built-in object "logtalk". This
method is usually called from goal_expansion/2 hooks in order to compile
auxiliary clauses generated for supporting an expanded goal.

Added the meta-predicate ignore/1, implemented by some Prolog compilers
as a built-in predicate, as a Logtalk built-in method.

Updated the SWI-Prolog config file in order to prevent the auto-loading
of module libraries when querying predicate properties from Logtalk.

Updated the Qu-Prolog, SICStus Prolog, and XSB config files in order
to make available the predicate term_variables/2 as an ISO built-in
predicate.

Updated the Qu-Prolog integration scripts in order to double the heap size
value for compiling the Logtalk compiler/runtime, solving a startup crash.

Restored the MacOS X command-file for GNU Prolog (moved up from the
"scripts/macosx/command_files/unsupported" folder).

Added predicates findall_member/4-5 to the library object "meta" (based
on Richard O'Keefe's draft Prolog library proposal). Removed declaration
and definition of the predicates callable/1 and ignore/1 from the library
entities "metap" and "meta" (callable/1 is a built-in predicate in all
supported Prolog compilers; ignore/1 is now a Logtalk built-in method).

Added a library object, "meta_compiler", that can be used as an hook
object to expand calls to the meta-predicates defined in the library
object "meta" in order to avoid the meta-call overheads. On those cases
where the meta-predicate call cannot be optimized, the default definition
on the "meta" object is used. Using control constructs as meta-arguments
is not (yet) supported. Work in progress.

Added a protocol, "loggingp", and category and object implementations,
"logging" and "logger", declaring and defining basic functionality for
logging events to files to the library.

Added a protocol, "intervalp", and an object, "interval", declaring and
defining basic temporal interval relations to the library (based on the
James F. Allen Interval Algebra work).

Added a category implementing named integer counters, "counters", to the
library.

Added predicates proper_prefix/2 and proper_suffix/2 to the library
entities "listp", "list", and "difflist".

Added predicate transpose/2 to the library object "pairs".

Added two pseudo-examples for testing purposes, "lambdas_compiled" and
"metapredicates_compiled", that make use of the new library object
"meta_compiler".

Updated the "expansion" example in order to illustrate how to define
and combine hook objects.

Added support for using the SyntaxHighlighter package (version 3.0.83
or later) by Alex Gorbatchev with Logtalk source code.

Added missing syntax coloring of the else/0 conditional compilation
directive to the GNU Source-highlight highlighter support.

Added a note on the Logtalk text editing support available on the free
MacOS X Kod text editor.

Added syntax coloring support for the new ignore/1 built-in method to
all supported text editors and syntax highlighters.


2.42.1 - December 22, 2010
==========================

Added support to the Logtalk compiler for detecting and reporting
missing dynamic/1 and discontiguous/1 predicate directives.

Added a new lint flag, "missing_directives", with a default value of
"warning", to all config files. This flag enables printing of compiler
warnings for missing dynamic/1, discontiguous/1, and multifile/1 predicate
directives.

Added support for new meta-predicate template mode indicators: (/) for a
predicate indicator, [/] for a list of predicate indicators, and [0] for
a list of goals.

Corrected a bug in the Logtalk compiler when checking meta-predicate
clause heads for errors where the "::" meta-argument mode indicator
was still being interpreted as "0".

Corrected a bug in the handling of closures (in meta-calls) that are
used to construct calls to control constructs or built-in predicates
which could result in calling some meta-arguments in the wrong context.
Thanks to Ulrich Neumerkel for the illustrating example in the SWI-Prolog
mailing list.

Updated the Logtalk compiler in order to avoid printing warnings about
missing references to the "expanding" and "monitoring" built-in protocols
when compiling modules as objects.

Reverted the changes in version 2.39.2 that allowed the pseudo-object
"user" to virtually contain the definition of all Prolog built-in
predicates. This could lead to different results depending on the
back-end Prolog compiler, specially when calling meta-predicates in
the context of "user".

Corrected a bug in the SWI-Prolog "swihooks.lgt" file that resulted in
a bogus error message about an invalid Logtalk flag when consulting a
Prolog file when there is a Logtalk source file with the same name in
the same directory.

Added missing support for proprietary built-in meta-predicates to the
B-Prolog, SICStus Prolog, SWI-Prolog, XSB, and YAP config files.

Updated the SICStus Prolog, SWI-Prolog, and YAP config files in order to
override the meta-predicate templates for the consult/1 and load_files/2
built-in predicates.

Added an implementation of a map_reduce/5 meta-predicate to the library
object "meta".

Added an implementation of a command_line_arguments/1 predicate to the
"cc" example.

Updated the "lambdas" and "metapredicates" examples, illustrating how to
calculate Fibonacci numbers using a fold left meta-predicate. Updated the
"metapredicates" example with an usage example of the map_reduce/5 meta-
predicate.

Updated the "problog" example, adding preliminary support for the new
annotated disjunctions syntax.

Added a "magic" element to the Logtalk mime-type file for the
freedesktop.org shared mime-info database.


2.42.0 - December 1, 2010
=========================

Added an implementation of persistent object flags, contributed by
Theofrastos Mantadelis.

Added a new pair of built-in predicates, conforms_to_protocol/2-3,
implementing the transitive closure of the protocol implementation
relation.

Added an experimental directive, annotation/1, for supporting Logtalk
integration with other languages such as ProbLog and CHR.

Reverted the changes in the previous release that allowed a stand-alone
object to (also) be used as root of class hierarchies.

Defined a set of low-level utility predicates, available as public methods
from the "logtalk" built-in object. Modified the (virtual) compilation of
this object in order to always interpret it as a prototype.

Simplified and improved the performance of the predicate lookup caching
code, resulting in improved dynamic binding performance. Simplified and
improved the performance of the built-in methods asserta/1 and assertz/1
when asserting a clause for a new predicate. Simplified the updating of
the internal table for dynamically defined predicates when using the
built-in methods retract/1 and retractall/1. Simplified the compiler
code used when creating new entities.

Changed the semantics of the "dynamic_declarations" flag so that it is
only checked when sending an asserta/1 or assertz/1 message to an object.
Local asserting of clauses for new predicates is now always allowed.

Updated the implementation of the built-in method abolish/1 in order to
also support abolishing of local dynamic predicates.

Improved the performance of source file compilation by avoiding redundant
cleaning of auxiliary compilation predicates.

Modified the Logtalk compiler in order to interpret occurrences of ":" in
module meta-predicate directives as equivalent to "::" instead of "0" in
Logtalk meta-predicate directives. This change follows the current trend
for de facto standardization for meta-predicate directives but is still
problematic for some Prolog compilers. Logtalk will throw an exception
when compiling calls to Prolog built-in meta-predicates or Prolog module
meta-predicates whose template includes a ":" meta-argument specifier.

Improved compiler error checking for entity relations, detecting and
reporting most incorrect attempts to extend, instantiate, specialize,
import, implement, or complement the wrong kind of entities.

Implemented a parameter_names/1 property for parametric categories
and parametric objects. Implemented a public/1 property for objects,
protocols, and categories.

Corrected a bug where the compilation (or dynamic creation) of parametric
objects or parametric categories with instantiated parameters would fail
to discard the parameter values in the runtime tables of defined entities.

Added support for using explicit object qualification and explicit module
qualification in meta_predicate/1 directives.

Updated the implementation of the directives dynamic/1, discontiguous/1,
and multifile/1 in order to remove explicit module qualification from the
predicate indicator arguments when the module is "user".

Updated the compilation of clause heads for module multifile predicates
in order to remove the explicit module qualification when the module is
"user".

Updated the implementation of the built-in predicate logtalk_compile/2
in order to ensure the same compiler options semantics for compiled code
as the built-in predicate logtalk_load/2.

Avoid a spurious choice-point created when reporting singleton variables
during source file compilation with some back-end Prolog compilers such
as ECLiPSe and SWI-Prolog.

Corrected a error handling issue in the implementation of the built-in
predicates logtalk_compile/2 and logtalk_load/2 where the error terms
would be wrapped in an extra error/2 term.

Updated the Logtalk compiler in order to also delete the intermediate
files generated by the back-end Prolog compilers when compiling the
Prolog files generated by Logtalk with the "clean" flag is set to "on".

Updated the built-in objects, "logtalk" and "debugger", and the built-in
protocols, "expanding" and "monitoring", to support static binding.

Updated the term-expansion mechanism implementation in order to support
the use of the {}/1 compiler bypass control construct to wrap elements
in the list of expanded terms (returned by the term_expansion/2 built-in
method when used in hook objects), to allow expansion of the end_of_file
term, and to throw an instantiation error when term-expansion generates
a list containing an element that is a variable.

Interpret "prolog_compatible_version", "tabling", and "coinduction" as
read-only flags. Removed documentation on the deleted "multifile_directive"
flag. Added missing documentation on the "prolog_compatible_version" flag.

Added support for accessing the compiler input stream using the built-in
predicate logtalk_load_context/2 with the key "stream".

Improved compile-time and runtime error handling for the blackboard
built-in predicates.

Updated the implementation of the built-in predicate set_logtalk_flag/2
in order to switch off the clean (smart_compilation) flag when switching
on the smart_compilation (clean) flag.

Recognize logtalk_library_path/2 as a Logtalk built-in predicate when
compiling source files.

Corrected a bug where abolishing a dynamic category or a dynamic protocol
compiled in debug mode would fail to update the internal table of entities
compiled in debug mode.

Corrected a bug in the processing of meta-calls when the called goal is
an explicit message to an object where the receiver instead of the sender
would be tested for events support.

Corrected a static binding bug where meta-predicates called within other
meta-predicates would not be correctly compiled. This bug prevented the
"mtbatch" example from running.

Corrected a bug in the Logtalk compiler second pass where processing
calls to non-standard Prolog built-in meta-predicates would fail to
test for Logtalk built-in predicates and Logtalk built-in methods with
the same predicate indicator.

Removed references to the de facto standard predicates retractall/1,
compare/3, and callable/1 from all the config files of Prolog compilers
where these predicates are built-in.

Added declaration of file name extensions for intermediate files generated
by the Prolog compilers themselves to the config files of B-Prolog, Ciao,
Qu-Prolog, and XSB.

Updated the config files of SICStus Prolog, SWI-Prolog, and YAP in order
to parse the Prolog proprietary directive public/1 (whose semantics depend
on the Prolog compiler).

Updated the Qu-Prolog config file with support for accessing environment
variables and updated definitions of file-system access predicates that
avoid issues with file paths containing spaces. Logtalk now requires
Qu-Prolog 8.12 or a later version.

Updated the GNU Prolog config file in order to support the built_in_fd/0,
built_in_fd/1, built_in/0, built_in/1, and ensure_linked/1 proprietary
directives.

Workaround a B-Prolog 7.4 bug where the built-in predicate callable/1 is
missing the property "built_in". Declared the predicates and/3, equiv/3,
and or/3 as built-in predicates. Added support for table/1 directives
whose argument is predicate template with the cardinality limit omitted.

Updated the Qu-Prolog integration files in order to avoid a segmentation
fault when using a 32 bits compilation of Qu-Prolog. Thanks to Peter
Robinson for his help in solving this problem.

Added a YAP integration file, "yaphooks.pl", defining the hook predicate
user:prolog_predicate_name/2.

Commented out the setting of the proprietary Prolog flag "iso" to "true"
in the SWI-Prolog config file due to all the module libraries that fail
to compile/work in "iso" mode. Updated the implementation of the internal
predicate '$lgt_expand_path'/2 in order to also expand directory paths.
Added a workaround for ignoring the new operator public/1 introduced in
SWI-Prolog 5.11.9 while compiling Logtalk source files.

Renamed the SWI-Prolog integration files, "swihook.pl" and "xpcehook.pl"
to, respectively, "swihooks.pl" and "xpcehooks.pl".

Added a definition for the hook predicate user:prolog_predicate_name/2 to
the SWI-Prolog hook file, "swihooks.pl". This hook predicate allows a more
user-friendly experience when using the SWI-Prolog profiler with Logtalk.
Requires SWI-Prolog 5.10.2 (stable) or 5.11.8 (development) or later
versions. Thanks to Jan Wielemaker for his support on improving the usage
of the user:prolog_predicate_name/2 hook on the SWI-Prolog profiler.

Updated the library "types_loader.lgt" file to also load the heaps library
files.

Added an example, "diagrams", illustrating how to generate entity diagrams
for a source file and for a library of source files using Logtalk reflection
features and the DOT language.

Added an example, "profilers", featuring simple wrappers for the SICStus
Prolog 4 profiler and the YAP count profiler.

Added an example, "delegates", illustrating an implementation of the
delegation design pattern.

Added a highly experimental example, "chr", of CHR integration. Currently
requires the use of Qu-Prolog, SICStus Prolog, SWI-Prolog, or YAP as the
back-end Prolog compiler.

Added an experimental example, "constraints/sicstus", illustrating how
to use the SICStus Prolog CLP(FD) library within objects and categories.

Added individual examples of using B-Prolog Action Rules within objects
to the "constraints/bp" example.

Updated the "cc" example, adding support for Qu-Prolog, adding an
operating_system_type/1 predicate, and correcting the declaration
of the predicate shell/1.

Updated the "help" example, allowing it to be loaded when using any
supported back-end Prolog compiler. In addition, when help is requested
for a functor that is both a built-in directive and a built-in predicate,
both web pages will be open.

Updated the "benchmarks" example in order to use more realistic tests for
the database built-in predicates and the methods assertz/1 and retract/1.

Updated the "operators" example in order to avoid a unit test failure
when reloading the "triple.lgt" source file.

Updated the "modules" example in order to use the use_module/2 directive
(which is supported by Logtalk) instead of the use_module/1 directive
(which is only supported for some back-end Prolog compilers) in the
"client" module.

Updated the "problog" example. Logtalk integration with ProbLog is much
improved but requires the latest development version of ProbLog. Thanks
to Theofrastos Mantadelis for his support on ProbLog.

Added a fully connected graph path search problem, contributed by
Theofrastos Mantadelis, to the "benchmarks" example.

Corrected a bug in the use of the built-in predicate object_property/2
in the "roots" example that would result in errors when deleting dynamic
objects.

Updated the "examples/tester.sh" script in order to also work when
running Logtalk without using the installer packages or scripts.

Updated the "build_release.sh" script in order to calculate all the
necessary checksums when building the MacOS X installer package.

Corrected a bug in the "logtalk_backend_select" shell script that
prevented selecting GNU Prolog as the default back-end Prolog compiler.


2.41.1 - October 6, 2010
========================

Modified the compilation of objects that don't instantiate, specialize,
or extend other objects in order to allow their use as roots of either
class hierarchies or prototype hierarchies. This change simplifies the
definition of class hierarchies when reflexive designs are not required.

Added support for the blackboard built-in predicates (bb_put/2, bb_get/2,
bb_delete/2, and bb_update/3) found on some Prolog compilers such as YAP
and SICStus Prolog. Note that a Prolog compiler natively supporting these
predicates is required. This support aims to facilitate porting of Prolog
applications using the blackboard; these predicates should be avoided when
writing portable Logtalk applications.

Simplified the internal representation of entity prefixes and entity
predicate functors. The new representation is also a bit more compact.

Corrected a bug in the compilation of clauses for multifile predicates
defined in the pseudo-object "user" where the execution context for the
clause body goals would be undefined.

Corrected an optimization bug in the compilation of grammar rules that
resulted in failed compilations whenever the body of a grammar rule was
equivalent to fail/0. Thanks to Ulrich Neumerkel for the bug report.

Corrected an optimization bug in the compilation of grammar rules that
resulted in incorrect compilations when folding pairs of consecutive
variable unifications generated as a by-product of the translation of
grammar rules into clauses. Thanks to Ulrich Neumerkel for the bug report.

Corrected a bug in the table of ISO Prolog specified arithmetic functions
that is used by the Logtalk compiler when checking arithmetic expressions
for portability (wrong arity of the bitwise complement function).

Corrected a bug in the compilation of calls to Prolog proprietary built-in
meta-predicates that are redefined within an object or category.

Corrected a race condition bug when running multi-threaded code compiled
in debug mode that could result in bogus failures.

Corrected a bug in the definition of the built-in object "logtalk" when
used as a root for a class hierarchy (two missing linking clauses for the
implemented protocols).

Set the default value for the Logtalk flag "code_prefix" to '$' for all
supported back-end Prolog compilers.

Updated the ECLiPSe config file in order to support compilation of the
proprietary built-in predicate set_error_handler/2 and correct the buggy
compilation of the proprietary built-in predicate set_event_handler/2.
These two built-in predicates are only supported, however, when used as
directives.

Updated the SICStus Prolog, SWI-Prolog, and YAP config files in order
to correct a bug in the compilation of the ensure_loaded/1 directive
when used within a Prolog module being compiled as an object.

Updated the YAP config file to workaround a bug in the built-in predicate
absolute_file_name/3 where a directory can be returned when looking for a
regular file (fixed in the current beta of the 6.0.7 version).

Restored the hacks in the SWI-Prolog and YAP config files, broken in
the previous release, that allow calling the Prolog built-in predicates
phrase/2-3 with a Object::GRBody goal in the first argument.

Updated the SWI-Prolog config file in order to use the numbervars/3 option
singleton(true) in the auxiliary predicates for pretty printing non-ground
terms.

Added predicates intersection/4 and union/4 to the library support for
ordered sets (protocol "setp" and object "set").

Added a highly experimental example of ProbLog integration. Requires
Logtalk to be run with YAP; it may also require a patched version of
the ProbLog distribution.

Updated the "multifile" example to illustrate how the body of clauses for
multifile predicates is compiled.

Removed some unused files from the "coinduction" example.


2.41.0 - September 15, 2010
===========================

Added experimental support for coinductive predicates to the Logtalk
compiler using a coinductive/1 directive. Requires a back-end Prolog
compiler providing minimal support for cyclic terms (currently, YAP,
CxProlog, ECLiPSe, SICStus Prolog, and SWI-Prolog). Thanks to Gopal
Gupta, Neda Saeedloei, Feliks Kluzniak, Ajay Bansal, and Vitor Santos
Costa for feedback and implementation suggestions.

Added a "coinductive" predicate property.

Added a "coinduction" read-only flag.

Added a new Logtalk built-in predicate, logtalk_load_context/2, similar
to the prolog_load_context/2 built-in predicate found in several Prolog
compilers, in order to provide access to the compilation/loading context.
The initial set of supported context keys is {entity_name, entity_prefix,
entity_type, file, directory, term_position}. The term_position key is
only supported in back-end Prolog compilers that provide the start and
end lines of a read term.

Simplified the Logtalk compiler by using stream aliases.

Updated the implementation of the create_object/4 and create_category/4
built-in predicates in order to also support the definition of an initial
set of grammar rules.

Corrected a bug in the runtime error handler that would result in entity
existence errors being reported as (internal) predicate existence errors.

Corrected a bug where the protocols implemented by the built-in object
"logtalk" would not be returned by the implements_protocol/2-3 built-in
predicates.

Corrected a bug where multifile/1 directives would only accept a single
predicate indicator as argument.

Corrected a bug where compiling an explicit module-qualified module meta-
argument would result in a compilation loop when the qualified goal is a
variable.

Ciao Prolog 1.10 is no longer supported as this compiler fails to provide
support for ISO Prolog standard stream aliases and the stream_property/2
built-in predicate. Support for this Prolog compiler will be reevaluated
when a new stable version becomes available.

ECLiPSe 6.0#141 or a later version is now required when using this
back-end Prolog compiler due to use of stream aliases in the Logtalk
compiler.

Updated the CxProlog config file to require version 0.97.5 or later. This
version adds the epsilon/0 arithmetic constant that is used in the library
unit test support.

Updated the SWI-Prolog and YAP config files in order to support the
expects_dialect/1 proprietary directive.

Removed the definitions of the no longer necessary '$lgt_keysort'/2 and
'$lgt_sort'/2 internal predicates from the supported config files.

Updated the shell scripts used for generating the PDF versions of the
Logtalk User and Reference Manuals for better performance and to fix an
incompatibility with recent versions of the xsltproc XSLT processor.

Updated the "library/lgtunit_loader.lgt" helper file to avoid entity
redefinition warnings when loading the "library/all_loader.lgt" helper
file from the "library" directory itself.

Added an example, "adventure", with Logtalk adaptations of Prolog text
adventures originally written by David Matuszek and Dan Cliburn.

Added an experimental example of coinduction when using CxProlog, ECLiPSe,
SICStus Prolog, SWI-Prolog, or YAP as the back-end Prolog compiler.

Updated the "constraints/swipl/loader.lgt" helper file to avoid the errors
generated when loading the CLP(FD) library with the SWI-Prolog "iso" flag
set to "true".

Updated the "dcgs" example unit tests to avoid a compilation error when
using SWI-Prolog 5.11.3 and later versions due to changes in this compiler
parser for stricter ISO Prolog syntax.

Updated the "help" example in order to provide help for Logtalk built-in
directives.

Updated the "examples/tester.sh" script in order to fix a compatibility
issue with XSB.

Added support for the SHJS syntax highlighter.

Added syntax coloring support for the new coinductive/1 predicate
directive to all supported text editors and syntax highlighters.

Updated the TextMate commands used for generating (X)HTML, PDF, and TXT
documentation in order to be independent of the default settings for the
"altdirs" and "xmldir" compiler flags.


2.40.1 - June 30, 2010
======================

Restored support for GNU Prolog. Requires GNU Prolog version 1.4.0 (or
later), which provides support for the ISO Prolog predicate directive
multifile/1.

Improved the built-in debugger by making explicit the calls to the (\+)/1
built-in predicate.

Corrected a bug where a call to a local predicate would always fail when
an object or category contains a declared but not defined non-terminal.

Corrected a bug in the compilation of (\+)//1 calls in grammar rules.
Thanks to Bart Demoen, Ulrich Neumerkel, and Jan Burse for driving my
attention to this bug.

Recognize retractall/1, ground/1, keysort/2, sort/2, and numbervars/3 as
de facto Prolog standard built-in predicates. Calls to these predicates
will no longer be reported when the portability flag is set to "warning".
In addition, added syntax coloring support for these predicates.

Recognize e/0 and pi/0 as de facto Prolog standard built-in arithmetic
functions. Calls to these functions will no longer be reported when the
portability flag is set to "warning". In addition, added syntax coloring
support for these two functions.

Updated the internal hook predicate '$lgt_tr_predicate_indicators'/2,
used in the config files for processing proprietary Prolog directives,
in order to translate both grammar rule non-terminal indicators and
predicate indicators. A possible usage scenario is tabling grammar rule
non-terminals.

Updated the definition of the Logtalk flag "prolog_version" for Ciao in
order to compute major, minor, and patch version numbers.

Corrected a bug in the goal_expansion/2 clauses found on the SWI-Prolog
and YAP config files that are used for expanding calls to the Prolog
built-in predicates phrase/2-3 with a Object::GrammarRuleBody in the
first argument.

Updated the B-Prolog config file to require version 7.4 or later and
to workaround a missing "built_in" property for the built-in predicate
setup_call_cleanup/3.

Added unit tests for the "securemp" and "constraints/gprolog" examples.

Improved the "debug_hooks" example, showing how to use a parametric hook
object in alternative to two simple hook objects.

Improved the "help" example by adding a helper predicate for quick access
to the Logtalk User and Reference manuals. Added support for CxProlog.

Added partial support for CxProlog to the "cc" example in order to support
the "help" example.

Updated the "multifile" example in order to avoid compilation errors when
using a back-end Prolog compiler (such as B-Prolog) that doesn't support
discontiguous predicates.

Improved the "examples/tester.sh" script in order to print the current
time and date, plus the Logtalk and back-end Prolog compiler versions,
and to abstract the different syntax requirements for initialization
goals used by Prolog compilers.


2.40.0 - June 16, 2010
======================

Bundled version 1.0 of Verdi Neruda, a meta-interpreter collection that
includes both top-down and bottom-up search strategies. Thanks to Victor
Lagerkvist for his contribution.

Added experimental on-line help support using the new "help" example. See
the example "NOTES.txt" file for a list of current operating-system and
back-end Prolog compiler limitations.

Changed the representation of meta-argument which are goals in the
meta_predicate/1 directives from "::" to "0". When compiling calls
to Prolog module meta-predicates, the meta-argument indicator ":" is
interpreted as "0", which will work for most cases. Due to the lack of
standardization of the Prolog meta_predicate/1 directive, special care
must taken when calling both Prolog proprietary built-in meta-predicates
and Prolog module meta-predicates.

Changed the implementation of the term and goal expansion mechanisms in
order to only be performed automatically by the Logtalk compiler when
compiling a source file. When dynamically creating entities at runtime or
when asserting clauses, the term_expansion/2 and goal_expansion/2 built-in
methods must be called explicitly.

Corrected a bug when using the ::/2, ::/1, ^^/2, and :/1 Logtalk control
constructs from bagof/3 and setof/3 calls with existentially quantified
variables. This bug could result in missing alternative solutions when
backtracking over the bagof/3 and setof/3 calls. Thanks to Victor
Lagerkvist for his help in diagnosing the problem.

Updated the Logtalk compiler implementation in order to simplify passing
of compilation context data.

Updated the Logtalk compiler in order to throw a permission error if the
user attempts to define clauses for the (>>)/2 control construct used in
lambda expressions.

Updated the Logtalk compiler in order to preprocess proprietary Prolog
directives within entities (using any defined config file hooks) after
term-expansion but before trying to compile the directives as Logtalk
directives. This change allows for simpler and more robust handling of
Prolog proprietary directives, specially module directives.

Recognize callable/1 and compare/3 as Prolog standard built-in predicates.
Calls to these predicates will no longer be reported when the portability
flag is set to "warning". In addition, added syntax coloring support for
these two predicates.

Added support for the built-in non-terminal call//1 specified in the ISO
Prolog standardization proposal for Definite Clause Grammars.

Improved the implementation of Definite Clause Grammars in order to
rewrite exceptions generated when processing NonTerminal, ::NonTerminal,
and Obj::NonTerminal goals in grammar rules to refer to non-terminals
instead of compiled predicate forms when running in debug mode.

Changed the scope of the built-in methods phrase/2-3 from public to
private for consistent meta-predicate semantics. Updated the "dcgs"
example and the corresponding unit tests in order to comply with the
new phrase/2-3 predicate scope.

Corrected a bug in the implementation of the phrase/2-3 built-in methods
that could allow access to non-visible grammar rules.

Improved performance of Module:NonTerminal goals in grammar rules.

Corrected a bug in the implementation of the dynamic/0 directive where
the Logtalk compiler would fail to generate all the necessary dynamic/1
predicate directives. Bug introduced in the previous stable release.

Corrected a bug in the implementation of the clause/2 built-in method
where an error would be generated when trying to access clauses for
local dynamic predicates without a corresponding dynamic/1 directive
within objects and categories with a dynamic/0 directive.

Corrected a bug in the compilation of meta-calls and the <</2 control
construct in debug mode where the meta-arguments would not be compiled
in debug mode.

Corrected a silly bug in the compilation of proprietary Prolog directives
where meta-arguments would be compiled in debug mode if the "debug" flag
as "off" and in normal mode of the "debug" flag was "on".

Corrected a bug in the processing of compiler options when recursively
calling the logtalk_load/2 and logtalk_compile/2 built-in predicates with
a list of source files which could result in failure to apply the compiler
options to the compilation of all files.

Updated the ECLiPSe config file to always use atoms instead of strings in
the file-system access internal predicates. Corrected some bugs in the
processing of proprietary directives.

Added goal_expansion/2 clauses to the SWI-Prolog and YAP config files in
order to allow calling the Prolog built-in predicates phrase/2-3 with a
Object::GrammarRuleBody in the first argument.

Corrected the definitions of the predicates '$lgt_current_date'/3 and
'$lgt_current_time'/3 in the Qu-Prolog config file and rename it to
"qp.pl". Updated the minimum required version to 8.11 and changed the
value of the Logtalk flag "prolog_dialect" from "qu" to "qp" in order
to match the value of the new Prolog flag "dialect" implemented in
Qu-Prolog 8.11. Added support for the "smart_compilation" compiler flag.
Improved startup time when using the "qplgt.sh" integration script. 

Corrected a typo in the template of the predicate subsumes/2 in the
library protocol "termp".

The SWI-Prolog POSIX integration shell script, "swilgt", now tries
first to use a "swipl" executable and, if not found, tries to use
instead a "pl" executable (thus preferring a recent SWI-Prolog version
over an older one).

Added man pages for the POSIX "*lgt" Prolog integration shell scripts,
for the Logtalk management shell scripts, and for the XML documenting
files processing scripts.

Renamed the "logtalk_select" shell script to "logtalk_version_select".
Renamed the "cplgtdirs.*" scripts to "logtalk_user_setup.*".

Include a XHTML version of the library documentation on the directory
"library/docs".

Improved the "examples/tester.sh" script in order to support running the
example unit tests compiled in debug mode.

Added a a simple maze search problem using a depth-first strategy with
loop detection to the "benchmarks" example.

Corrected a wrong declaration for the predicate environment_variable/2
in the "cc" example. Solved portability issues with Ciao Prolog, ECLiPSe,
and XSB.

Added missing support for syntax coloring of the "complements" entity
relation to Pygments.


2.39.2 - May 4, 2010
====================

Changed the internal representation of most entity properties to use a
more compact bit representation, stored in the tables of loaded entities.

Changed the internal representation of non-critical predicate properties
to use a more compact bit representation, which includes the "multifile"
predicate property, stored in the tables of entity declared predicates.

Changed the type of the built-in methods (\+)/1, call/N, once/1, catch/3,
throw/1, bagof/3, findall/3, forall/2, setof/3, self/1, sender/1, this/1
and parameter/2 from local predicates to private predicates. Corrected a
bug in the predicate_property/2 built-in method where some of those built-
in methods would be reported as public. These changes should only require
updating any application code that explicitly expected the exception term
permission_error(access, local_predicate, Pred), which have been changed
to permission_error(access, private_predicate, Pred).

Added support for parsing operator declarations in the uses/2, public/1,
protected/1, and private/1 directives.

Updated the Logtalk compiler in order to avoid generating unnecessary (but
harmless) catchall clauses for predicate declaration lookups when there
are no local predicate declarations.

Updated the Logtalk compiler in order to convert calls to the non-standard
assert/1 built-in predicate as calls to the assertz/1 built-in predicate.

Updated the Logtalk compiler in order to reject use_module/2 directives
where the first argument is not an atom (assumed to be the module name).
This change results in using the config files to find the module name from
the module file name when the first argument of the use_module/2 directive
is a compound term (as usual when using library notation).

Updated the definition of the pseudo-object "user" in order to virtually
contain the definition of all Prolog built-in predicates.

Updated the definition of the built-in object "logtalk" in order to
implement the built-in protocols "expanding" and "monitoring".

Updated the "expanding" built-in protocol in order to declare the
predicates goal_expansion/2 and term_expansion/2 as both dynamic and
multifile.

Corrected a bug in the Logtalk compiler where a misleading error would be
reported when a source file contains a clause or a directive that is a
plain variable.

Corrected a bug in the Logtalk compiler where a broken term_expansion/2
definition expanding a term to a variable would succeed instead of
throwing an error.

Optimized meta-calls when using closures wrapped in the {}/1 control
construct. Removed redundant check for debugging mode when executing
meta-calls.

Corrected a potential bug in the Logtalk runtime when updating the meta-
call context for lambda expressions that could result in losing part of
the context.

Corrected a bug in the Logtalk runtime where errors generated when calling
the phrase/2 built-in method would be reported as resulting from calls to
the phrase/3 built-in method.

Corrected a bug in the implementation of the predicate_property/2 built-in
method that allowed access to properties on non-visible predicates.

Updated the predicate_property/2 built-in method in order to generate
meta-predicate templates for the call/2-N built-in methods, to return
"multifile" properties for Prolog built-in predicates, and to support
two new properties, "logtalk" and "prolog", that allows us to distinguish
between predicates defined by Logtalk and predicates defined by the back-
end Prolog compiler.

Corrected a bug in the Logtalk built-in debugger that would prevent user
interaction at the "fact" and "rule" unification ports by always skipping
to the next port, ignoring leash settings.

Corrected a bug in the processing of the conditional directives that would
result in an hanging else/0 directive when the "then" block of a false "if"
was a "if...else...endif" block.

Added support for the predicate renaming operator as/2 found on SWI-Prolog
and YAP but only when parsing use_module/2 directives in modules that are
being compiled as objects.

Updated the SWI-Prolog hooks file, "configs/swihook.pl", in order to avoid
intercepting Prolog ensure_loaded/1 calls by the prolog_load_file/2 hook
predicate. This change solves issues when calling Prolog module library
predicates from within Logtalk objects and categories. Thanks to Parker
Jones for the bug report.

Changed the predicate '$lgt_pl_meta_predicate'/2 in all supported config
files to '$lgt_pl_meta_predicate'/3 in order to simplify the handling of
proprietary built-in meta-predicates by the Logtalk compiler and runtime.

Updated the ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP config files in
order to preprocess the use_module/2 directive, converting the module file
name into the module name.

Updated the SWI-Prolog and YAP config files in order to preprocess the
reexport/2 module directive, converting the module file name into the
module name, and in order to ignore the meta-predicate declaration of
the directives/predicates use_module/1-2.

Updated the SWI-Prolog config file with more robust and fast code for
preprocessing module directives when the modules are already loaded.
Also added support for the new use_foreign_library/1-2 proprietary
directives and partial support for modules that take their name from
the file name.

Updated the YAP config file in order to use the more standard predicate
working_directory/2.

Added support for parsing use_module/3 directives to the SICStus Prolog
config files. Improved finding the module name from the module file name
when preprocessing the directives use_module/1-2.

Moved the "eclipse6.pl" config file to the "configs/unsupported" folder
as Logtalk only supports ECLiPSe using the "eclipse6iso.pl" config file.
Updated the "eclipse6iso.pl" config file with partial parsing solutions
for some proprietary ECLiPSe directives.

Updated the ECLiPSe config file with more robust code for parsing
use_module/1 directives, capable of processing both ECLiPSe module/1
directives and the module/2 directives used by other Prolog compilers.

Updated the B-Prolog config file in order to support foreach/2-N calls
for any valid value of N (previous versions of the config file only
supported foreach/2-4 calls).

Updated the Qu-Prolog integration script, "qplgt.sh", in order to use
larger memory values for the code and environment areas an in preparation
for its next release.

Replaced the predicate as_dictionary/2 in the library object "bintree"
with a new implementation, contributed by Paul Fodor, that returns a
semi-balanced tree.

Added a predicate and an operator definition, =~=/2, for comparing floats
for approximate equality to the library object "lgtunit". Updated the unit
tests of several examples in order to use this predicate.

Updated the "examples/tester.sh" script in order to report an error when
the default Prolog back-end compiler is not found. Thanks to Parker Jones
for the bug report. Updated the script to also report errors that are not
written to standard error and to print a note when a unit test is not
applicable.

Updated the "encodings" example unit test loader file to correctly detect
back-end Prolog compilers supporting the encoding/1 directive. Added new
test files for each supported text encoding.

Added some more unit tests for the "reflection" example.

Fixed a typo that prevented running the "mtbatch" example using XSB as the
back-end Prolog compiler. 

Simplified the "cc" example when using SICStus Prolog 4.x as the back-end
compiler by moving importing of the "system3" library shell/1-2 predicates
to the config file.

Added unit tests for the "constraints", "hooks", "miscellaneous", and
"people" examples.

Corrected a bug in the "lambdas" example (a missing meta_predicate/1
directive in the object "sigma").

Improved "inheritance" example in order to better illustrate inheritance
semantics for classes and instances. Added a set of unit tests.

Updated the implementation of the best first heuristic state space search
method in the "searching" example in order to avoid unnecessary meta-calls.


2.39.1 - March 31, 2010
=======================

Updated the Logtalk compiler and runtime to support calls to the built-in
predicates threaded_peek/2 and threaded_exit/2 where the second argument
(thread tag) is only instantiated at runtime. Updated the exception term
thrown by threaded_exit/2 when the thread goal don't exist to use error/3
instead of error/2.

Updated the implementation of the uses/2 directive in order to allow
safe use of the declared shortcuts at runtime.

Corrected a bug when using the use_module/2 directive that resulted in
a performance penalty when calling module predicates.

Updated the Logtalk compiler to warn the user when finding deprecated
directives.

Updated the Logtalk compiler to recognize "prolog_compatible_version"
and "tabling" as valid flags.

Updated the Logtalk compiler to compile settings files with the compiler
flag "altdirs" turned off in order to avoid problems with directory
permissions for back-end Prolog compilers, such as Ciao and XSB, which
require the first run of Logtalk to be made by an administrative user.

Corrected a bug in the Logtalk compiler that forced recompilation of
source files when the "smart_compilation" flag was set to "on".

Corrected an erroneous compiler warning when reporting referenced but
unknown objects when using the {Object}::Message notation. Thanks to
Parker Jones for the bug report.

Corrected a bug in the <</2 control construct where errors would be
reported without an error/3 wrapper. Thanks to Parker Jones for the
bug report.

Corrected a predicate lookup caching bug for the ^^/1 control construct
(aka "super") that resulted in cached entries never being used when the
argument was a different predicate from the one being defined by the
clause containing the call (uncommon but possible).

Updated the SWI-Prolog and YAP config files in order to recognize and
compile the proprietary directive create_prolog_flag/3.

Updated the ECLiPSe config files to treat numbervars/3 as a built-in
predicate.

Updated the file system utility predicates in the XSB config file in
order to ensure full expansion of paths into full paths.

Updated the CxProlog config file to workaround a wrong priority for the
standard **/2 operator ("xfx" instead of "xfy"). Updated the CxProlog
integration scripts for both POSIX and Windows systems in order to use
the "--script" command-line option, thus freeing the "--goal" option.

Added a logtalk_library_path/2 entry for the library "home" (the user
home directory) when running on Windows operating-systems. However, 
this definition only works for some back-end Prolog compilers due to
bugs in expanding paths containing more than one environment variable.

Updated the "types_loader.lgt" library loader file in order to load the
"rbtree" library object.

Updated the library object "term" in order to implement the variant/2
predicate as a call to the =@=/2 proprietary built-in predicate when 
using either SWI-Prolog or YAP as the back-end Prolog compilers. 

Corrected a predicate property name bug (alias/1 instead of alias_of/1)
in the library object "listing".

Replaced the library object "lgtunit" with a new version supporting three
different dialects for specifying unit tests. See the example "testing"
and the library file "lgtuint.txt" for details.

Added unit tests for most examples, based on the sample queries found
on each example "SCRIPT.txt" files. The original tests, contributed by
Parker Jones, were rewritten to use the updated version of the "lgtunit"
library. Added a POSIX shell script, "examples/tester.sh" for automating
running the examples unit tests.

Added a missing file, "lists.lgt", to the "metainterpreters" example.

Added shell/1-2 predicates to the operating-system interface defined in
the "cc" example.

Added an op/3 directive for the :/2 operator to the "logging" example in
order to avoid syntax errors when running on a back-end Prolog compilers
that don't define this operator.

Updated the "modules" example in order to avoid an object name conflict
with the library object "list".

Corrected assorted typos on the sample queries of several examples. Thanks
to Parker Jones for the bug reports.

Updated several examples sample queries to workaround a XSB parsing bug
triggered by using message broadcasting syntax.

Updated the Windows installer in order to warn the user when no compatible
Prolog compiler can be found. In addition, update the installer in order
to also create shortcuts to the main documentation files on the Logtalk
user folder.

Added an experimental "logtalk_backend_select.sh" POSIX shell script for
defining an alias, "logtalk", to a chosen back-end Prolog integration
script. Suggested by Parker Jones.

Documentation cleanup, specially regarding installing, customizing, and
running Logtalk. Simplified Prolog compatibility notes as the current set
of compatible Prolog compilers provide a more uniform experience.

Corrected a bug in the POSIX lgt2*.sh scripts that would prevent finding
the Logtalk documenting XML files when using a XML Schema specification.
Thanks to rbt for the bug report.


2.39.0 - February 28, 2010
==========================

Removed support for the following Prolog compilers: ALS Prolog, Amzi!
Prolog, BinProlog, GNU Prolog, IF/Prolog, JIProlog, K-Prolog, LPA
MacProlog, LPA WinProlog, MasterProlog, Open Prolog, Prolog II+, and
Quintus Prolog. Only Amzi! Prolog, GNU Prolog, and Quintus Prolog seem
to be actively maintained. Amzi! Prolog ignores the official and de
facto Prolog standards. GNU Prolog is currently missing crucial support
for the standard directive multifile/1. Quintus Prolog is superseded by
SICStus Prolog (and required hacks to the Logtalk compiler that rendered
it incompatible with all the other Prolog compilers).

Removed support for all Prolog compilers that don't support the ISO Prolog
standard directive multifile/1. Removed the "multifile_directive" config
flag from the config files of supported Prolog compilers.

Reintroduced parametric categories. Changed semantics of the parameter/2
built-in execution-context method when used in category predicates to
access the category parameters instead of the parameters of the object
importing the category. This change provides safer and more robust use
of parameter/2 calls in categories. 

Improved compile-time error-checking of calls to the parameter/2 built-in
execution-context method.

Removed the meta-predicate compilation safety rule that required the
number of normal arguments to be equal or greater than the number of
extra arguments required by a closure. This overly restrictive rule
prevented the definition of some useful and safe meta-predicates.

Moved all config files for no longer supported Prolog compilers to the
"configs/unsupported" folder. Moved all integration scripts for no longer
supported Prolog compilers to the "integration/unsupported" folder.

Changed the default value of the compiler flags "altdirs" and "clean" to
"on" for all supported Prolog compilers. Added a read-only compiler flag
"modules".

Added index/1 directives for the predicate lookup caching predicates to
the SWI-Prolog integration files (see "integration/logtalk_comp_swi.pl").
These directives may or may not improve performance, depending on your
application.

Added meta-predicates sort/3 and msort/3 to the library object "list".
These sorting predicates accept as first-argument a closure to be used
for comparing list terms when sorting.

Added new predicate declarations to the "dictionaryp" library protocol:
clone/3, clone/4, previous/4, next/4, min/3, max/3, apply/4, delete_min/3,
delete_max/3, map/2, and update/4. Removed the insert_all/3 predicate
declaration. Added a missing meta-predicate directive for the predicate
map/3. Improved predicate documentation.

Added new predicates preorder/2, inorder/2, and postorder/2 to the library
object "bintree". Corrected broken definition of the meta-predicate map/3.
Added definitions for the new predicates in the updated "dictionaryp"
library protocol.

Added a port of the Vitor Santos Costa implementation of Red-Black trees
(found on the YAP library "rbtrees"). The port, "rbtree", implements the
"dictionaryp" library protocol. Work in progress.

Added a port of the Richard O'Keefe implementation of heaps. This port
provides both max-heap and min-heap implementations. Work in progress.

Updated the "parametric" example to also illustrate the use of parametric
categories.

Updated the "metainterpreters" example, adding a new meta-interpreter for
counting the number of resolution steps when proving a goal.

Added the classical naive list reverse benchmark test to the "benchmarks"
example. Added a version of the benchmark test module for ECLiPSe. Updated
the "benchmarks" object in order to also run the module tests whenever the
back-end Prolog compiler supports modules.

Simplified usage of the "mtbatch" multi-threading benchmarks example
by using conditional compilation directives for the Prolog-specific
predicates instead of a parametric object.

Updated the loader file for the "encodings" example in order to only load
the UTF-32 source file ("mythology.lgt") on Prolog compilers supporting
this encoding (currently, only CxProlog and SICStus Prolog). This avoids
misleading loading error messages when using SWI-Prolog or YAP.

Updated the "cc" example with predicate definitions for Ciao and added a
new predicate, expand_path/2, for all supported back-end Prolog compilers.

Updated the Logtalk Windows installer in order to detect the ECLiPSe 6.1
version (both 32 and 64 bits versions). If both 6.0 and 6.1 versions are
installed, the installer creates a shortcut for using version 6.1 as the
back-end Prolog compiler.

Updated the Logtalk Windows installer in order to detect SICStus Prolog
4.x installations when running on Windows 64 bits versions.

Update the Windows installer script in order create integration shortcuts
for both console and window versions of SWI-Prolog, to check for the new
names of the SWI-Prolog executables ("swipl.exe" and "swipl-win.exe") when
creating the shortcuts, and to detect SWI-Prolog 64 bits versions.

Updated the Windows installer to detect CxProlog 0.97.4 and later versions
(which use a different registry entry from previous versions).


2.38.2 - January 31, 2010
=========================

Added support for calling :/1 goals using the <</2 control construct.

Added support for calling non-redefined built-in predicates using the
:/1 control construct.

Corrected a Logtalk compiler bug (introduced in version 2.38.1) where
redefinitions of Prolog built-in predicates would be ignored. Thanks
to Parker Jones for the bug report.

Simplified declaration and handling of proprietary directives with
meta-arguments by defining a '$lgt_pl_meta_directive'/1 predicate in
all config files.

Added '$lgt_tr_predicate_indicators'/2, '$lgt_tr_predicate_heads'/2, and
'$lgt_tr_predicate_heads'/3 hook predicates to the Logtalk compiler in
order to simplify handling of proprietary directives in config files.

Added support for the proprietary directive eager_consume/0 to the config
file of B-Prolog and corrected bugs that prevented using the proprietary
tabling directives mode/1 and (:)/2 within objects and categories.

Added support for the proprietary directive demon/1 to the config files
of ECLiPSe.

Added support for the proprietary directive block/1 to the config files
of SICStus Prolog.

Corrected a bug in the SWI-Prolog config file that prevented using the
proprietary directive index/1 within objects and categories.

Added support for the proprietary directive initialization/2 to the config
files of SWI-Prolog and YAP.

Added support for the proprietary meta-predicates findall/4 and tfindall/3
to the XSB config file.

Updated the library object "term" with a (hopefully) faster implementation
of the predicate ground/1, following advise by Lindsey Spratt on the GNU
Prolog mailing list.

Corrected a validation (but harmless) bug in the Logtalk syntax coloring 
support for the jEdit text editor.


2.38.1 - December 21, 2009
==========================

Added support for using the {}/1 compiler bypass Logtalk control construct
to wrap closures that should be executed within the context of the pseudo-
object "user" (e.g. calls to Prolog built-in predicates).

Workaround module meta-predicate semantics in order to support calls to
module meta-predicates specified in use_module/2 directives within objects
and categories.

Added support for compiling calls to module meta-predicates that are
explicitly qualified (may require that the modules are loaded, depending
on the back-end Prolog compiler).

Code cleanup of meta-predicate directives compilation. Tighten checks of
meta-predicate directives and meta-predicate directive arguments.

Improved compilation of call/2-N goals.

Improved performance of meta-calls that are compiled at runtime.

Improved performance of the lambda expression Free/Goal, usually used in
bagof/3 and setof/3 calls.

Improved lambda expression error-checking and added support for using a
(>>)/2 lambda expression as a goal.

Corrected a bug in handling the meta-call context for lambda expressions.

Corrected a typo in the exception term generated when sending a message to
an object corresponding to a built-in local method.

Updated the CxProlog config file in order to set write term and write list
limits that allow compilation of the Logtalk libraries and examples (the
default values are two low in some cases leading to compilation errors).
In addition, take advantage of the new "version_data" flag, thus requiring
version 0.97.4 or a later version.

Updated the SWI-Prolog config file in order to recognize and compile the
proprietary directive at_halt/1.

Updated the B-Prolog config file in order to add a missing meta-predicate
declaration for the proprietary foreach/2 built-in predicate.

Updated the YAP config file by adding a missing declaration for the time/1
proprietary built-in meta-predicate and by commenting out setting the call
that sets the "language" flag to "iso" due to all the YAP libraries that
don't compile with this setting.

Updated the XSB config file by adding a missing declaration for the time/1
proprietary built-in meta-predicate and a definition for the predicate
setup_call_cleanup/3 (in order to restore compatibility with the Logtalk
multi-threading features).

Updated all config files with information on the availability of the
setup_call_cleanup/3 predicate.

Expanded the "lambdas" example with a contribution by Artur Miguel Dias,
more sample queries (including non-deterministic ones), and some simple
benchmarks.

Added syntax coloring support for the ^/2 existential quantifier operator
for most supported text editors and syntax highlighters. Added sample code
using the ^/2 existential quantifier to the syntax coloring test file.


2.38.0 - December 3, 2009
=========================

Added lambda expression support. Added an example, "lambdas", illustrating
the use of lambda expressions.

Added a new compiler flag, "clean", for cleaning the intermediate Prolog
files generated when compiling Logtalk source files.

Simplified representation of the runtime execution context using a simple
list resulting in a small performance increase for some back-end Prolog
compilers such as B-Prolog.

Added support for using the set_logtalk_flag/2 directive within Logtalk
entities. This allows e.g. easy setting of entity properties such as
"events" when creating new entities using the entity creation built-in
predicates. Updated documentation on the set_logtalk_flag/2 directive
and the set_logtalk_flag/2 built-in predicate exceptions.

Added support for runtime instantiation of the <</2 control construct
arguments.

Added support for specifying multiple initialization/1 directives when
using the create_object/4, create_category/4, and create_protocol/3
built-in predicates.

Corrected a bug where some entity properties would not be set when using
the create_object/4, create_category/4, and create_protocol/3 built-in
predicates.

Corrected a bug (introduced in version 2.37.4) when simplifying calls
to the {}/1 external call control construct with a variable argument.

Updated the Logtalk compiler to workaround file path problems with
back-end Prolog compilers (e.g. SICStus Prolog) where calls to the
open/3-4 built-in predicate are not always relative to the current
working directory.

Improved Logtalk runtime error handler when using SWI-Prolog as the
back-end compiler.

Updated the ECLiPSe, SICStus Prolog, SWI-Prolog, and YAP config files
in order to detect discontiguous predicate clause warnings and update
the Logtalk warnings counter accordingly.

Corrected a bug in the ECLiPSe config files in the definition of the
predicate '$lgt_current_time'/3 where the number of seconds would be
returned as a float instead of as an integer.

Updated the SICStus Prolog config files in order to support settings
files. Updated the SICStus Prolog 4.x config file in order to use the
new "version_data" flag and recognize the new do/2 meta-predicate added
in version 4.1.

Updated the GNU Prolog config file to make use of the new "version_data"
flag in the forthcoming 1.3.2 version.

Updated the SWI-Prolog config file in order to ensure that the predicate
'$lgt_expand_path'/2 returns full paths.

Updated the library support for unit testing in order to allow per-test
setup and cleanup goals and to provide timing, testing file information,
and statistics on passed and failed tests. Updated the "testing" example
for the changes to the unit test support.

Updated the Logtalk Windows installer in order to detect the new SICStus
Prolog 4.1 version. If both 4.0.x and 4.1.x versions are installed, the
installer creates a shortcut for using version 4.1.x as the back-end
Prolog compiler.

Updated the Logtalk Windows installer in order to detect both XSB single-
threaded and multi-threaded versions and to no longer create integration
shortcuts for XSB versions older than 3.2.

Corrected a possible data loss bug when using the Debian and MacOS X
installers where some third-party mime-type associations might be lost
when adding the Logtalk mime-type to the Shared MIME-info Database.

Corrected a harmless syntax error in the Logtalk POSIX install shell
script when the "update-mime-database" command is not available.

Added a Logtalk version of the "Closed Knight's Tour" SWI-Prolog CLP(FD)
example written by Markus Triska.

Added a analyse_text() function to the Pygments syntax highlighter.

Updated the Logtalk mode for the SubEthaEdit text editor 3.5.x version
with the help of Martin Pittenauer.

Added some missing cases to the Logtalk source file used for testing
syntax highlight support.


2.37.5 - October 29, 2009
=========================

Updated the Logtalk multi-threading features, replacing all calls to
the call_cleanup/2 built-in predicate with calls to the more reliable
setup_call_cleanup/3 built-in predicate. This change requires recent
versions of SWI-Prolog, XSB, and YAP when writing multi-threading code.

Improved performance of the database built-in methods retractall/1 and
retract/1 whenever their use requires updating the internal predicate
definition lookup tables.

Corrected a bug in the Logtalk runtime when printing a compatibility
warning regarding the back-end Prolog compiler version being used.

Corrected a bug in the implementation of the :/1 control construct when
some but not all imported categories are compiled for static binding.

Corrected a event handling bug in the processing of meta-calls whose
meta-arguments have the format Object::Closure.

Corrected a bug in the runtime error handler for top-level message sending
calls where the reported exception term context argument would be unbound.

Corrected a bug in the compilation of multifile directives and multifile
predicate clauses within objects and categories for the pseudo-object
"user". Corrected a bug in the compilation of multifile predicate clauses
for other objects and categories.

Corrected a safety meta-predicate compilation bug when using static
binding to optimize a call to a local meta-predicate made by a normal,
public predicate.

Corrected a safety meta-predicate compilation bug when using a meta-
predicate call in the body of a user-defined meta-predicate clause in
order to change the number of extra arguments of a closure.

Added a monitor/1 predicate to the library event registry objects.
Updated the definition of the monitors/1 and monitored/1 predicates
in order to avoid using the non-standard predicate sort/2.

Added support for adding the Logtalk mime-type to systems supporting
the freedesktop.org shared mime-info database.

Added support for adding the Logtalk mime-type and the file description
for Logtalk source files to the Windows installer.

Updated the RPM spec file and build script to no longer require root
access. The RPM package built from these files is no longer relocatable.

Updated the POSIX (un)install shell scripts to print the version number
of the Logtalk release being (un)installed.

Updated the "lgt2pdf.sh" and "lgt2txt.sh" POSIX shell scripts in order
to prevent accidental deletion of the "logtal.dtd" and "logtalk.xsd"
files in the Logtalk installation directory. Thanks to Michael Igler
for the bug report.

Added information about using the Sublime Text Windows text editor for
editing Logtalk source files.

Added a set of Logtalk source code snippets and a set of compilation
and documenting tools for the Gnome's Gedit text editor (see the file 
"coding/gedit/NOTES.txt" for details).

Replaced GeSHi support with a new language file written from scratch.

Added syntax coloring support for the module directive reexport/1 to
all supported text editors and syntax highlighters.

Updated documentation in order to remove references to deprecated syntax
for sequences of predicate indicators in predicate directives. Updated 
documentation on built-in meta-predicates to correctly described their
semantics when used within categories.

Added a simple example, "multifile", illustrating how to use multifile
predicates within Logtalk objects and categories.

Updated the "securemp" example in order to illustrate using a call to
a meta-predicate to change the number of extra arguments of a closure.


2.37.4 - September 18, 2009
===========================

Updated the Logtalk compiler to expand calls to the forall/2 and once/1
built-in predicates in order to improve performance.

Corrected a bug in the Logtalk compiler that resulted in some calls to the
built-in predicate call/1 being transparent to cuts in its goal argument.

Updated the Logtalk runtime to try to detect and warn about incompatible
back-end Prolog compiler versions.

Updated the Logtalk runtime to avoid spurious type errors when working
with parametric objects using ECLiPSe due to the weak ISO Prolog standard
compliance of this compiler.

Added two new commands to the Logtalk built-in debugger for writing the
current goal as a quoted term and for removing context spy points at
leashed ports.

Added a logtalk_library_path/2 entry for the library "home" (the user home
directory) when running on POSIX operating-systems.

The library object "pairs" in now loaded by the "types_loader.lgt" loader
file.

Updated the Qu-Prolog integration script in order to set the choice point
stack to 256KB in order to support loading larger Logtalk applications.
Updated the Qu-Prolog config file in order to set the default value of the
"tmpdir" to '.lgt_tmp/'.

Updated the B-Prolog and SICStus Prolog config files in order to set
the default value of the "tmpdir" depending on the operating-system by
checking for a POSIX-only default environment variable.

Added missing reference to the "settings.lgt" file to the spec file used
for building the Logtalk RPM installer.

Modified the scripts used to generate the PDF versions of the User and
Reference manuals to use a local catalog file for the XHTML DTD files.


2.37.3 - August 10, 2009
========================

Added a workaround for the lack of standardization of the Prolog built-in
predicate predicate_property/2 when used in the if/1 and elif/1 conditional
compilation directives.

Added support for using file name paths containing environment variables
in the logtalk_library_path/2 predicate and in the "xmldir" and "tmpdir"
compiler flags when using selected back-end Prolog compilers.

Corrected a code optimization bug in the Logtalk compiler when compiling
bagof/3 and setof/3 calls whose goal is existentially qualified (using
the ^/2 operator). Thanks to Joerg Schuster for the bug report.

Simplified compilation and improved performance of initialization/1 goals
for Prolog compilers supporting the multifile/1 predicate directive.

Added a "libpaths/libpaths_no_multifile.pl" file for pesky back-end Prolog
compilers that don't support the ISO Prolog standard multifile/1 predicate
directive (e.g. GNU Prolog and K-Prolog).

Updated the library object "term" to use the built-in predicates ground/1
and subsumes/2 when available in the back-end Prolog compiler.

Updated the library object "list" to use the built-in predicates sort/2
and msort/2 when available in the back-end Prolog compiler.

Updated the library object "integer" to use the built-in predicates plus/3,
between/3, and succ/2 when available in the back-end Prolog compiler.

Added a predicate add/3 to the library object "difflist".

Added a predicate '$lgt_expand_path'/2 to all config files in order to
expand file name paths that may contain e.g. environment variables or
relative paths into absolute file names (only supported in some back-end
Prolog compilers).

Updated all Prolog integration shell scripts to avoid a compatibility
issue in Ubuntu distributions when running the scripts using sudo.

Updated the SWI-Prolog integration support files in order to hide some of
the Logtalk internal predicates from the SWI-Prolog native profiler.

Updated the ECLiPSe config files to ensure that attempts to create a
directory that already exists succeed instead of throwing an error.

Updated text editor configuration files in order to remove references to
the no longer used ".config" file name extension and to support syntax
coloring and code completion of multifile/1 and use_module/1-2 directives.

Corrected some spelling and XHTML typos in the User and Reference Manuals.
Corrected some BibTeX typos in the "BIBLIOGRAPHY.bib" file.


2.37.2 - June 29, 2009
======================

Added support for parsing reexport/2 module directives when compiling
Prolog modules as Logtalk objects. The predicate renaming operator as/2
found on SWI-Prolog and YAP is also supported.

Added support for using grammar rule non-terminal indicators in the uses/2,
multifile/1, reexport/2, and use_module/2 directives.

Added support for parsing operator declarations in the module/2, export/1,
reexport/2, and use_module/2 module directives.

Added support for declaring and defining object and category multifile
grammar rules.

Corrected a bug where a failed load of a settings file could be reported
as successful.

Corrected a bug when compiling an object or category normal predicate that
redefines a built-in Prolog meta-predicate.

Added missing dynamic directives in the definition of the pseudo-objects
"debugger" and "user".

Eliminated message-sending overheads when sending messages to the pseudo-
object "user".

Allow the "debugger" built-in object to play the role of both a class and
a prototype when being specialized or extended.

Added support for compiling module-qualified arguments in calls to Prolog
database and reflection built-in predicates.

Added support for using multiple info/1 directives per entity and multiple
info/2 directives per predicate (useful when compiling modules as objects). 

Added limited support for translating queries as initialization goals when
compiling modules as objects (queries must be calls to locally defined
predicates or to predicates referenced in use_module/2 directives).

Optimized performance of meta-predicates whose meta-arguments are goals,
not closures, when using static binding.

Improved automatic generation of XML documenting files for parametric
objects when no parameter names are given.

Updated the SWI-Prolog config file in order to support the proprietary
predicate directives format_predicate/2, noprofile/1, and volatile/1.

Updated the SICStus Prolog, SWI-Prolog, and YAP config files in order to
workaround broken meta-predicate declarations for the built-in predicates
format/2-3 and in order to support the sloppy use of the ensure_loaded/1
directive within modules, rewriting it to a use_module/1 directive.

Updated the ECLiPSe, SWI-Prolog, and YAP config files in order to support
parsing of reexport/1 module directives.

Updated the YAP config file in order to support parsing of the use_module/1
module directive.

Updated the ECLiPSe config files in order to make available the predicate
numbervars/3 and to support parsing of comment/2, import/2, inline/2, and
set_flag/3 proprietary directives.

Updated the SICStus Prolog config files in order to support parsing of the
load_foreign_resource/1 proprietary directive.

Updated the B-Prolog config file in order to support the new foreach/3-4
meta-predicates and the new table mode directive in B-Prolog 7.3. Added
definition for the "prolog_version" read-only compiler flag.

Updated the XSB config file in order to enable support for multifile
directives (version 3.2 or later is required).

Improved conversion guide for migrating Prolog module code to Logtalk.

Improved documentation and examples of using the term and goal expansion
mechanisms.

The "lgt2*.sh" documentation shell scripts have been updated to ignore
XML files that are not XML documenting files generated by the Logtalk
compiler.


2.37.1 - June 2, 2009
=====================

Added support for declaring and defining object and category multifile
predicates. This new feature is primarily intended to facilitate migration
of Prolog code to Logtalk and should be used with caution as multifile
support is spotty among back-end Prolog compilers (easily leading to
portability problems). Multifile predicates can be both static or dynamic.

Added support for compiling clauses for module multifile predicates within
objects and categories in order to provide better integration with CLP(FD)
and similar constraint packages.

Added support for parsing module/2 directives whose export list contains
operator declarations.

Improved Logtalk compiler error-checking of file-level op/3 directives.

Updated the Logtalk compiler to try to detect calls to proprietary built-in
meta-predicates not declared in the config files. Detection may fail due to
the lack of standardization of meta-predicate specifications.

Updated the Logtalk compiler to avoid unexpected compilation errors when
querying predicate properties due to the lack of standardization of the
Prolog predicate_property/2 built-in predicate.

Improved runtime error-checking for meta-calls whose closure argument is
either Object::Closure or Module:Closure.

Corrected a Logtalk compiler bug where a redefinition of a proprietary
built-in meta-predicate (declared in the config files) would be ignored.

Corrected a bug in the implementation of the Logtalk <</2 control construct
where a redefinition of a built-in predicate would be ignored.

Corrected a Logtalk compiler bug where initialization/1 goals would be
compiled as normal code instead of as debug code when compiling a source
file in debug mode.

Updated documentation support to include predicate alias information on
the automatically generated XML documenting files. Updated the sample XSLT
transformations to output alias information.

Corrected a bug in the YAP config file that prevented setting the "unknown"
flag to "error" and the "language" flag to "iso". Added declarations for
the proprietary built-in meta-predicates call_residue_vars/2, ignore/1,
depth_bound_call/2, setup_call_cleanup/3, and setup_call_catcher_cleanup/4.
Removed declaration for the outdated meta-predicate on_cleanup/1. Added
support for the thread_initialization/1 proprietary directive.

Updated the B-Prolog config file with declarations for the proprietary
built-in meta-predicates fd_minimize/2 and fd_maximize/2.

Updated the GNU Prolog config file in order to recognize the foreign/1-2
proprietary directives. Added declarations for the proprietary built-in
meta-predicates fd_minimize/2 and fd_maximize/2.

Updated the ECLiPSe config files with declarations for the proprietary
built-in control constructs and meta-predicates ~/1, not/1, and (-?->)/1
(the last one only for the non-ISO config files).

Updated the SWI-Prolog config files with declarations for the proprietary
meta-predicates setup_call_cleanup/3, setup_call_catcher_cleanup/4, when/2,
and with_output_to/2. Added support for the thread_initialization/1
proprietary directive. Improved parsing of use_module/1-2 directives.

Added a dummy definition for the predicate '$lgt_logtalk_prolog_encoding'/3
to the config files of all back-end Prolog compilers that don't support the
encoding/1 directive.

Added predicate sequence/3 to the library object "integer".

Added predicates append/2 and msort/2 to the library object "list". Fixed
some termination issues with the length/2 predicate on buggy input.

Added aliases foldl/4, foldr/4, scanl/4, and scanr/4 to the library object
"meta".

Added an experimental "logtalk_select.sh" POSIX shell script for switching
between installed Logtalk stable versions.

Added a POSIX integration script, "xsb64lgt", for using Logtalk with the 
single-threaded, 64 bits version of XSB.

Added a note on a workaround for compatibility issues between GNU Prolog
and the contributed XML parser library. Thanks to Nicolas Pelletier for
the bug report.

Added a new example, "securemp", illustrating the safety rules used by
Logtalk in the compilation and execution of meta-predicates (as described
on the "Secure Implementation of Meta-predicates" PADL'09 paper).

Extended the "constraints" example for SWI-Prolog and YAP illustrating
the definition of custom constraint propagators within objects.

Added instructions on how to enable a source code browser when editing
Logtalk source files with the Vim text editor (using the Taglist Vim
plugin and Exuberant ctags).


2.37.0 - May 11, 2009
=====================

Improved dynamic binding performance by making calls to the lookup caches
call the cached methods instead of simply returning the corresponding
callable terms, therefore no longer requiring a meta-call for executing
the methods. For multi-threaded back-end Prolog compilers, the new caching
mechanisms forces the lookup cache dynamic predicates to be declared
thread shared instead of thread local.

Improved performance of the built-in database methods retractall/1 and
retract/1.

Improved performance of user-defined object and category meta-predicates.

Added support for meta-calls whose closure argument is ::Closure.

Simplified compilation and improved performance of predicates that call
local user meta-predicates.

Corrected a bug where calls to local user meta-predicates would fail.
   
Corrected a bug where local calls to user meta-predicates would not be
visible to the built-in debugger.

Improved Logtalk compiler error checking for accidental redefinition of
control constructs.

Improved Logtalk runtime error checking for message sending and "super"
control constructs when the message is only bounded at runtime.

Updated the exception terms thrown when sending messages to self in order
to use the ::/1 control construct for the culprit goal instead of the ::/2
control construct.

Simplified message sending implementation by removing the seldom used
control constructs that allowed sending the same message to a conjunction
or disjunction of objects.

Specify \+/1, call/1-N, and once/1 as built-in local meta-predicates. 

Disallow using Logtalk and Prolog built-in meta-predicates as messages in
order to avoid conflicting semantics with user-defined object and category
meta-predicates. The Logtalk control constructs (,)/2, (;)/2, (->)/2, and
!/0 may still be used as syntactic sugar when sending a set of messages to
a single object.

Updated the Logtalk compiler to ensure that calls to the built-in meta-
predicate call/2-N within a category have the same semantics as other
built-in meta-predicates whose meta-arguments are goals.

Improved performance of the :/1 control construct when using dynamic
binding by adding a predicate lookup caching mechanism.

Added support for using the :/1 control construct with a non-instantiated
argument at compile-time (handled using dynamic binding at runtime).

Allow a variable clause body to be interpreted as a meta-call instead of
an instantiation error. Although is not recommended to use a variable as
a goal instead of using the call/1 built-in meta-predicate, this change
allows for uniform handling of variables as goals in clause bodies. Thanks
to Victor Noel for the bug report.

Updated the ECLiPSe config files in order to support the non-standard 
*->/2 (aka "soft cut") control construct. Thanks to Victor Noel for the
bug report.

Updated the Ciao config file in order to support the non-standard if/3
(aka "soft cut") control construct.

Updated the Qu-Prolog config file in order to enable support for multifile
directives (version 8.9 or later is now required).

Updated the Windows installer in order to ensure compatibility with the
new CxProlog 0.97.3 version. Updated the CxProlog config file in order
to set the "underscore_variables" compiler flag to "dont_care".

The library object "varlist" is no longer derived from "list" as several
of the "listp" predicates are not meaningful or cannot be used with the
same modes. Added a new library protocol "varlistp". Corrected several
bugs in the implementation of the library object "varlist" predicates.
Thanks to Victor Noel for the bug report.

Added meta-predicates scan_left/4, scan_right/4, map/6-8, and partition/6
to the library object "meta". Optimized performance of the meta-predicates
fold_left/4, fold_right/4, include/3, exclude/3, map/2-8, and partition/4.

Added predicates partition/5, same_length/3, selectchk/3, and
subsequence/3 to the library objects "difflist" and "list".

Added predicates product/3 and selectchk/3 to the library object "set".

Updated the "puzzle" constraint example for SWI-Prolog to avoid leaking
"clpfd" predicates in query binding results. Thanks to Ulrich Neumerkel
for reporting the problem.

Corrected a bug in the "expansion" example that prevented some of the
example queries from working.

Simplified running the "poem" example.


2.36.0 - April 9, 2009
======================

Added support for overriding Logtalk default compiler flags (specified
in the back-end Prolog compiler config files) using new settings files.
Logtalk will load a "settings.lgt" file in the startup directory. If 
this file is not found, Logtalk will load a "settings.lgt" file in the
Logtalk user directory. Requires running Logtalk with a back-end Prolog
compiler supporting access to environment variables. Loading of settings
files from the Logtalk startup directory is only available when running
on Windows for B-Prolog, Ciao, GNU Prolog, SWI-Prolog, XSB, and YAP.
SICStus Prolog is incompatible with this new feature, requiring manual
loading of settings files after startup. Consult the "configs/NOTES.txt"
file for compatibility details for other Prolog compilers.

Updated the "cplgtdirs.*" scripts to copy an existing "settings.lgt" file
from the backup of the Logtalk user folder to the new Logtalk user folder.

Updated the Logtalk compiler and runtime and all the config files to use
the more standard call/N predicates instead of the call_with_args/N 
predicates.

Improved the XML documentation automatically generated for objects and
categories by appending to the compilation mode all the active optional
features and the "threaded" and "synchronized" properties. Improved the
XML documentation automatically generated for synchronized predicates by
appending the "synchronized" property to the predicate compilation mode.

Improved error handling for the built-in predicates logtalk_load/2,
logtalk_compile/2, current_logtalk_flag/2, and set_logtalk_flag/2
when checking flag names and values.

Improved Logtalk compiler error and warning reporting by printing,
whenever possible, the range of lines where the problem was found
instead of printing only the starting line.

Added new object properties "complements", "context_switching_calls",
"events", and "dynamic_declarations". Added new category property
"events". These properties are defined whenever the corresponding
compiler flags are set to "allow" at the time of entity compilation.

Corrected a bug where some file-related entity properties would not
be stored when using back-end Prolog compilers that don't support
multifile predicates.

Corrected a bug (introduced in version 2.35.0) in the compilation
and processing of "super" calls for objects that both instantiate
and specialize other objects.

Corrected a bug (introduced in version 2.35.1) in the Logtalk compiler
that prevented compilation of Prolog modules as objects.

Corrected a bug in the implementation of the Logtalk built-in predicate
threaded/1 that would result in wasting space in the main thread message
queue with duplicated messages. Thanks to Rui Marques for the bug report.

Corrected a bug where the Logtalk compiler would report the number of
compilation warnings with the "report" compiler flag set to "off".

Updated the Logtalk compiler in order to avoid unwanted backtracking
during source file compilation and when calling the entity creation
built-in predicates.

The compiler flags "context_switching_calls", "startup_message", and
"altdirs" are no longer read-only flags in order to support changing
their default values from within "settings.lgt" files. The possible
values for the compiler flags "dynamic_declarations", "complements",
"context_switching_calls", and "events" are now "allow" and "deny".
Added a new read-only compiler flag "prolog_version". Renamed the 
compiler flag "prolog" to "prolog_dialect".

Changed object compilation in order to restrict the use of the <</2
control construct to objects compiled with the "context_switching_calls"
compiler flag set to "allow".

Changed the "cplgtdirs.*" and installation scripts to no longer make a
copy of the "configs" directory in the Logtalk user directory. Updated
the customization instructions to advise users to edit the "settings.lgt"
file instead of editing the Prolog configuration files.

Changed the Windows installer script in order to create Logtalk startup
scripts that start from the current directory as returned by the "%CD%"
dynamic environment variable. This allows a shortcut to be simply copied
to a Logtalk project directory without requiring editing its properties
in order to set the startup directory to the project directory.

Updated the XSB-MT integration scripts to make all lookup cache dynamic
predicates thread private. Updated the XSB config file for version 3.2,
which is now required for running Logtalk.

Added a new '$lgt_prolog_feature'/2 predicate to all config files, used 
for representing back-end Prolog supported features that are previously 
represented by read-only compiler flags.

Added new '$lgt_environment_variable'/2, '$lgt_startup_directory'/1, and
'$lgt_user_directory'/1 predicates to all config files.

Changed the default value for the "startup_message" compiler flag to the
less verbose "flags(compact)".

Updated the config files of the Ciao, CxProlog, GNU Prolog, ECLiPSE, and
XSB compilers to set the default directory for temporary files depending
on the host operating-system.

Updated the config files of the Ciao, ECLiPSe, GNU Prolog, Qu-Prolog, 
and SICStus Prolog compilers to return term position line numbers for
reporting warnings and errors.

Switched off use of multifile/1 directives for Amzi! Prolog due to its 
buggy implementation on this Prolog compiler.

Updated the Ciao config file to workaround a weird bug in the read_term/3
predicate that prevented compilation of some Logtalk source files.

Updated the K-Prolog config files in order to make the definition of the
'$lgt_delete_file'/1 predicate compatible with Windows systems.

Updated the predicate comments in the library protocol "loopp" in order
to workaround the limitation on the maximum size of atoms in Qu-Prolog.

Removed the obsolete SWI-Prolog hook file "swi_set_logtalk_context.pl"
(the Logtalk control construct <</2 provides similar functionality).

Added a check/1 predicate to all objects in the "types" library. This 
predicate differs from the existing valid/1 predicate by throwing an
exception when the argument is not valid instead of failing.

Added multifile and dynamic declarations for the logtalk_library_path/2
Logtalk predicate to the "libpaths/libpaths.pl" file. This change allows
use of Ciao as the back-end Prolog compiler without patching the Logtalk
compiler and runtime (but also results in a harmless warning when using
GNU Prolog).

Updated the POSIX integration scripts to export the environment variable
LOGTALK_STARTUP_DIRECTORY before starting Logtalk.

Changed the SWI-Prolog POSIX integration script to use the option "-s"
instead of the option "-f" in order to allow loading of any existing
user initialization file (".plrc" on POSIX systems; see the SWI-Prolog
reference manual) before loading the Logtalk files. On Windows the use
of the "-s" results in a weird double initialization bug.

Workaround a compilation error in the object "salt/3" in the "searching"
example when using SICStus Prolog as a back-end compiler.


2.35.1 - March 1, 2009
======================

Allow explicitly qualified module calls in the body of category and object 
predicate clauses without requiring the calls to be wrapped within a {}/1
control construct to ensure successful compilation. This requires an hack
in the config files of most back-end Prolog compilers with module systems
as the implementations of the predicate_property/2 built-in predicate 
cannot be used to test if :/2 is a built-in control construct. Updated the
config files of Amzi! Prolog, Ciao, IF/Prolog, Quintus Prolog, SICStus 
Prolog, SWI-Prolog, XSB, and YAP.

Improved the Logtalk compiler compatibility with Prolog meta_predicate/1
directives that use the integer zero as an alternative to the atom ':' for
denoting meta-arguments that are goals.

Added limited support for calling Prolog meta-predicates defined in module
libraries (meta-arguments must be goals, not closures; the Logtalk compiler
assumes that an imported_from/1 property is defined for these predicates).

Implemented a "warnings" value for the "report" compiler option, providing
a less verbose compilation reporting where only warnings are printed.

Corrected a Logtalk compiler bug where warnings related to synchronized
predicates would be reported even with the "report" flag set to "off".

Added support to the message sending mechanisms for using the object proxy
access notation when the message receiver is only known at runtime.

Changed the semantics of the Logtalk notation for accessing object proxies
when sending a message to allow backtracking over the proxy goal.

Updated the Logtalk compiler to avoid reporting multiples reference to the
same unknown parametric object.

Corrected a bug in the implementation of the "smart_compilation" compiler
flag that would result in a misleading successful compilation of a source
file containing syntax errors after the first failed compilation when the 
errors are not corrected between compilations. Thanks to Joerg Schuster 
for the bug report.

Added a '$lgt_delete_file'/1 predicate to all config files. For back-end
Prolog compilers that don't support a built-in predicate for deleting 
files, the '$lgt_delete_file'/1 predicate simply fails. This predicate is
used by the Logtalk compiler to ensure correct behavior of the compiler
flag "smart_compilation".

Updated the YAP config file to ensure that the "syntax_errors" proprietary
flag is set to "error".

Integration with GNU Prolog now requires version 1.3.1 or later of this
compiler.

Added a workaround to the ECLiPSE config files in order to support smart 
compilation of source files (due to the non-standard implementation of the
read_term/3 predicate that fails for syntax errors instead of throwing an
exception).

Updated the library object "term", adding a singletons/2 predicate and
renaming the predicate vars/2 to variables/2 (the old name is still
available but shall be considered deprecated). Both the variables/2 and
the singletons/2 predicates return variables ordered as found when doing
a depth-first, left-to-right term traversal. Optimized the implementation
of the predicates ground/1 and variables/2 to use the Prolog built-in 
versions of these predicates when available.

Moved the Prolog Integration and Migration Guide into the User Manual.

Updated the "BIBLIOGRAPHY.bib" file with recent Logtalk publications.

Updated the "build_release.sh" POSIX shell script steps for building the
MacOS X installer.

Updated the Windows installer to disable by default the installation of
the integration shortcuts for Ciao Prolog and Quintus Prolog as these 
compilers require patches that render Logtalk incompatible with the other
compilers.

Simplified loading and testing instructions for the XML parser included
in the "contributions" directory. Added a missing logtalk_library_path/2
entry for XML parser directory.


2.35.0 - January 16, 2009
=========================

Improved compilation of source code resulting in smaller intermediate
Prolog files and better performance for most back-end Prolog compilers.

Improved caching of message sending predicate lookups (by generating
more reusable cache entries for public predicates, improving lookup
performance and cache hits).

Improved caching of ^^/1 calls (by generating more reusable cache entries,
improving lookup performance and cache hits).

Simplified compilation of meta-predicates. Allow meta-predicates to be
specialized and called using the ^^/1 control construct.

Simplified and improved performance of the multi-threading built-in
predicates within the context of the pseudo-object "user" (e.g. for
top-level queries).

Changed compilation of once/1 goals in debug mode in order to make them
explicit when tracing using the built-in debugger.

Added support for multiple source-level and entity-level initialization/1
directives (matching the ISO Prolog standard and current Prolog practice).

Corrected a bug in the compilation of top-level queries for the Logtalk
built-in predicates.

Corrected a predicate scope checking bug when using the dynamic-binding
cache entries.

Corrected a bug when compiling entities in debug mode that contain
initialization goals. Thanks to Parker Jones for the bug report.

Corrected a bug in the library object "meta" where the deprecated
predicates filter/3 and succeeds/2 are declared (using aliases) but
not defined.

Changed the formulas used for calculating sample skewness and sample
kurtosis to match the formulas used in statistical software such as R.

Added declaration for the proprietary findall/4 meta-predicate to the
SWI-Prolog and YAP config files.

Corrected processing of the proprietary load_foreign_files/3 directive
in the YAP config file.

Switched off use of multifile/1 directives for Ciao due to its buggy
implementation on this Prolog compiler.


2.34.1 - December 15, 2008
==========================

Allow the ^^/1 control construct to be used for calling any inherited
predicate instead of being restricted to calling inherited predicates
in the context of their redefinitions.

Updated the Logtalk compiler to detect (and report as an error) the
definition of two entities with the same name in a source file.

Updated the Logtalk compiler to detect (and report as an error) any
invalid relation between an entity and itself. In most cases, these
errors could result in predicate lookup endless loops.

Corrected a long-standing bug in the implementation of the Obj<<Pred
control construct, which failed to properly set the execution context
to the receiving object for top-level queries. This bug, which results
in a stack overflow error, was uncovered by an unrelated bug fix in the
previous Logtalk version.

Fixed some broken links in the Logtalk XHTML documentation. Updated the
screen CSS file to match the new, lighter style of the Logtalk website.

Updated the "testing" example to take into account the recently added
"dynamic_declarations" compiler flag.

Added a set of double-clickable *.command Terminal.app files for starting
Logtalk with selected back-end Prolog compilers on MacOS X.

Updated the MacOS X installer postflight script to open the Logtalk
installation folder, the *.command files folder, and the "README.md"
file.

Updated the "BIBLIOGRAPHY.bib" file with recent Logtalk publications.


2.34.0 - November 26, 2008
==========================

Added conditional compilation support, as found in some Prolog compilers,
using the new directives if/1, elif/1, else/0, and endif/0. The arguments
of the if/1 and elif/1 directives are subjected to goal expansion.

Added a "prolog" read-only compiler flag whose value is the name of the
back-end Prolog compiler (an atom). This flag is defined in the config
files and can be used for conditional compilation of Prolog specific code.

Added an expand_goal/2 built-in method to support explicit calls to the
goal_expansion/2 user-defined hook. Updated the term expansion mechanism
to allow the expansion to be a single term instead of a list of terms.
Improved the documentation of the term and goal expansion predicates.

Simplified the implementation of the threaded/1 built-in predicate.

Removed support for the deprecated message_queue_create/1 Prolog built-in
predicate.

Corrected a bug in the implementation of the predicate_property/2 built-in
method which would return wrong meta-predicate templates for predicate
aliases. 

Corrected a bug in the compilation of meta-calls to predicates defined
in the pseudo-object "user".

Corrected a bug that prevented goal expansion of source file initialization
goals (expressed using the initialization/1 directive).

Modified the "statistics" library to use the variance numerically stable
algorithm to calculate accurate values for the standard deviation for both
samples and populations. Thanks to Parker Jones for the bug report. Added
calculation of sample and population kurtosis.

Added predicates included/3, excluded/3, fold_left/4, and fold_right/4
to the library entities "metap" and "meta" (requested by Paul Crocker).
Renamed the predicates filter/3 and succeeds/2 to included/3 and map/2,
respectively (the older names are still available using aliases defined
in "meta").

Changed the file name extension of all config "*.config" files to "*.pl".
Changed the file name extension of all library "*.notes" files to "*.txt".

Updated the "metapredicates" example with Paul Crocker's contributions
to illustrate the use of some of the meta-predicates implemented in the
Logtalk library.

Updated the "expansion" example to illustrate the new expand_goal/2
built-in method. Improved example documentation.

Added an example, "cc", of using conditional compilation directives to
implement a portable operating-system interface for selected back-end
Prolog compilers.

Added an example, "prototypes", illustrating the concept of "prototype".

Added syntax coloring support for the new expand_goal/2 method and for
the if/1, elif/1, else/0, endif/0, set_logtalk_flag/2, ensure_loaded/1,
and set_prolog_flag/2 directives to the supported text editors and
syntax highlighters.

Fixed a block comment syntax coloring bug in the support for the Highlight
syntax highlighter with the help of André Simon.


2.33.2 - November 3, 2008
=========================

Added support for using the set_logtalk_flag/2 directive in source files.
This directive is executed when read (thus affecting the compilation
of the source code that follows) but its scope is restricted to the source
file being compiled.

Allow a complementing category to define aliases for predicates of the
complemented object.

Improved reporting of line numbers for syntax warnings and errors when 
using YAP or SWI-Prolog as back-end compilers (requires YAP git version).

Improved reporting of entity existence errors for top-level queries.

Implemented a more flexible internal representation of entity properties.
Added entity properties file(File, Path) and lines(Start, End) for 
entities compiled and loaded from source files. The property lines/2 is 
only available when using YAP or SWI-Prolog as back-end compilers.

Added missing "built_in" properties for the built-in protocols "expanding"
and "monitoring".

Corrected a bug where compiler options are not cleared after compilation
of source files. This bug would result in wrong compilation of messages 
sent from the top-level interpreter (i.e. messages sent by the pseudo-
object "user") whenever events or hooks compiler options are used.

Updated the Logtalk compiler to print a warning when defining clauses for
the built-in predicates term_expansion/2 and goal_expansion/2 without a
reference to the built-in protocol "expanding".

Updated the Logtalk compiler to provide file information when printing
redefined entity warnings if the files of the new and the old definitions
are different (requires support for the file/2 entity property).

Updated the Logtalk compiler to prefix all informative messages with the
line comment character, "%", to make it easier to use interactive section
transcripts as scripts.

Corrected a bug that prevented using synchronized/1 predicate directives
within protocols.

Corrected a bug where portability warnings would be reported even when the
"report" compiler option is turned off. Corrected a bug where portability
warnings would not increment the warnings counter.

Added an optimization and safety compiler option, "complements", that 
allows objects to be compiled with support for complementing categories 
either enabled or disabled. The default value (set in the config files)
is "off" (disabled).

Added an optimization and safety compiler option, "dynamic_declarations",
that allows objects to be compiled with support for dynamic declaration of
predicates (using the built-in database methods asserta/1 and assertz/1) 
either enabled or disabled. The default value (set in the config files) is
"off" (disabled).

Updated the Logtalk built-in debugger to print goal invocation numbers 
and to accept the return and enter keys as alternatives to the "c" key
("creep") when tracing. Updated the implementation of the "display"
command to include the write options quoted(false) and numbervars(false)
(as common in most Prolog debuggers). Added a "unify" command.

Modified the implementation of the built-in predicate define_events/5 to
throw an exception when the monitor object doesn't define the required
event handler methods (previously the define_events/5 call would simply
fail).

Added a customized version of the read_term/2 predicate to all config 
files that returns the line where a term starts for back-end Prolog 
compilers whose implementation of the read_term/2 predicate supports 
this information.

Updated the shortcut created by the Windows installer for integration with
K-Prolog to require version 6.0.4 of this compiler.

Updated the User Manual comparison between Logtalk nomenclature and C++
and Java nomenclatures.

Added a simple example, "instvars", illustrating how to define instance
variables, default variable values, and setter and getter methods.

Added a simple example, "classmethods", illustrating how to define 
"class methods" as found on class-based object-oriented programming
languages.

Improved the documentation of the "complements", "hello_world", "hooks",
and "msglog" examples.

Added a new logic puzzle programming example, adapted with permission 
from Kevin Stone's BrainBashers website (http://www.brainbashers.com/).

Updated all the examples that make use of event-driven programming
("birthdays", "bricks", "complements", "msglog", "polygons", "profiling",
and "searching") to ensure correct functionality after fixing the bug in
cleaning compiler options after compilation of source files.

Updated the "dynpred" example to use the new "dynamic_declarations" 
compiler option. Updated the "complements" example to use the new 
"complements" compiler option. 

Corrected a Vim syntax coloring bug with line comments occurring within
entity opening directives.


2.33.1 - October 12, 2008
=========================

Added a syntax construct for easy access to parametric object proxies
represented as Prolog facts when sending a message ({Proxy}::Message).
Updated the "proxies" example to illustrate this new functionality.

Improved the Logtalk built-in debugger to print clause numbers for 
static predicates at the unification ports ("fact" and "rule").

Modified the built-in methods create_category/4, create_object/4 and
create_protocol/3 to accept a variable as the first argument, which 
will be instantiated to the identifier generated for the new entity.

Modified the database built-in methods to allow initialization of static
predicates for dynamic objects when the predicates are declared in "this".
These changes simplify the initialization of dynamic objects when writing
constructors. The dynamic objects must be descendants of "this".

Fixed a bug in the Logtalk compiler that would result in failure to 
restore the operator table in case of compilation error.

Removed support for the deprecated mutex_create/1 Prolog built-in 
predicate.

Updated the XSB config file by switching off the use of multifile/1 
directives when compiling Logtalk source files as a workaround for know 
bugs in the implementation of this directive for dynamic predicates.

Updated the ECLiPSe 6.0 config files in order to switch on the use of 
multifile/1 directives.

Update the CxProlog config file to work with and require version 0.97.2.

Updated the Windows installer to warn the user when it fails to detect 
the installation of the selected Prolog compilers. Fixed a bug where the
creation of the K-Prolog integration shortcut could fail when performing
a custom installation. Fixed a bug in detecting a CxProlog installation.

Added a library for descriptive statistics. For details, see the file 
"library/statistics.notes".

Added a new library object, "pairs", defining predicates over lists of 
pairs (key-value terms).

Added a new library object, "gensym", defining predicates for generating
unique atoms (object protocol based on the "gensym" module of SWI-Prolog).

Added a predicate partition/4 to the library entities "metap" and "meta".

Added predicates plus/3 and succ/2 to the library object "integer".

Added predicates is_ascii/1, is_white_space/1, is_quote/1, is_period/1,
is_punctation/1, and parenthesis/2 to the library entities "characterp"
and "character".

Modified the implementation of the predicate product/2 in the library 
object "numberlist" to fail for empty lists.

Added an example, "people", illustrating a simple implementation solution
for object constructors.

Improved "metainterpreters" example.

Improved performance of the object initialization and release predicates
defined in the category "initialization" of the "roots" example.


2.33.0 - September 1, 2008
==========================

Applied several optimizations to the Logtalk compiler, mostly related to 
non-detected deterministic predicates due to calls to dynamic predicates 
or due to limitations in clause indexing by most Prolog compilers.

Added support for using Prolog use_module/2 directives in objects and 
categories. This allows module predicates to be called using implicit 
qualification, improving readability. The use_module/2 directive supports
the declaration of predicate aliases using the notation Original:Alias.

Added support for meta-calls whose closure arguments are explicitly 
qualified (using either Object::Closure or Module:Closure).

Added support for using the ensure_loaded/1 and the set_prolog_flag/2 
directives in source files. These directives are both processed when 
read (thus affecting the compilation of the source code that follows) 
and copied to the generated intermediate Prolog files.

Corrected a set of singleton variable related bugs in the Logtalk compiler
and runtime. Thanks to Joachim Schimpf and Kish Shen work on the improved 
singleton variable checking code found on the new ECLiPSe 6.0 compiler.

Simplified the implementation of the Logtalk built-in debugger. Changed 
the implementation of the debugger abort command to use the de-facto 
standard Prolog built-in predicate abort/0. Corrected a bug that prevented
the use of the command "ignore" at the redo port.

Removed the Logtalk dispatcher thread used for dispatching asynchronous 
multi-threading calls, simplifying the implementation and improving the 
performance of the asynchronous built-in multi-threading predicates.

Corrected a bug in the implementation of the threaded_once/1-2 built-in 
multi-threading predicates (use of detached threads could make the methods 
fail despite the success of the thread goals).

Improved performance of multi-threading applications (specially on MacOS X)
when using SWI-Prolog as the back-end compiler by making the lookup caches 
thread local.

Small performance improvement of the threaded/1 built-in multi-threading 
predicate.

Added a multifile/1 directive for the logtalk_library_path/2 predicate.

Corrected a bug in the verification of portable calls that would fail to 
flag the soft-cut control construct (_ *-> _; _) found on some Prolog 
compilers.

Corrected a bug in the implementation of the built-in database predicates 
retract/1 and retractall/1 when adding a cache entry for a local dynamic 
predicate with no scope declaration.

Corrected a bug in the compilation of source file op/3 directives whose 
third argument is a list of operator specifiers instead of a single 
operator specifier.

Added ECLiPSe 6.0 config files. Modified the POSIX script "eclipselgt" 
to test for both ECLiPSe 5.10 and 6.0 versions. Added Windows shortcut 
to start Logtalk with ECLiPSe 6.0.

Updated the GNU Prolog config file to allow Logtalk to recognize as 
built-in predicates the finite domain solver built-in predicates.

Added a new set of examples, "constraints", illustrating how to use the 
Constraint Domain Solver libraries available with B-Prolog, ECLiPSe, 
GNU Prolog, SWI-Prolog, and YAP. Thanks to Markus Triska for his help 
with the YAP and SWI-Prolog CLP(FD) examples.

Updated the "mtbatch" example to provide multi-threading overhead data
for the competitive or-parallelism benchmark.


2.32.2 - July 26, 2008
======================

Simplified the predicate lookup linking clauses that are generated when 
compiling private and protected entity relations, resulting in a small 
runtime performance improvement when looking up non-cached predicate 
declarations.

Simplified and optimized handling of predicate aliases when looking up 
predicate declarations and predicate definitions.

When sending a message to an object using the ::/2 control construct, 
also check if the sender matches the scope container when the message 
does not correspond to a public predicate. This is necessary in order 
to allow calling a protected or a private predicate in "this" from 
within a category.

Updated the Logtalk compiler to throw an error when detecting a call 
to a dynamic predicate in the body of a predicate clause defined in a 
category.

Optimized the implementation of the ^^/1 control construct. The Logtalk 
compiler now requires an instantiated, callable argument that must have 
the same functor and arity of the predicate being (re)defined.

Allow the ^^/1 control construct to be used within extended categories.
Updated the "engines" example to illustrate this new functionality.

Moved support for categories that complement existing objects from the 
code generated when compiling objects to the Logtalk runtime, making the 
implementation of this feature easier to maintain and contributing to 
smaller code sizes of the intermediate Prolog files.

Corrected a typo in the exception term throw by the built-in database 
method assertz/1 when an error occurs while trying to assert a fact.

Restored the redefined entity warnings for back-end Prolog compilers 
that support multifile predicates (broken in the final version of 2.32.1).

Changed the format of the Logtalk compiler informative messages for 
better readability.

Improved simplification of the clauses generated by the compilation of 
object and category predicate clauses.

Corrected a bug that prevented asserting and retracting clauses in "this" 
from within a category.

Corrected a bug in the implementation of the built-in database method 
retractall/1 when retracting clauses in an object compiled in debug mode.
Thanks to Parker Jones for the bug report.

Updated all config files due to changes to the predicate used to declare 
proprietary meta-predicates, '$lgt_pl_meta_predicate'/1. Added a second 
argument to represent if the meta-predicate is either a built-in predicate 
or a built-in control construct. This is necessary in order to properly 
compile calls to these proprietary meta-predicates when in debug mode.
Thanks to Victor Noel for the bug report.

Corrected a bug in the config files hooks used in handling proprietary 
Prolog directives for the SWI-Prolog, B-Prolog YAP, SICStus, and XSB 
Prolog compilers. Added a new internal predicate for constructing 
predicate indicators for compiled entity predicates that simplifies the 
config files hooks used in handling proprietary Prolog directives.

Changed the valid values of the read-only compiler flag "break_predicate"
to "supported" and "unsupported" for consistency with other read-only 
compiler flags.

Updated the SWI-Prolog config file to set the Prolog flag "optimise" to 
true. Added declaration for the setup_and_call_cleanup/3 meta-predicate.
Removed setting of the deprecated "prompt_alternatives_no_bindings" 
Prolog flag. Oldest SWI-Prolog compatible version is now 5.6.44.

Updated the GNU-Prolog config file, adding a declaration for the 
call_det/2 proprietary meta-predicate. GNU-Prolog version 1.3.0 or 
later is now required.

Added list mapping predicates map/4 and map/5 to the "metap" library 
protocol and the "meta" library object.

Added a proper definition for the predicate select/3 to the "varlist" 
library object.

Added a new example, "logging", of using a category to define a set of 
predicates that handle a dynamic predicate in the context of "this" 
(i.e. in the context of the objects importing the category).

Added a new multi-threading example, "integration2d", implementing methods
for numerical integration for functions of two variables, contributed by 
Paul Crocker.

Updated the "integration" multi-threading example by bringing all example 
functions under the same object in order to allow for static binding. This
is essential for good results in operating-systems with poor mutex thread 
synchronization performance such as MacOS X.

Added experimental support for the GeSHi syntax highlighter, contributed 
by Clara Dimene.

Improved support for the Pygments syntax highlighter, correcting coloring 
bugs when there are single line comments in the same lines of an opening 
entity directive.


2.32.1 - July 7, 2008
=====================

Restored the redefined entity warnings for back-end Prolog compilers 
that support multifile predicates. Simplified updating of the runtime 
tables when compiling and loading source files with back-end Prolog 
compilers that don't support multifile predicates.

Changed the representation of the runtime tables for loaded entities 
in order to drop the need of a "functors clause" per entity. This avoids 
some redundancy on the representation of entity functors and also helps 
reducing a bit the size of the generated Prolog files.

When reloading an object, also clean all entries in the event handlers 
table where the object plays the role of a monitor.

Modified the Logtalk runtime to clean all lookup caches when compiling 
and loading source files (instead of doing it only when the files 
redefine existing entities).

Corrected a bug that prevented using predicate aliases with the :/1 
control construct without static binding.

Updated the CxProlog config file to take advantage and require the new 
0.97.1 version (which implements some more ISO Prolog predicates and 
expands support for text encodings).

Updated the B-Prolog config file to take advantage and require the new 
7.1 version (which adds support for the multifile/1 predicate directive).

Updated the SWI-Prolog config file notes about possible compatibility 
issues with the default setting of the proprietary "iso" Prolog flag 
to "true".

Updated the SWI-Prolog hook file, "swihook.pl", to avoid hijacking the 
arguments of use_module/1-2 calls (which resulted in wrongly loading 
Logtalk libraries with the same name as the SWI-Prolog library modules 
we intended to load).

Updated the "k6.config" config file for the K-Prolog 6.0.3 release.
Renamed the K-Prolog 5.1.x config file from "k.config" to "k5.config" 
to avoid being mistaken for the K-Prolog 6.0.x config file. Updated 
the "plclgt.sh" integration script to work with both K-Prolog 5.1.x 
and 6.0.x versions.

Renamed the SICStus Prolog 3 config file from "sicstus.config" to 
"sicstus3.config" to avoid being mistaken for the SICStus Prolog 4 
config file.

Updated the POSIX integration scripts and the Windows installer script 
to detect outdated versions of the Logtalk user folder (setting the 
compatibility version to 2.32.0 or later).

Improved the Prolog migration guide on converting code that makes use 
of multifile predicates.

Added note on licensing conditions to several files. Clarified copyright 
and license conditions for files on the "contributions" folder. Clarified 
dual-licensing conditions for text editor and syntax highlighter 
supporting files. Added license file to the TextMate bundle.

Updated support for the Vim text editor (changed the Logtalk file type 
plug-in to set the path to the completion dictionary and updated the 
installation instructions).

Updated the support for the Pygments syntax highlighter to avoid marking 
as errors non-standard directives.


2.32.0 - June 16, 2008
======================

Updated the Logtalk compiler and runtime to use multifile predicates for 
the runtime tables of loaded entities and entities relations. This is 
necessary to generate sensible intermediate Prolog code for large files 
with tens of thousands of objects. The workaround of using the directive 
initialization/1 is still available (for now) for Prolog compilers that 
fail to support the thirteen years old ISO Prolog standard.

Updated the Logtalk runtime error handler to test for non-instantiated 
exception terms. Thanks to Joerg Schuster for the bug report.

Corrected two bugs in the implementation of the built-in meta-predicate 
threaded/1 when canceling individual threads. The first bug resulted from 
out-of-order thread status messages. The second bug resulted from a leak 
of thread results between calls to the threaded/1 predicate. Changed the 
thread cancellation process to not automatically releasing any locks, 
leaving that task to a catcher associated to the thread goal.

Added a new default compiler option, multifile_directive, to all config 
files. Possible values are "supported" and "unsupported".

Added instructions on how to patch the Logtalk compiler in order to use 
Quintus Prolog as the back-end compiler.

Removed from the YAP POSIX integration script the command-line options 
setting the initial stack, heap, and tail data area sizes. These settings
are no longer necessary for recent YAP versions.

Updated the Windows installer script to search the registry for the 
location of the YAP installation (for YAP 5.1.3 or later versions).

Added missing implementation of the predicate as_dictionary/2 to the 
"bintree" library object. Thanks to Victor Noel for the bug report.

Updated all the search methods in the "searching" example to delegate 
checking for cycles to the state space being searched (thus allowing 
state descriptions to carry additional information that should not be 
taken into account when comparing states). Updated the "salt" example 
to support heuristics.

Updated the "mtbatch" example to include a benchmark test for competitive 
or-parallelism applied to state-space search (using the resources from the
"searching" example). Improved the example documentation.

Added a filetype plugin for the Vim text editor. Updated the installation 
instructions. Updated the syntax coloring support to properly highlight 
quoted atoms and strings in the opening directive of parametric objects. 
Fixed an auto-indentation bug when opening and closing a conditional 
block in a single line in the last goal a predicate clause body. Thanks 
to Victor Noel for the bug report.

Corrected missing installation of the "VERSION.txt" file when building 
Linux RPMs.


2.31.6 - May 26, 2008
=====================

Fixed a Logtalk runtime bug when using the :/1 control construct to 
call a predicate defined in a category extended by a category that 
is imported by the object making the call.

Fixed a Logtalk compiler bug when compiling an alias/3 predicate 
directive within a category that extends other categories.

Fixed a Logtalk compiler bug when compiling a class that imports a  
category that extends other categories. Thanks to Victor Noel for 
the bug report.

Small performance improvement to the caching of category predicates 
when using static binding.

Removed from the YAP config file the definition of the forall/2 
predicate, which have been added as a built-in predicate in YAP 
version 5.1.3 (the current YAP CVS version must be used; the last 
stable version, 5.1.2, is broken on Windows). Added support for 
the proprietary if/1, else/0, elif/1, and endif/0 directives (when 
used outside Logtalk entities).

Updated the SWI-Prolog config file with support for the proprietary 
if/1, else/0, elif/1, and endif/0 directives (when used outside 
Logtalk entities).

Improved the "complements" example, moving the category initialization 
goal to the beginning of the example source file.

Added support for the Pygments syntax highlighter (co-developed with 
Clara Dimene).

Added a new sample Logtalk source file for testing syntax coloring 
support.

Added missing auto-completion keywords for the ISO Prolog standard 
arithmetic functions to the SubEthaEdit, TextMate, Notepad++, jEdit, 
Vim, and Smultron text editors.

Corrected a bug in the SubEthaEdit syntax coloring support for the 
imports_category/2-3 built-in predicate. Added some missing files 
(license and mode settings) to the SubEthaEdit mode bundle.

Corrected a bug in the Vim syntax coloring support for the ISO Prolog 
built-in predicate current_char_conversion/2 and added missing support 
for for quoted atom escape sequences.

Corrected a bug in the jEdit syntax coloring support for hexadecimal 
numbers and added missing support for the extends_category/2-3 and 
at_end_of_stream/0 built-in predicates.

Added missing support for the syntax coloring of the external call 
Logtalk control construct, {}/1, to the Highlight package. Corrected 
a bug with 0'Char constants and with octal and hexadecimal escape 
sequences.

Added missing support for the syntax coloring of the external call 
Logtalk control construct, {}/1, and for quoted atom escape sequences 
to the TextMate text editor.

Added missing support for the syntax coloring of quoted atom escape 
sequences and of the built-in predicate at_end_of_stream/0 to the 
Source-highlight package.

Added missing support for the syntax coloring of the built-in predicate 
at_end_of_stream/0 and of the scope operator ::/2 to the Kate text editor.
Improved syntax coloring of parametric object opening directives.

Added missing support for the syntax coloring of the built-in predicate 
unify_with_occurs_check/2 to the Emacs text editor. Corrected a bug in 
the syntax coloring of arithmetic comparison operators.

Added missing support for the syntax coloring of quoted atom escape 
sequences and of the built-in control construct call/1 to the 
GtkSourceView 2.x text widget.

Added missing support for the syntax coloring of variables, of quoted 
atom escape sequences, of the built-in method clause/2, of the external 
call Logtalk control construct {}/1, and of variables to the Nedit text 
editor. Corrected a bug with 0'Char constants and corrected some typos 
in the support for the current_event/2, implements_protocol/2-3, and 
abolish_category/1 built-in predicates.


2.31.5 - April 29, 2008
=======================

Added support for checking arithmetic expressions for calls to 
non-portable functions when using the "portability" compiler flag.

Updated the implementation of the threaded/1 built-in predicate to 
ensure that thread creation errors (usually, virtual memory address 
space exhaustion) result in the corresponding exception rather than 
in a non-terminating call. Improved cancellation of all individual 
threads when one of them terminates with an exception or a failure.

Simplified the terms used to post individual results from threaded/1
calls to the queue associated to the call.

Corrected a bug in the built-in predicate threaded/1 when its argument 
is a conjunction (disjunction) of conjunctions (disjunctions).

Added a workaround for a mutex creation error when reloading a source 
file with entities defining synchronized predicates.

Updated the Logtalk runtime to unlock all mutexes hold by a thread when 
upon thread cancellation.

Corrected a Logtalk compiler bug that allowed a predicate to be declared 
both dynamic and synchronized. Thanks to Paul Crocker for the bug report.

Corrected a Logtalk compiler bug where local definition clauses for 
dynamic predicates are being generated for categories. Thanks to Victor 
Noel for the bug report.

Updated the YAP and SWI-Prolog config files to set the default value of 
the "tmpdir" flag depending on the host operating-system. Added missing
declaration for the multi-threading predicate thread_initialization/1.

Added missing declarations for some proprietary built-in meta-predicates 
to the B-Prolog config file.

Corrected a bug in the SWI-Prolog config file in the declaration of the 
proprietary built-in meta-predicate "soft cut". Thanks to Victor Noel 
for the bug report.

Updated the XSB config file to use the new optimized call/N predicates 
found on the current XSB CVS version.

Added an integration script, "xsbmt64lgt", for using Logtalk with the 
multi-threaded, 64 bits version of XSB.

Simplified building of MacOS X Installer packages. Updated the Windows 
installation script to use the "C:\lgtsvn" as base. Simplified manual 
installation instructions.

Updated the definitions of the predicate valid/1 for the library objects 
"list", "list(Type)", "numberlist", "set", "set(Type)", "varlist" to fail 
for lists with unbound tails after discussion with Jan Wielemaker and 
Ulrich Neumerkel.

Corrected a bug in the library object "lgtunit" when running "throws" 
tests (make sure the generated exception is subsumed by the expected 
exception). Added a "lgtunit_loader" loader utility file for loading 
the Logtalk unit test library.

Added a simple example, "debug_hooks", of using compilation hooks and 
term expansion for conditional compilation of debug statements.

Updated the "primes" multi-threading example to allow any number of 
threads to be used in the computation of primes numbers.

Added a new multi-threading example, "integration", implementing 
Recursive Gaussian Quadrature Methods for Numerical Integration for 
functions of a single variable, contributed by Paul Crocker.

Added a new multi-threading example, "mtbatch", for benchmarking 
multi-threading performance.

Added a new example, "ack", implementing the Ackermann function (general 
recursive function).

Added support for using the Highlight package (version 2.6.9 or later) 
by Andre Simon with Logtalk source files.

Updated the TextMate Logtalk bundle and its configuration instructions 
to make the "Compile" and "Generate ..." commands more general, making 
it compatible with most Prolog compilers. Added syntax coloring for 
standard arithmetic functions. Added a command for generating plain text 
files from XML documenting files.

Corrected a syntax coloring bug with character codes using the 0'Char 
notation in the SubEthaEdit 2.x and Vim text editors and in the
source-highlight package.

Removed some redundant regular expressions from the jEdit text editor 
syntax coloring support files.

Corrected syntax coloring bugs with variables starting with underscores 
(including anonymous variables) and with atoms containing an underscore 
in the Emacs text editor. Thanks to Joerg Schuster for the bug report.

Updated the Logtalk grammar documentation to reflect the changes to 
category relations introduced in version 2.31.0.

Added a "lgttxt.xsl" XSLT style-sheet and two shell scripts, "lgt2txt.sh" 
and "lgt2txt.js", for converting XML documenting files into text files.
Updated the "lgt2*.sh" shell scripts for POSIX compliance, removing 
dependencies on bash shell features.

Updated the "lgtxml.xsl" XSLT style-sheet to ensure that the generated 
HTML files are fully compliant with the HTML 4.01 standard.

Updated the Debian installer package shell scripts for POSIX compliance, 
removing dependencies on bash shell features.


2.31.4 - February 20, 2008
==========================

Optimized the performance of threaded/1 calls by using a per-call 
message queue for collecting the individual call results and by 
using the message queue identifier as a tag for the individual 
calls. This solution avoids runtime synchronization of a large 
number of threads on the same message queue, simplifies compilation 
and runtime handling of threaded/1 calls, and simplifies thread 
cancellation, eliminating any risk of dangling individual thread 
results.

Removed two redundant calls to the built-in predicate thread_exit/1 
on the Logtalk compiler.

Corrected a bug where a competitive or-parallelism call would 
prematurely fail with one or more individual calls still pending.

Corrected a bug where a competitive or-parallelism call would succeed 
when all the individual calls had failed.

Corrected a bug when compiling calls to the Logtalk multi-threading 
built-in predicates made from the top-level interpreter, i.e. from 
within the pseudo-object "user".

Added foreach/3, forto/5, and fordownto/5 meta-predicates to the 
library object "loop". Updated the definitions of the forto/3-4 
and fordownto/3-4 meta-predicates to allow the use of arithmetic 
expressions as arguments for convenience and clarity.

Corrected a bug in the implementation of the predicate lookup/3 in 
the library object "bintree".

Added a multi-threading example of the Fast Fourier Transform, "fft", 
contributed by Paul Crocker.

Corrected a bug in the implementation of the bisection algorithm in 
the multi-threading example "functions". Added an implementation of 
the MATLAB humps function (contributed by Paul Crocker).

Updated the multi-threading example "sorting" to workaround a mutex 
performance issue with XSB when generating lists of random numbers.

Updated support for the TextMate text editor. Added a command for 
generating the PDF documentation of open source files. Added missing 
tab triggers to the Logtalk snippets. Updated installation notes.

Corrected a bug in the jEdit syntax coloring support for the is/2 
operator.


2.31.3 - January 28, 2008
=========================

Added a "VERSION.txt" file that is used to check compatibility between 
an existing Logtalk user folder (whose path is stored in the LOGTALKUSER 
environment variable) and a new Logtalk version.

Updated the POSIX Prolog integration scripts to check for an outdated 
Logtalk user folder, creating a new one if necessary by running the 
"cplgtdirs" script (a backup is automatically made of the old directory).

Updated the Windows installer to create a registry key with the Logtalk 
version number and to check for an outdated Logtalk user folder.

Changed the predicate used to load Prolog files, adding an additional 
argument that represents a list of load/compile options (notably,
encoding/1, which is necessary for supporting source files encodings 
such as ISO-8859-X).

Updated the SICStus Prolog 4 config file to also accept UCS-2 encodings 
when using the encoding/1 directive (UCS-2 is subsumed by UTF-16).

Added a workaround to the Quintus Prolog integration scripts for its 
lack of support for expanding environment variables in file names.


2.31.2 - January 21, 2008
=========================

Extended compatibility of the experimental Logtalk encoding/1 directive 
to CxProlog 0.96.3 and SICStus Prolog 4.0.2 and improved support for YAP 
and SWI-Prolog.

Improved source file encoding handling by ensuring that a BOM present 
in a source file being compiled is inherited by the generated Prolog 
and XML files.

Changed the atoms used to represent different encodings when using the 
encoding/1 directive; Logtalk now uses the encoding names specified by 
IANA (using the preferred MIME name whenever available).

Updated the Logtalk compiler to throw an exception when the specified 
encoding is not supported by the used back-end Prolog compiler.

Updated the "encodings" example to use the new encoding names and added 
new source files using UTF-16 and UTF-32 text encodings.

Added POSIX and Windows integration scripts for Quintus Prolog.


2.31.1 - January 3, 2008
========================

Duplicated the range of threaded_call/2 tags for multi-threading Prolog 
compilers with bounded integers.

Updated the YAP config file to set the flag "language" to "iso". This 
is not strictly necessary to run Logtalk with YAP but it helps prevent 
nasty surprises when writing portable applications that rely on ISO 
Prolog semantics.

Updated the SWI-Prolog integration script to test for the availability of
the XPCE library before trying to load the "xpcehook.pl" XPCE hook file.

Updated the Linux (RMP and Debian) and MacOS X package build scripts to 
include Prolog version compatibility information.

Updated the "tak" multi-threading example to allow parameterization of 
the number of threads to use.

Updated the "sorting", "hanoi", and "fibonacci" multi-threading examples 
to interpret the object parameter as the number of leaf threads (i.e. 
working threads that will not recursively create additional threads).

Updated the TextMate text editor support to include commands to compile 
and to automatically generate the XHTML documentation of open source 
files.


2.31.0 - December 21, 2007
==========================

Added a new built-in protocol, "expanding", declaring goal_expansion/2
and term_expansion/2 public predicates.

Added compiler support for user-defined goal expansion hooks. Changed 
the implementation of compiler term and goal expansion hooks to use 
object-defined predicates term_expansion/2 and goal_expansion/2.
Changed the compiler option "hook" to accept object identifiers.

Allow a category to explicitly complement an existing object, thus 
providing functionality similar to Objective-C categories. Added 
complements_object/2 built-in predicate. Added a new example, 
"complements", illustrating the new category functionality.

When constructing a new category from other categories, the relation 
"extends" is now used instead of the relation "imports". The relation 
"imports" is now only used when an object imports a category. Added  
extends_category/2-3 built-in predicates.

Modified the built-in predicate define_events/5 to throw an existence 
error instead of failing when the specified monitor object does not 
exists. 

Updated the Logtalk compiler to convert version numbers to atoms when 
generating XML documenting files (thus avoiding problems with some 
Prolog compilers that resulted in version numbers with a large number 
of fractional digits).

Improved updating of runtime bookkeeping tables when loading source 
files.

Renamed the predicate property "alias/1" to "alias_of/1" in order to 
disambiguate its meaning.

Added a new config file, "k6.config", for the K-Prolog 6.0 beta 
version.

Updated the B-Prolog config file to recognize mode/1 directives.
Added some missing meta-predicate specifications to the config files 
of XSB and YAP.

Updated the Windows GUI installer to support running Logtalk with 
CxProlog 0.96.1 and later versions.

Added two new multi-threading examples, "fibonacci" and "hanoi", and 
corrected a bug in the recursive creation of threads on the "sorting" 
example. Updated the "primes" multi-threading example to also support 
eight threads.

Updated the "hooks" example for illustrating goal expansion hooks in 
addition to term expansion hooks.

Updated the "engines" example for compliance with the changes made 
to category relations.

Added syntax coloring support for the new goal_expansion/1, 
complements_object/2, and extends_category/2-3 predicates to all 
supported text editors.

Updated the TextMate code snippets to use the TM_FULLNAME variable 
instead of the deprecated "niutil" command to retrieve the user full 
name. Thanks to Michael Sheets for the update. 


2.30.8 - November 9, 2007
=========================

Fixed bug in the compilation of synchronized predicates that breaks 
Logtalk on single-threaded Prolog compilers.


2.30.7 - November 5, 2007
=========================

Updated the multi-threading built-in predicates threaded_notify/1 and 
threaded_wait/1 to accept lists of notifications.

Added a new read-only compiler flag, "context_switching_calls", allowing 
context switching calls to be disabled (they are enabled by default).

Updated the B-Prolog config file to recognize eager_consume/1 directives.
Updated the XSB config file to recognize use_subsumptive_tabling/1,
use_variant_tabling/1, index/1, thread_private/1, and thread_shared/1 
directives.
Updated the YAP config file to recognize thread_local/1 directives.
Updated the SICStus Prolog config files to recognize volatile/1 directives.
Updated the SWI-Prolog config file to recognize thread_local/1, index/1, 
and hash/1 directives.

Changed the backup directory names generated by the "cplgtdirs.*" shell 
scripts to not include whitespace characters.

Updated the "xsbmtlgt.sh" integration script to start XSB-MT using the 
new command-line option "--shared_predicates" (requires current XSB CVS 
version). Updated the Windows GUI installer to support running Logtalk 
with XSB-MT.

Added a multi-threading example, "barriers", of barrier synchronization.

Updated the "functions" example sample queries for compatibility with the 
ISO Prolog standard regarding representation of float numbers.

Added a workaround for a parsing bug in XSB while compiling the "sicstus" 
example.


2.30.6 - October 21, 2007
=========================

Replaced calls to the proprietary current_thread/2 predicate in the 
Logtalk runtime by calls to the thread_property/2 predicate as per 
the ISO DTR on multi-threading predicates. Added a goal_expansion/2 
clause to the SWI-Prolog config file to automatically switch between 
the current_thread/2 and thread_property/2 predicates depending on 
availability.

Changed handling of the encoding/1 directive by the Logtalk compiler in 
order to improve compatibility with YAP and SICStus Prolog. Removed from 
the config files the now obsolete '$lgt_set_stream_encoding'/2 predicate 
definition. Renamed the compiler option "supports_encoding_dir" to 
"encoding_directive" and changed its possible values to "unsupported", 
"full" (used in both Logtalk source files and compiler generated Prolog 
files), and "source" (used only in Logtalk source files).

Renamed compiler option "underscore_vars" to "underscore_variables".
Renamed compiler option "supports_break_predicate" to "break_predicate".

Improved the performance of recursive predicate definitions synchronized 
by mutexes. Explicitly create all mutexes used by an entity at load time 
for compatibility with XSB. Several changes to multi-threading support in 
order to improve compatibility with current and forthcoming versions of 
YAP, SWI-Prolog, and XSB.

Updated the Logtalk compiler for compatibility with both logical and 
immediate update semantics when compiling synchronized predicates.

Updated the meta-predicate compilation sanity checks to verify the 
existence of the minimum number of normal arguments in a clause head 
required by the closure of maximum arity.

Added a workaround for the lack of built-in support for character 
unbuffered input when using the Logtalk built-in debugger to the config 
files of ALS Prolog, B-Prolog, Ciao, CxProlog, IF/Prolog, JIProlog, 
Prolog II+, SICStus Prolog, XSB, and YAP. Thanks to Parker Jones for the 
bug report.

Updated the YAP config file to auto-detect working Unicode support when 
setting the "encoding_directive" compiler option.

Updated the XSB config file to auto-detect multi-threading support
when setting the "threads" compiler option. Added an integration script 
(xsbmtlgt.sh) and supporting files for the multi-threaded version of XSB 
(requires current development version, available from the XSB CVS server).

Added a MacPorts portfile for building MacOS X installer packages.

Added a BOM to the "babel.lgt" UTF-8 file in the "encodings" example in 
order to improve compatibility with SICStus Prolog.

Updated the library object "random" by replacing the synchronized/0 
directive by a synchronized/1 directive listing only the predicates 
that modify the random number seed.

Added syntax coloring support for the GtkSourceView 2.x text widget 
(used e.g. on the Gnome's Gedit text editor and on the MonoDevelop IDE).
Removed syntax coloring for the obsolete atomic/1 predicate directive 
from the support files of Source-highlight, Emacs, SubEthaEdit, jEdit, 
and Kate. Optimized the regular expressions used in most syntax coloring 
configuration files of supported text editors.


2.30.5 - September 19, 2007
===========================

Added new multi-threading built-in predicates threaded_call/2, 
threaded_once/2, threaded_exit/2, and threaded_peek/2. These new 
predicates support the use of threaded call identifier tags in order to 
link specific threaded_call/2 and threaded_once/2 calls to specific 
threaded_exit/2 and threaded_peek/2 calls. Extended the "nondet" example 
in order to illustrate the new functionality.

Changed the implementation of the built-in predicate threaded_exit/1 in 
order to avoid blocking when its argument is subsumed by the argument of 
the corresponding threaded_call/1 call instead of being a variant.

Updated the Logtalk compiler to encapsulate resource errors inside 
error/2 exception terms (as specified in the ISO Prolog core standard).

Corrected a bug in the library object "lgtunit" (wrong arity on two 
failed_test/2 predicate calls).

Corrected two problems with the "testing" example: a wrong call on the 
object "dyn_tests" and a missing entry in the "libpaths.pl" file. Removed 
two pointless dynamic predicate directives in the "buffer" example.

Improved documentation of multi-threading programming and predicate 
properties. Corrected the reference manual page describing the threaded/0 
directive.

Simplified installation scripts by moving shared distribution cleaning 
code into the "cleandist.sh" script. Updated the "install.sh" script to 
set the installation prefix to "/usr" on Debian systems. Updated the 
Linux RPM spec file in order to clean the building directory after the 
package creation.


2.30.4 - August 22, 2007
========================

Allow the argument of {}/1 calls to be a variable at compile time.

Updated the Qu-Prolog config file to match and require version 8.1 and 
added a "qplgt" integration script.

Updated the XSB config file to match and require version 3.1 and to 
take advantage of the compiler option optimize/1 when loading Logtalk 
generated Prolog intermediate files.

Added a new library object, "lgtunit", providing preliminary support for 
writing and running unit tests. Added a new example, "testing", defining 
sample unit tests.

Added a new multi-threading example, "threads/tak", implementing the 
Takeuchi function (recursive arithmetic).

Updated the "threads/philosophers" example to use notifications instead 
of a dynamic predicate for representing chopstick availability. Added an 
alternative implementation using a parametric object. Improved example 
documentation.

Updated the "benchmarks" example to allow comparisons between calls to 
imported category predicates and calls to local object predicates.

Updated Emacs support in order to fix problems with the syntax-coloring 
of some Logtalk built-in predicates and applied a patch contributed by 
Nicolas Pelletier to workaround a compatibility problem with XEmacs.

Updated jEdit support in order to fix a problem with the syntax-coloring 
of the :/1 control construct.


2.30.3 - July 9, 2007
=====================

Updated the multi-threading built-in predicate threaded/1 to support 
both conjunctions of goals (akin to and-parallelism) and disjunctions 
of goals (akin to or-parallelism) as an argument.

Removed the experimental threaded_race/1 multi-threading built-in 
predicate (its or-parallelism functionality is subsumed by the updated 
threaded/1 predicate).

Corrected a bug in the implementation of the multi-threading built-in 
predicate threaded_exit/1 when there are more than one thread running 
the same non-deterministic goal (the fix ensures that all alternative 
solutions per threaded_exit/1 call come from the same computing thread). 
Thanks to Paul Crocker for the suggested bug fix solution.

Updated the sample queries in the "threads/buffer"/SCRIPT.txt" file to 
match the updated entities in the example.

Updated the "threads/functions" example to use the new version of the 
threaded/1 multi-threading built-in predicate.


2.30.2 - June 24, 2007
======================

Updated the Logtalk compiler to throw a compilation error when duplicated 
or conflicting predicate scope directives are found.

Updated the Logtalk compiler to correct a cosmetic glitch when reporting 
compilation errors.

Updated the Logtalk compiler to check for mismatches between the argument 
of this/1 calls and the parametric object identifier.

Corrected a bug in the implementation of the multi-threading built-in 
predicate threaded_ignore/1.

Revamped the "threads/buffer" example to support setting the buffer 
maximum number of items.

Added a new DCG example, "dcgs/morse.lgt", for parsing messages in Morse 
code.

Extended the "parametric" example to illustrate a solution for dealing 
with inheritance when defining "setter" predicates/methods that return 
updated object identifiers.


2.30.1 - June 12, 2007
======================

Added a new, experimental control construct, :/1, for calling imported 
category predicates without using message sending mechanisms.

Added preliminary support for static binding when calling imported 
category using the new :/1 control construct for categories compiled 
and loaded using the compiler option "reload(skip)".

Added a new control construct, <</2, which allows proving a goal within 
the context of a specified object. Useful for debugging and for writing 
object unit tests.

Improved multi-threading support in order to make the built-in predicate 
threaded_exit/1 generate an exception instead of blocking indefinitely 
waiting for a reply from a non-existing thread.

Updated the Logtalk compiler to generate an error when compiling from a 
source file new definitions for built-in entities.

Updated the Logtalk compiler to generate simpler clauses for the book-
keeping table of local predicate definitions for parametric entities.

Updated the config files for ECLiPSe, Ciao Prolog, SICStus Prolog, 
SWI-Prolog, and YAP to use the renamed hook predicates introduced in 
Logtalk 2.30.0 for parsing Prolog proprietary directives.

Updated the "benchmarks" example to compare the performance of calls to 
imported category predicates when using a message to "self" and a direct 
call.


2.30.0 - May 28, 2007
=====================

Added preliminary support for static binding when sending messages to 
objects compiled and loaded using the compiler option "reload(skip)".

Allow non-instantiated arguments at compile time for the multi-threading 
built-in predicate threaded/1.

Simplified and optimized the implementation of Logtalk multi-threading 
support. Use a single dispatcher thread for all objects instead of each 
object running its own dispatcher in order to minimize the number of 
created threads (which can be problematic on 32 bits systems). Updated 
the built-in predicate threaded/1 to make it deterministic and opaque to 
cuts (similar to once/1).

Updated the Logtalk built-in database methods to always interpret rules 
whose body is the control construct true/0 as facts. Corrected a bug when 
compiling dynamic predicates that would prevent using of the clause/2 
built-in database method. Corrected a bug when using the Logtalk built-in 
database method clause/2 with entities compiled in debug mode. Improved 
predicate lookup caching when asserting and retracting dynamic facts.

Improved detection and reporting of entity existence errors.

Added a quit option, "q", to the Logtalk built-in debugger. Modified 
the debugger behavior to automatically switch to trace mode when a spy 
point is found.

Added two new compiler options, "xmldir" and "tmpdir", allowing per 
project definition of directories for storing XML documenting files 
and for storing intermediate compilation files (e.g. Prolog files).
Removed the config file predicate '$lgt_alt_directory'/2.
Older config files are incompatible with this new Logtalk version.

Added a shortcut to the Logtalk built-in predicate logtalk_load/1, {}/1,
to all config files (defined there in order to be easy to comment it out 
in case of conflict with some Prolog native feature or lack of compliance
with the ISO Prolog standard regarding the definition of the {}/1 syntax.

Allow the compiler flag "misspelt" to be set to "error".

Updated the Logtalk compiler to not try to clean-up the dynamic predicates
of redefined entities when reloading source files. Most Prolog compilers 
already behave as expected when reloading the intermediate Prolog files 
generated by the Logtalk compiler. For those Prolog compilers that do not 
replace old definitions for dynamic predicates with the new ones, it is 
not possible for Logtalk to implement a workaround that would work 
correctly in all cases. Consult the "configs/NOTES.txt" file for more 
information.

Corrected a bug that prevents abolishing a dynamic entity loaded from a 
source file. Thanks to Victor Noel for the bug report.

Renamed the '$lgt_copy_pl_directive'/1 and '$lgt_rewrite_pl_directive'/2 
config files predicates to '$lgt_rewrite_and_copy_pl_directive'/2 and 
'$lgt_rewrite_and_recompile_pl_directive'/2. 

Updated the config file for B-Prolog to match (and require) the new 7.0 
version; updated and simplified the corresponding integration scripts.

Updated the XSB POSIX integration script to automatically detect if we 
are running the XSB stable version of the XSB CVS version.

Added basic support for tabling to the config files of B-Prolog, XSB, 
and YAP. Added a simple example of using tabling directives within 
objects.

Updated the SWI-Prolog and YAP config files to automatically detect if 
the Prolog systems have been compiled with multi-threading support and 
to set the Logtalk compiler flag "threads" accordingly.

Corrected a bug when running Logtalk with SWI-Prolog that prevented the 
use of alternative compilation directories (i.e. turning on the "altdirs" 
compiler flag). Corrected a bug when running Logtalk with SWI-Prolog that 
can prevent make/0 from recompiling and reloading modified Logtalk source 
files. Updated the SWI-Prolog integration script, "swilgt.sh", to more 
reliably detect the compiler executable name.

Added a "configs/swi_set_logtalk_context.pl" file defining a useful but 
fragile hack implementing a set_logtalk_context/1 built-in predicate for 
switching the Logtalk top-level execution context to objects other than 
the default pseudo-object "user" (you may use it as a debugging tool).

Updated the CxProlog config file to use the new built-in predicates in 
version 0.95. Added a shell script, "cxlgt.sh", for easy integration of 
Logtalk with CxProlog on POSIX systems (with the help of Artur Miguel 
Dias, CxProlog author).

Updated the Ciao, K-Prolog, and ECLiPSe config files to set the default 
value for the compiler flag "underscore_vars" to "dont_care" in order to 
avoid spurious warnings with some of the provided examples.

Added a config file and integration scripts for the current XSB CVS 
version (3.0.1+), which supports some features needed by Logtalk not 
present in the current stable version (namely, expansion of environment 
variables).

Added predicates depth/2 and variant/2 to the library object "term".

Much improved "benchmarks" example, updated to allow running tests in 
three different scenarios: event support on, event support off, and 
using static binding. Moreover, metacalls are no longer used to run 
the benchmark goals, resulting in more meaningful and accurate results.

Removed all the "make_*lgt.*" and "makeall_lgt.*" shell scripts, replaced 
by pre-made integration scripts that can be found on the new "integration"
directory. Removed the "lgt_install.js" script. Renamed the POSIX install 
and uninstall scripts to, respectively, "install.sh" and "uninstall.sh".
Updated the integration, uninstall, and user-setup POSIX shell scripts 
to use "$prefix/share/logtalk" as the default installation directory to 
better comply with the Filesystem Hierarchy Standard 2.3 for UNIX-like 
operating systems.

Updated the integration scripts to automatically call the "cplgtdirs" 
script when the Logtalk user directory cannot be located.

Updated the integration, documentation, uninstall, and user-setup POSIX 
shell scripts to try to locate the Logtalk installation directory and the 
Logtalk user directory when the environment variables LOGTALKHOME and 
LOGTALKUSER are not set.

Updated the "install.sh" POSIX shell script to better clean and sanitize 
the file permissions on the installation directory.

Updated the "build_release.sh" POSIX shell script to also build a Debian 
binary installer package (experimental).

The Windows GUI installer no longer spans command-line shells running 
JScript scripts to build the Prolog integration shortcuts. This hopefully 
solves issues with security software silently blocking and breaking the 
Logtalk installation.

Added basic syntax coloring support for the GNU Nano 2.x text editor.


2.29.5 - March 28, 2007
=======================

Added a new built-in predicate, threaded/1, for proving each goal in 
a conjunction in its own thread, simplifying common multi-threading 
tasks that previously required sequences of calls to the built-in 
predicates threaded_call/1 and threaded_exit/1.

Simplified and optimized the implementation of Logtalk multi-threading 
support.

Simplified implementation of the database built-in methods. Simplified 
caching of dynamic predicate lookups. Improved performance of the 
built-in methods retract/1 and retractall/1.

Simplified the code generated when compiling entities in debugging mode.

Corrected a bug in the built-in debugger when processing actions at a 
port that implies reading a port action again (e.g. print exception term,
print debugging status, and help options). Allow the debugger command 
"skip" to work as the command "creep" in ports other than "call" and 
"redo". Added a new debugger command, "ignore". Suspend debugging when 
using the "break" debugger command.

Generate a compilation error instead of just printing a warning when 
compiling calls to the multi-threading built-in predicates within an 
object with no threaded/0 directive present.

Corrected a bug when compiling entities containing synchronization 
directives on single-threaded Prolog configurations.

Improving reporting of working directory when loading or compiling source 
files by expanding any environment variables occurring in the path.

Added a new compiler option, "reload", for defining the Logtalk behavior 
when reloading source files. Valid values are "always" (default value; 
defined in the config files) and "skip".

Updated the programming examples to use the new "reload" compiler option 
when loading library entities. Simplified loading of example source files 
(by updating the utility loader files to automatically load all required 
library files).

Updated GNU Prolog config file to take advantage of some built-in list 
predicates (requires version 1.2.14 or later); changed the compiler flag 
"underscore_vars" value to "dont_care" to avoid spurious warnings with 
some examples.

Added a config file for B-Prolog 7.0b1.

Updated the POSIX integration shell scripts to pass along any command 
line options to the corresponding Prolog compiler and to prevent some 
problems with paths containing spaces.

Much improved syntax coloring and code completion support for the jEdit 
text editor.

Updated the "threads/primes" example to use the new "threaded/1" built-in 
predicate. Updated the "threads/birthdays" example to better illustrate 
the use of the built-in multi-threading predicates. Added a new 
multi-threading example, "msort", implementing single-threaded and 
multi-threaded versions of the merge sort algorithm.

Expanded the "operators" example to illustrate a simple solution for 
making operators local to source files (but global to all entities 
defined within the source files).


2.29.4 - February 19, 2007
==========================

Added a new library category, "listing", defining listing/0 and 
listing/1 methods for helping in debugging tasks.

Added two new experimental multi-threading predicates, threaded_wait/1 
and threaded_notify/1, which allows suspension of a thread's goal until  
a notification is received. Added two new examples, "threads/buffer" and 
"threads/blackboard", illustrating the use of these predicates.

Simplified and changed implementation of the threaded built-in 
predicates in order to ensure that the threaded/0 directive is only 
required on objects calling these predicates.

Only print a warning for a missing threaded/0 directive for objects.

Corrected a ordering bug on the entity initialization goal that 
prevented using an object initialization goal that make use of 
the object thread. 

Corrected a bug when asserting rules into an object's database that
resulted in only the first asserted rule being visible when calling 
the predicate through message sending. Thanks to Parker Jones for 
the bug report.

Corrected a bug when compiling dynamic entities (defined in source 
files using the dynamic/0 directive) where declared predicates would 
be wrongly compiled as static instead of dynamic.

Updated the "make_xsblgt.sh" integration script again to fix an 
additional problem resulting from the lack of support in XSB for 
using environment variables in file paths.

Added minimal support for using Exuberant Ctags with Logtalk.

Added auto indent support to the jEdit text editor.

Added support for listing public predicates in the "Functions" window 
of the SuperEdi text editor.

Converted TextMate code snippets from the old Property List format 
to XML in order to provide compatibility with the "e" Windows text
editor.

New example: 99 bottles of beer on the wall! Sing along!


2.29.3 - January 15, 2007
=========================

Corrected a bug in the Logtalk compiler optimizer code which was 
discarding some declaration and definition catchall clauses needed 
by the Logtalk built-in database methods when there is no initial 
declaration and definition for the referenced dynamic predicates.
Thanks to Parker Jones for the bug report.

Corrected a bug in the compilation of empty, standalone protocols 
and categories.

Updated the "make_xsblgt.sh" integration script to workaround 
the lack of support in XSB for using environment variables in 
the argument of the reconsult/1 predicate.

Corrected a bug in the "potions.lgt" example puzzle that prevented 
its solution of being found.


2.29.2 - January 10, 2007
=========================

Silently compile synchronized predicates as normal predicates on 
single-threaded Prolog compilers.

When using the threaded_race/1 predicate, competing threads are 
now created detached.

Corrected a bug that resulted in a loading error when reloading 
source files defining threaded objects.

Corrected a bug in the implementation of the built-in predicate 
threaded_peek/1 that prevented alternative solutions from being 
retrieved using the built-in predicate threaded_exit/1. Removed 
the built-in predicate threaded_discard/1.

The library object "random" is now a synchronized object. Updated 
the "philosophers" multi-threading example accordingly.

Dropped loading of broken "cleanup" library from the YAP config file 
(the call_cleanup/2 predicate, required for Logtalk multi-threading, 
is now available as a built-in predicate in the YAP CVS version).


2.29.1 - December 28, 2006
==========================

Added a Logtalk version of John Fletcher's Prolog XML parser (see the 
folder "contributions/xml_parser").

Added shell scripts for helping building the distribution files of a 
new Logtalk release. Updated the MacOS X installer package to set 
default values for the Logtalk environment variables. Corrected a bug 
in the "logtalk.spec" file where the default value for the LOGTALKUSER 
environment variable only worked for the user doing the RPM installation.

Corrected a bug in the reporting of the line number where a compilation 
error (or warning) occurred. Extended support for reporting warning and 
error line numbers to Quintus Prolog, Ciao Prolog, Qu-Prolog, and 
ECLiPSe.

Corrected a bug when using partial lists with the predicates nth0/3-4 
and nth1/3-4 provided by the "list" library object.

Improved "philosophers" multi-threading programming example.


2.29.0 - December 18, 2006
==========================

Added a new built-in, empty object named "logtalk", which can play the 
role of both a class and a prototype. It may be used to define class 
hierarchies without forcing the use of metaclasses or reflective 
designs.

Added a built-in protocol named "monitoring" with declarations for 
the before/3 and after/3 public event handler predicates. Updated 
the Logtalk compiler to print a warning when defining an event handler 
with no reference to the "monitoring" protocol.

The default value of the compiler flag events/1 is now off. As most 
applications do not use events, this setting ensures the best possible 
message processing performance for those applications.

Removed the experimental threaded_call/2 and threaded_exit/2 thread 
predicates, replaced by new threaded_once/1, threaded_ignore/1, 
threaded_race/1, threaded_peek/1, and threaded_discard/1 predicates.
Renamed predicate directive atomic/1 to synchronized/1. Added new 
entity directive synchronized/0. Added support for new "synchronized" 
and "threaded" entity properties.

Corrected a bug when using threaded calls as initialization goals 
within threaded objects.

Corrected an elusive bug in the Logtalk compiler where a source file 
may not be properly closed when it contains a syntax error (thanks 
to Robert Shiplett for the bug report).

Corrected a bug in the implementation of the built-in methods retract/1 
and retractall/1 that could result in unexpected exceptions. Updated 
the built-in method assertz/1 to always use the lookup cache.

Corrected a bug in the compilation of empty classes that are not 
instances of a metaclass.

Corrected a bug in the built-in method predicate_property/2 where some 
predicate properties may not be returned when enumerating properties 
using backtracking.

Modified the SWI-Prolog config file and integration scripts to set 
the flag "iso" to "true". Corrected an elusive bug in the definition 
of predicate '$lgt_directory_exists'/1 that resulted in "library not 
found" errors when loading Logtalk source files from within the 
SWI-Prolog initialization file (thanks to Robert Shiplett for the bug 
report).

Updated the K-Prolog config file to workaround a problem when using 
library notation due to failure to check directory existence with the 
provided "libpaths.pl" file.

Added a RELAX NG file describing the structure of the XML documenting 
files generated by Logtalk.

Corrected a bug in the Logtalk DTD file (missing "copyright" tag 
declaration).

Corrected a bug in the lgt2*.js scripts where the "custom.ent" file 
was not being copied when generating (X)HTML and PDF files.

Added a new multi-threading example, "philosophers", illustrating a 
possible implementation of the "dining philosophers" problem.

Improved the "metapredicates" example in order to illustrate the use 
of closures instead of goals as meta-arguments.

Added a reference to the "e" Windows text editor, which supports 
the MacOS X TextMate 1.x text editor syntax configuration files.

Updated the manuals index pages in order to workaround browsers bugs 
with parsing of empty "span" tags (e.g. Internet Explorer and Opera).


2.28.2 - November 6, 2006
=========================

Corrected a compiler bug where unknown entities will not be report when 
compiling a source file whenever an entity with the same name but of a
different type is already known.

Added a XPCE hook file ("configs/xpcehook.pl") supporting Logtalk message 
sending goals as XPCE call-back goals. The XPCE library is now loaded by 
default by the SWI-Prolog integration scripts.

Added support for generating single PDF files of the User and Reference 
Manuals. Removed the PDF versions of the individual manual XHTML pages.

The ECLiPSe config files now require release 5.10#26 or a later version.

Added support for listing Logtalk entity names (objects, categories, and 
protocols) in the SubEthaEdit symbols menu.

Added a Vim dictionary file for Logtalk keyword completion and support 
for automatic indentation.

Added basic support for the MacOS X Smultron text editor. Added basic 
support for the Notepad++, SuperEdi, and ConTEXT Windows text editors.


2.28.1 - October 10, 2006
=========================

Added support for using XML entities using the notation "{entity name}" 
in info/1 documenting directives for the "author", "copyright", and 
"license" keywords. The XML entities are defined on a "custom.ent" file.

Updated Logtalk bibliographic references (in file "BIBLIOGRAPHY.bib").

Added a "lgt_uninstall.sh" shell script for uninstalling Logtalk on POSIX 
operating systems.

Improved RPM install-time messages on the defined Logtalk environment 
variables.

Updated the ECLiPSe integration scripts and config files to work with 
and require version 5.10 (the first open source version of ECLiPSe).

Corrected a typo on the "logtalk.dtd" file that prevents validation of 
XML documenting files that use a local reference to the DTD.


2.28.0 - September 28, 2006
===========================

Updated the Logtalk license to the final version of the "The Artistic 
License 2.0" (see http://www.perlfoundation.org/legal/ for details).

Added experimental support for multi-threading programming on selected 
Prolog compilers. Added a new object directive, threaded/0, a new 
predicate directive, atomic/1, and four new built-in predicates, 
threaded_call/1-2 and threaded_exit/1-2. Added a new set of examples, 
"threads". Thanks to Paulo Nunes for helping developing and testing 
these new features.

Expanded support for meta-predicates by allowing the declaration of meta-
arguments as closures instead of goals.

Added the generalized call/N predicate as a built-in method. This built-in 
method must be used in the implementation of meta-predicates which work 
with closures instead of goals.

The metapredicate/1 directive should be considered deprecated. Use the 
meta_predicate/1 directive instead.

Updated the Logtalk compiler to generate an error whenever a non-variable 
meta-argument is found in a clause head of a user-defined meta-predicate.

Improved compilation of meta-predicate meta-arguments. Corrected a bug 
in the compilation of user meta-predicates that allowed illegal calls 
to non-public predicates on the caller object to be made from the object 
defining the meta-predicates. Thanks to Rémy Haemmerlé and François 
Fages for reporting the problem.

Improved performance for non-cached messages, notably when running with 
YAP, GNU Prolog, and SWI-Prolog on all operating systems and with SICStus 
Prolog 4.0 on POSIX operating systems.

Updated the Logtalk compiler to generate an error whenever a predicate 
directive appears in a source file after predicate clauses that call the 
predicate specified in the directive.

Added support for "copyright" and "license" keys to the info/1 entity 
documenting directive.

Improved compiler error messages to also report source file line number 
for selected Prolog compilers.

Added PDF versions of all the manual XHTML pages. Added links to all the 
manual XHTML pages to the corresponding PDF versions.

Corrected a bug (introduced in version 2.27.0) in the built-in predicates 
abolish_object/1, abolish_protocol/1, and abolish_category/1 that resulted
in some clauses not being retracted when an object is abolished (thanks to 
Neng-Fa Zhou for the bug report).

Corrected a bug (introduced in version 2.27.1) in the processing of the 
term_expansion/2 predicate.

Corrected a bug in the compilation of redefined built-in predicates with 
no clauses.

Added an experimental config file for CxProlog 0.93.1.

Updated the SWI-Prolog config and hook files in order to support the 
edit/1 and make/0 development predicates.

Changed the Logtalk - SWI-Prolog integration scripts to load the hook 
file "swihook.pl" after the other files in order to avoid some possible 
errors at startup.

Updated the config file for the forthcoming SICStus Prolog 4.0 version 
to work in the current beta version. Modified the "make_sicstuslgt.*" 
shell scripts to work with both SICStus Prolog 3.12 and 4.0 versions.

Updated the config file for YAP to workaround a problem on the Windows 
version where testing for directory existence fails if the path ends with 
a slash (or a double backslash); this problem prevents the use of library 
notation to load source files (problem already fixed in the current YAP 
CVS version).

Updated the config file for B-Prolog to match (and require) the new 6.9 
version; added integration shell scripts (for both Windows and POSIX 
systems).

Updated the config files and the integration scripts for ECLiPSe in order 
to workaround bugs on the predicates abolish/1 and canonical_path_name/2.
Modified the "make_eclipselgt.js" script to work with both ECLiPSe 5.8 
and 5.9 versions.

Dropped support for versions of XSB older than 3.0. Added support for 
smart compilation of source files to the XSB config file.

Updated the config file for IF/Prolog 5.1 by adding a missing definition 
for predicate retractall/1 and avoiding some harmless singleton warnings.

Add a ".txt" extension to all "NOTES" and "SCRIPT" files.

Extended "dcgs", "parametric", and "metainterpreters" examples.

Added Inno Setup GUI Windows installer script.

Update all the JScript scripts to check if a compatible version of WSH 
is installed and to workaround some problems with spaces in file paths.

Updated the "lgt_install.js" JScript script to copy the scripts/*.bat and 
the xml/*.bat batch scripts to the system's Windows directory instead of 
modifying the PATH environment variable.

Updated the "cplgtdirs.*" shell scripts to make a backup copy of any 
previous LOGTALKUSER directory.

Added post-install scripts to the LINUX RPM "logtalk.spec" file for 
setting the environment variable LOGTALKHOME for all users, defining 
a default value for the environment variable LOGTALKUSER, and running 
the Prolog integration scripts.

Split the installation and customization instructions in two files, 
"INSTALL.txt" and "CUSTOMIZE.txt".

Added shell scripts for converting the manual XHTML pages into PDF files 
using CSSToXSLFO.


2.27.1 - March 27, 2006
=======================

Allow calls to the built-in method parameter/2 with the first argument 
not instantiated at compile time. Detect and report as a compilation 
error parameter/2 indexes that are out of range.

Optimized generation of predicate definition and declaration linking 
clauses, resulting in small lookup performance improvements and in space 
savings for the Prolog code generated when compiling Logtalk entities.

Many minor code and documentation improvements to the Logtalk compiler.

Added an object version of the tokenizer described in the Michael 
Covington's paper "Tokenization using DCG Rules" to the "dcgs" example.

Improved integration code for Qu-Prolog and Logtalk ("configs/qphook.ql").

Improved library hierarchy methods that return lists of objects in order 
to avoid duplicated elements (library objects "proto_hierarchy.lgt" and 
"class_hierarchy.lgt"). Added new methods extension/1 and extensions/1 to 
the library object "proto_hierarchy.lgt".

Documented the concept of "parametric object proxy". Added a new example, 
"proxies", illustrating the use of parametric object proxies.

Added support for code completion and for listing entity names on the 
symbol pop-up menu to the MacOS X TextMate text editor.

Updated the "cplgtdirs.*" scripts to also create an alias/shortcut to the 
"coding" directory.

Renamed the alternative compilation and documentation directory names on 
all config files to be compatible across operating-systems. Removed unused
predicate '$lgt_reverse'/2 from all config files.

Updated HTML manuals "print.css" CSS file for compatibility with the 
latest version of CSSToXSLFO.


2.27.0 - February 9, 2006
=========================

Improved performance for local calls to the built-in methods phrase/2-3.
Allow the built-in methods phrase/2-3 to accept both partial and proper 
lists of terminals.

Improved grammar rule translator. Report calls to undefined non-terminals 
when translating grammar rules.

Added support for declaring grammar rule non-terminal aliases using the 
alias/3 directive.

Added a new predicate property, non_terminal/1, for predicates resulting 
from the compilation of grammar rule non-terminals.

Improved support for the built-in method expand_term/2 in order to allow 
bypassing of the default Logtalk grammar rule translator by defining 
clauses for the term_expansion/2 predicate. Added a "term_expansionp" 
protocol to the Logtalk library. Added a new example, "expansion", 
illustrating the use of the term_expansion/2 predicate.

Added a new compiler flag, hook/1, allowing the specification of a 
compiler hook that is called for which term read from a source file.
Added a simple example, "hooks", of using the Logtalk compiler hook 
to expand author abbreviations in info/1 directives into full names 
and email addresses.

Added support for XSB 3.x to the runtime error handler. Updated the XSB 
3.x config file with declarations for multi-threading meta-predicates.

Removed a few choice-points when checking validity of entity directives.

Added two new objects, list(Type) and set(Type), to the standard library 
supporting the validation of lists and ordered sets whose elements are 
restricted to a single type.

Added a new DCG example for solving enigmas encoded using a cellphone 
keypad.

Added a missing library dependency to the "puzzles" example SCRIPT file.

Removed the experimental "systemp.lgt" protocol from the list of files 
loaded by the "library/all_loader.lgt" loader utility files to avoid 
compilation errors on some Prolog systems.

Corrected a bug that prevented dynamic creation of categories using the 
built-in predicate create_category/4.

Corrected a bug in the reporting of singleton variables, which failed to 
write an accurate message for facts and grammar rules.

Corrected a bug in passing the correct calling context ("this") when 
processing meta-calls in objects.

Corrected a bug in scope checking with local calls to reflection and 
database methods.

Corrected a bug in checking the validity of the arguments of the op/3 
directive appearing inside entities.

Added predicates for testing if a term is a partial list or a proper list 
to all config files.

Added a definition for missing open/4 ISO Prolog predicate to the config 
file of Bin Prolog.

Added a workaround for a B-Prolog bug to this compiler config file.


2.26.2 - December 20, 2005
==========================

Improved error-checking for the Logtalk compiler and for the grammar rule 
translator.

Small performance improvements for message sending and for the built-in 
database methods.

Corrected a bug on the implementation of the built-in methods phrase/2-3 
for negated grammar rule bodies (thanks to Mats Carlsson for pointing the 
error).

Removed the read-only compiler flag "iso_initialization_dir".

Corrected a compilation bug on the "metapredicates" example source file.
Corrected several bugs on the performance monitor of the "searching" 
example.

Switched off default generation of XML documenting files for the "symdiff" 
example in order to avoid file names compatibility problems on Windows.

Updated compatibility notes on B-Prolog 6.8 and Qu-Prolog 7.0. Added a 
config file for the forthcoming SICStus Prolog 4.0 version (based only 
on publicly available information). Updated the config file for Amzi! 
Prolog to solve issues with predicate properties and to ensure that file 
system utility predicates are loaded.

Added a config file for the current XSB CVS version and the corresponding 
integration shell scripts (for both Windows and POSIX systems). Modified 
the "cplgtdirs.*" shell scripts in order to make either links or copies of
the config files and the "libpaths.pl" file with the required ".P" file 
name extension.

Improved integration scripts documentation and help screens.

Added a predicate for checking directory existence to the config files.


2.26.1 - November 28, 2005
==========================

Added a CSS style-sheet to the Logtalk XHTML documentation that can be 
used with CSSToXSLFO to generate nicely formatted PDF files suitable for 
printing with running headers and page numbers.

Updated the Logtalk XHTML documentation for compliance with the XHTML 1.1 
standard.

Updated the "lgtxhtml.xsl" XSLT file in order to generate XHTML 1.1 files.

Added a new, generic "lgtpdf.xsl" XSLT file. Redefined the "lgtpdfa4.xsl" 
and "lgtpdfus.xsl" files to import the "lgtpdf.xsl" file.

Added support for the Lunasil XSL-FO processor to the "lgt2pdf.*" shell 
scripts.

Updated the "lgt2pdf.sh", "lgt2xml.sh", and "lgt2html.sh" shell scripts 
in order to write a warning message when the current directory does not 
contain any XML documenting file.

Added a definition for missing open/4 ISO Prolog predicate to the config 
files of Open Prolog, LPA Prolog compilers, and MasterProlog.


2.26.0 - November 7, 2005
=========================

Added support for defining predicate aliases when using uses/2 directives
(using the format Predicate::Alias).

Added Prolog modules migration code allowing modules to be compiled as 
objects and allowing messages to be sent to modules. Added a new "modules"
example.

Added a "Prolog Integration and Migration Guide" to the Logtalk 
documentation.

Added support for syntax coloring of common Prolog module directives to 
the configuration files of the supported text editors.

Added support for using library aliases on the second argument of the 
logtalk_library_path/2 predicate (using the format alias(path)).

Added support for ignoring, copying as-is, or rewriting proprietary Prolog
directives. The action to be taken is defined on a per-directive basis on 
the config files.

Updated the config files of CIAO, ECLiPSe, SWI-Prolog, and YAP to define 
actions for some proprietary directives in order to allow some or most of 
the module libraries distributed with these compilers to be compiled as 
objects.

Renamed some XML documentation-related compiler flags to more meaningful 
names: "xml" -> "xmldocs", "xsl" -> "xslfile", and "doctype" -> "xmlsref".
No changes to the other flags or flag valid values.

Updated documenting scripts to use system-wide resources from LOGTALKHOME
and user-modifiable resources from LOGTALKUSER.

Updated "cplgtdirs.*" shell scripts to create links to the documenting 
scripts and to the DTD and XML Schema specifications instead of making 
local copies. Updated the "cplgtdirs.js" Windows JScript script to create
a link to the Logtalk manuals.

Changed generation of predicate compiled functors to add an underscore 
between the original predicate functor and arity (further minimizing the 
chance of name conflicts).

Improved compilation warning messages (separately handling plural and 
singular forms).

Improved documentation of Windows JScript installation and integration 
scripts.

Corrected a bug on the Logtalk built-in debugger on the processing of the
abort option at a leashed port (option was ignored instead of aborting the
debugging session).

Added an option to choose between a "verbose" or "compact" listing of the 
default compilation flags at Logtalk startup (defined on the config files).

Improved CIAO integration scripts in order to suppress some (harmless) 
warnings which are generated at first run.

Updated SWI-Prolog hook file ("swihook.pl") in order to support Logtalk 
library notation when loading Logtalk files using the SWI-Prolog consult/1 
predicate.

Corrected a bug on the implementation of the built-in method abolish/1 
that prevented abolishing of dynamic predicates with arity zero.

Corrected a bug on the "gplgt" shell script generated by the make_gplgt.sh
shell script where the value of the LOGTALKHOME environment variable would
not be retrieved at runtime.

Removed config file for older versions of XSB (up to 2.6). Renamed the 
config file for XSB 2.7.1 to simply "xsb.config".

Consolidated the source files of the "birds", "errors", "lpa", "lo", 
"metainterpreters", "poem", "relations", "roots", and "symdiff" examples 
into single source files. Updated the "searching" example to take advantage
of the uses/2 directive.

Removed outdated documenting shell scripts from the "xml" directory.

Corrected a bug when compiling a source file where the generated Prolog 
file might not be properly closed when a compilation error occurs.


2.25.3 - September 12, 2005
===========================

Consolidated the source files of the examples "bricks", "dynpred", "mi", 
"parametric", "points", "polygons", "reflection", "shapes", "sicstus", 
and "viewpoints" into single source files.

Improved documentation on most example source files. Improved "parametric"
example by adding a new parametric objects and by illustrating alternative
ways of accessing object parameters.

Updated several config files with declarations for some more non-standard 
Prolog meta-predicates.

Corrected some omissions and typos on the B-Prolog - Logtalk integration 
instructions.

Small performance optimization for messages and super-calls with not yet
cached method lookups.


2.25.2 - August 11, 2005
========================

Updated Logtalk installation and Prolog integration scripts in order to 
detect a wrongly defined LOGTALKHOME environment variable.

Corrected a bug on the lgt_install.sh and make*lgt.sh shell scripts where 
the wrong variable was being referenced when testing the existence of the 
installation directory prefix.

Corrected a bug on the makeall_lgt.sh shell script that failed to properly
detect unsuccessful completion of individual Prolog integration scripts. 


2.25.1 - August 8, 2005
=======================

Added support for using the "source-highlight" package (version 2.0 or 
later) by Lorenzo Bettini with Logtalk source files.

Several optimizations to the syntax coloring configuration files of most 
of the supported text editors. Added support for code folding to the Vim, 
Kate, and TextMate text editors.

Updated the SWI-Prolog shell integration script (make_swilgt.sh) to use 
either "swipl" or "pl" as the name of the Prolog executable, depending 
on the host operating system.

Modified the way entity prefixes are generated for compiled code in order 
to further minimize possible conflicts resulting from the same prefix being
used for different entities (bug report and fix by Brian Hulley). Changes 
to the prefix format are also reflected on the names of the automatically 
generated XML documenting files.

Updated the lgt2html.* and lgt2xml.* documenting scripts in order to index 
parametric objects using the notation <entity>/<number of parameters>.

Corrected a bug on the lgtxml.xsl XSLT file where links to related files 
pointed to .html files instead of .xml files.

Updated the lgt_install.sh and the make_*lgt.sh shell scripts to check and
report invalid installation directory prefixes.

Added a new state-space search problem, "salt3.lgt", contributed by Paula 
Marisa Sampaio, to the "searching" example.


2.25.0 - May 23, 2005
=====================

Logtalk compiler is now source file-based instead of entity-based. Thus, 
a source file may now contain any number of entities, without the need of 
using source metafiles. Therefore, this version drops support for source 
metafiles and the .mlgt file name extension.

The experimental encoding/1 directive, when used, must be the first term 
on a source file.  Added an entry to the reference manual describing the 
directive. Improved "encodings" example.

Added a new method, debugging/1, to the "debugger" pseudo-object for 
querying the system about entities compiled in debug mode.

Improved source file and entity compilation and loading reporting. In 
particular, when using library notation for source files, the Logtalk 
compiler now prints the library path containing the source files being 
compiled or loaded.

Added new shell scripts, makeall_lgt.*, which run all the make_*lgt.* 
shell scripts in sequence.

Simplified compiler reporting of singleton variables in directives and 
clauses.

Added an adaption of the "timetables" LPA Prolog++ example.

Updated B-Prolog config file for the new 6.7 #2 version. Dropped support 
for older versions of B-Prolog.


2.24.0 - April 22, 2005
=======================

Added experimental support for a encoding/1 directive for declaring the 
text character encoding of a source file. This directive is fully based 
on a directive with the same name and with similar semantics found on 
recent development versions of SWI-Prolog. For now, this directive only 
works with SWI-Prolog as most Prolog compilers lack support for dealing 
with different text encodings. Added new flag "supports_encoding_dir". 
Added a new example, "encodings", of using the new encoding/1 directive.

When a source file contains an encoding/1 directive, the XML documenting 
files will use the same encoding. When no encoding/1 directive is present,
the XML documenting files will assume UTF-8 encoding.

Added new info/1 documenting directive key, "parameters", allowing the 
declaration of both parameter names and parameter descriptions. Added new 
info/1 documenting directive key, "remarks", allowing general remarks 
about an entity to be stated. Added new info/2 documenting directive key, 
"arguments", allowing declaration of both predicate argument names and 
predicate argument descriptions. Added new info/2 documenting directive
key, "examples", allowing the representation of predicate call examples.

Much improved and accurate reporting of non-portable predicate calls when 
using the "portability" flag.

Added a new directory to the Logtalk distribution, "contributions", which 
will be used for user-contributed code. On this release, it contains an 
implementation of the ISO 8601 standard by Daniel L. Dudley, providing a 
library of useful date predicates.

Added new lgt2xml.* shell scripts for generating (X)HTML indexes of XML 
documenting files.

Rewritten the lgtxhtml.xsl, lgthtml.xsl, lgtxml.xsl XSLT scripts in order 
to make it easier to define alternative CSS files for the generated HTML 
files. Rewritten the lgtpdfa4.xsl and lgtpdfus.xsl XSLT scripts in order 
to improve appearance of the generated PDF files.

Improved the documentation of the "benchmarks" example and added new 
predicates for running batches of benchmark tests and for generating 
lists of known size for the benchmark tests of static code.

Corrected a bug in the lgt2html.*, lgt2pdf.*, and cplgtdirs.sh shell 
scripts which resulted in failed transformations whenever the LOGTALKHOME 
and LOGTALKUSER environment variables or the output directory contained 
spaces or accented characters.

Added workaround for Prolog compilers that define operators other than 
','/2 that cannot be redefined (such as new B-Prolog 6.7).

Added a Logtalk "clip" file for the TextPad Windows text editor.

Renamed directory "misc" to the more meaningful name "scripts".

Corrected a bug in the implementation of the built-in database methods 
asserta/1 and assertz/1 when asserting facts on objects compiled in debug 
mode.

Corrected a bug in the method leash/1 of the built-in pseudo-object 
"debugger", which failed to accept an empty list as a valid argument.

Corrected a bug in the header of the automatically generated XML 
documenting files when using the XML Schema specification (logtalk.xsd), 
which could prevented validation when the compiler flag "doctype" is set 
to "web".

Corrected a compilation bug where Prolog clauses written outside entities 
on source metafiles would have their order reversed. Simplified splitting 
of source metafiles.

Corrected a compilation bug where Prolog clauses written before an entity 
opening directive would not be copied to the generated Prolog file.

Corrected a bug on the "roots" example in the object "class" which failed 
to properly test the validity of new object identifiers.

Corrected a bug in the syntax coloring file for the Vim text editor, which
resulted in an error message when opening a Logtalk source file for the 
first time.


2.23.1 - March 7, 2005
======================

Simplified message sending compilation, improving performance of both 
entity compilation and runtime top-level message sending.

Simplified implementation and improved performance of the built-in method 
current_predicate/2.

Updated the runtime error handler for top-level ::/2 calls to recognize 
non-existing predicate exceptions thrown by XSB, SICStus Prolog, CIAO, 
B-Prolog, and recent development versions of SWI-Prolog. Rewritten the 
::/2 predicate in order to minimize the overhead of the catch/3 calls 
associated with the runtime error handler, thus improving performance.

Expanded the benchmarks example to test performance of the built-in 
database methods.

Lookup caches are now cleaned before loading a redefined entity in order 
to avoid potential problems with entity initialization goals containing 
message sending calls (previous versions cleaned the caches only after 
entity loading).

When reloading an object, its tables of dynamic predicate declarations 
and dynamic predicate definitions are now reseted.

Corrected a compatibility problem with the "birds" example due to the use 
of an operator not available in some Prolog compilers.


2.23.0 - February 21, 2005
==========================

Optimized the code generated for local calls to the built-in predicates 
asserta/1, assertz/1, retract/1, and retractall/1, when the argument is 
a dynamic predicate declared by a scope directive in the object making 
the calls.

Added caching of predicate compiled forms for dynamic facts when used 
with the database built-in methods clause/2, asserta/1, assertz/1, 
retract/1, and retractall/1, resulting in a significant performance 
improvement when using an object's dynamic database. Improved performance 
of the database built-in methods abolish/1, asserta/1, assertz/1, clause/2, 
retract/1, and retractall/1 when the cache is not used.

Corrected a bug on the implementation of the built-in methods asserta/1 
and assertz/1 that prevented asserting of rules.

Corrected a bug on the implementation of built-in methods retractall/1, 
retract/1, and clause/2 that allowed access to local dynamic predicates 
from outside the container object.

Added a runtime error handler for top-level ::/2 calls which tries to deal
with exceptions thrown by calling non-existing predicates by translating 
Logtalk generated internal predicate names to user names.

Print the total number of warnings after a call to the Logtalk built-in 
predicates logtalk_compile/1-2 and logtalk_load/1-2.


2.22.5 - February 9, 2005
=========================

Added scripts for easy integration of Logtalk with K-Prolog. Updated the 
K-Prolog config file, correcting a show-stopper bug and enabling support 
for using the "library" notation for loading source files.

Updated JIProlog config file in order to allow smart compilation of source
files.

Changed format of preprocessor and runtime dynamic predicates that deal 
with predicate indicators for better performance.

Simplified implementation of Logtalk built-in methods asserta/1 and 
assertz/1.

Corrected a performance bug with calls to built-in predicates from objects
and categories.

Corrected spurious backtracking occurring on some calls to the built-in 
predicate define_events/5.

Updated shell script "misc/cplgtdirs.sh" to prevent copy of Logtalk files 
when the destination directory already exists (thus avoiding overriding 
any user-modified files when upgrading Logtalk).

Added syntax coloring for the predicate logtalk_library_path/2 to the 
supported text editors. Updated the syntax coloring file for the TextMate 
text editor, adding some missing ISO Prolog predicates.

Improved printing of lists of unknown referenced entities, misspelt calls,
and singleton variables when compiling source files. Simplified handling 
of references to unknown entities.

Added workaround for Prolog compilers with broken read_term/3 singletons/1
option.

Updated the Logtalk compiler/runtime and the documentation to always 
use the expression "compiler flag" instead of "compiler option" for 
consistency. Other minor documentation improvements.

Corrected a bug with the abolishing of dynamic entities where the clauses 
corresponding to the alias/3 directive are not being abolished.

Added new predicates '$lgt_call'/9 and '$lgt_once'/9 to all config files.

Simplified and improved performance of compilation of directives containing
predicate indicators.


2.22.4 - January 12, 2005
=========================

Simplified method lookup cache tables, resulting in a small message 
sending performance speedup. Improved method lookup caching for 
parametric objects. Added support for caching method lookups for 
parametric objects whose parameters contain cyclic terms.

Added a new category, "assignvars", to the Logtalk standard library, 
containing an adaptation of the implementation of logical assignable 
variables developed by Nobukuni Kino. Added a new example, "assignvars",
illustrating some possible uses of assignable variables in the context 
of parametric objects.

Simplified compilation of op/3 directives: there is no longer need to 
repeat the directives both before and inside an entity in order to make 
the operators global and also use them in the compilation process.

Simplified installation instructions.

Corrected a compiler bug in the code that checks and prints a warning 
when redefined parametric objects.

Corrected a bug in the built-in predicate abolish_category/1, which 
failed to delete import relation clauses between the category being 
abolished and other categories.


2.22.3 - December 30, 2004
==========================

Added a configuration file for XSB 2.7.

Corrected a bug where the use of alternative compilation directories 
would fail the first time the logtalk_compile/1-2 or logtalk_load/1-2 
predicates are used for a given file or library.

Corrected a bug in the built-in methods asserta/1 and assertz/1 when 
asserting facts into an object being debugged, which resulted in wrong 
execution context information being printed when tracing calls to 
the asserted facts.

Corrected a bug in the built-in methods asserta/1 and assertz/1 when 
asserting rules into an object being debugged, which resulted in rule 
bodies compiled with no information being printed for the rule head 
when tracing calls to the asserted rules.

Corrected a bug in the dynamic creation of objects, protocols, and 
categories when the debug flag is on, which resulted in the table of 
entities being debugged not being updated.

Corrected a bug in the handling of exceptions thrown when using the 
built-in debugger, which resulted in exceptions always being turned 
into failures.


2.22.2 - December 24, 2004
==========================

Improved performance of dynamic creation and compilation of objects, 
protocols, and categories.

Improved error-checking code of methods asserta/1 and assertz/1 to 
prevent asserting of non-callable terms as facts.

Improved error checking for documenting directives (info/1 and info/2).

Improved the XSB integration script (make_xsblgt.sh) in order to use the 
configuration file and the libpaths file available from the $LOGTALKUSER 
directory.

Improved installation instructions and installation scripts documentation.

Added documentation to the user manual on describing predicate exceptions 
using the info/2 directive.

Corrected a bug in the predicate_property/2 built-in method when called 
with the second argument instantiated to the alias/1 property.


2.22.1 - December 6, 2004
=========================

Improved installation instructions. Updated the Windows installation 
JScript script to add the Logtalk directories "misc" and "xml" to the 
system PATH environment variable and to reuse the environment variable 
LOGTALKUSER if already defined.

Added helper batch scripts (misc/cplgtdirs.bat, xml/lgt2html.bat, and 
xml/lgt2pdf.bat) for easily running the corresponding Windows JScript 
scripts from the command-line (the two xml/lgt2*.bat scripts run the 
corresponding *.js scripts stored in the %LOGTALKUSER%/xml directory 
in order to support user customization).

Updated the lgt2html.* and lgt2pdf.* shell scripts to use the environment 
variable LOGTALKUSER instead of LOGTALKHOME for finding supporting files, 
thus allowing for end-user customization of the scripts and their related 
files.

Added documentation on read-only compiler flags (which are defined in the 
configuration files) to the User Manual.

Updated the misc/lgt_install.js JScript installer script to workaround a 
Windows Scripting Host bug which may result in broken shortcuts in the 
created "Logtalk" program group.


2.22.0 - November 29, 2004
==========================

Implemented a notion of library as simply a directory containing source 
files. Added a new dynamic predicate, logtalk_library_path/2, for 
specifying library paths. Added a sample file (libpaths/libpaths.pl) for 
setting library paths for the Logtalk standard library and for all the 
supplied examples.

Added support for using the notation <library>(<entity>) with the built-in
predicates logtalk_compile/1-2 and logtalk_load/1-2 for compiling and 
loading source files contained on a library, without the need to first 
change the current working directory.

Changed the library and the examples loading instructions to use the new 
notation <library>(<entity>).

Improved installation instructions and installation scripts. Added a new 
environment variable, LOGTALKUSER, for defining the location of the copies 
of the user-modifiable Logtalk files in a multiple users setup environment.

Added utility predicates to most configuration files for querying and 
changing the current working directory (needed for the <library>(<entity>) 
notation to work).

Corrected a bug in the code that checks for file existence when compiling 
or loading source files and source metafiles.


2.21.6 - November 15, 2004
==========================

Added support for using alternative directories for storing the Prolog files 
and the XML documenting files resulting from entity compilation. A new read-
only flag, "altdirs", allows the support for alternative directories to be 
turned on the configuration files of Prolog compilers providing the necessary 
operating-system access predicates for implementing this feature.

Improved installation instructions and reporting of default compilation flags 
at startup.

Added a sorely missing "Hello World!" example :-)


2.21.5 - November 2, 2004
=========================

Added a new optimization compiler flag, "events", which can be used to switch 
off event-driven programming support when not needed, on a per-object basis, 
resulting in improved message sending performance.

Small performance optimization to the runtime creation of dynamic objects, 
protocols, and categories.

Updated the error handling code of the built-in predicates create_object/4, 
create_protocol/3, and create_category/4 in order to throw an instantiation 
error when called with a non-instantiated argument.

Updated the benchmarks example to calculate and subtract the benchmark loop 
time in order to print absolute timings for the benchmarked goals.

Updated the Qu-Prolog patching instructions to also convert Logtalk source 
metafiles.

Fixed a typo on the XSB compatibility notes in the configs/NOTES file.


2.21.4 - October 26, 2004
=========================

Corrected a silly bug in the definition of the predicate repeat/1 on the 
new "benchmarks" example.


2.21.3 - October 25, 2004
=========================

Corrected a silly bug in message translation which resulted in a small 
performance loss. Simplified implementation of message sending predicate 
::/2, resulting in a small performance improvement.

Added a new section on message sending performance to the user manual.

Added a new example, "benchmarks", for helping measuring performance of 
Logtalk message sending between Prolog compilers and for comparing message 
sending with predicate calls in plain Prolog and with calls to modules 
predicates.

Updated the YAP configuration file to hide some of the book-keeping tables 
of the Logtalk runtime engine and to use statistics/2 instead of cputime/0 
for more accurate timings.

Updated the Logtalk shell installation script to create the $prefix/bin 
directory if it does not exist.

The lgt2pdf.sh and lgt2html.sh shell scripts now use "rm -f" when removing 
the DTD and XSD files in order to avoid prompting the the user about the file 
permissions.

Updated the cplgtdirs.sh shell script to make all copied files user writable.


2.21.2 - October 18, 2004
=========================

Added support for declaring grammar rule non-terminals as public, protected, 
private, dynamic, or discontiguous using the notation Functor//Arity. Added 
support for documenting non-terminals using the info/2 and mode/2 directives.

Added support for a new alias/1 predicate property.

New experimental config file for JIProlog 3.0 (see the configs/NOTES file 
for details).

Added a mode bundle for the TextMode 1.0.x (MacOS X) text editor providing 
syntax highlighting, folding, and code snippets for editing Logtalk source 
files. Corrected a bug in the syntax coloring of the :-/1-2 operator on the 
SubEthaEdit 2.x text editor.


2.21.1 - September 27, 2004
===========================

Added experimental support for a new built-in method, expand_term/2, which 
allows access to the Logtalk grammar rule translator. Corrected bug in the 
expansion of \+/1 in grammar rule bodies. Optimized code generated for 
message sending calls in the body of grammar rules. Improved error handling 
of the built-in methods phrase/2 and phrase/3. Added new section on definite 
clause grammars to the user manual. Added two new objects to the "dcgs" 
example, dcgtest and bypass, containing test cases for the Logtalk DCG 
translator and illustrating some programming techniques with grammar rules.

Corrected a bug in the implementation of the built-in method clause/2 which 
prevented access to clauses of local dynamic predicates.

Corrected some dead links on the XHTML manuals.

Corrected a bug in the syntax coloring of numbers on the SubEthaEdit 2.x 
text editor.


2.21.0 - September 14, 2004
===========================

Added a new predicate directive, alias/3, which allows the definition of 
alternative predicate names in order to improve readability of inherited 
features and to solve conflicts between implemented, imported, or inherited 
predicates.

Added new example, "aliases", illustrating the use of the new alias/3 
predicate directive for improving readability of inherited features.

Added new example, "diamonds", illustrating problems and solutions for the 
"diamond problem" (multi-inheritance conflicts and ambiguities) using the 
new alias/3 predicate directive.

Allow categories to import other categories, i.e. allow categories to be 
defined as a composition of other categories. This feature should only be 
used when extending a category without breaking its functional cohesion 
(for example, when a modified version of a category is needed for importing 
into several unrelated objects). 

Added new example, "engines", illustrating importing a category from another 
category.

Updated the syntax coloring configuration files for the supported text 
editors for the new alias/3 predicate directive.

Added auto-complete strings for Logtalk methods and for Logtalk and Prolog 
built-in predicates and directives to the SubEthaEdit 2.x text editor.

Corrected a bug which prevented compilation of metafiles containing 
parametric entities.


2.20.2 - August 31, 2004
========================

Added a Windows JScript script for installing Logtalk. Improved the Windows 
JScript scripts used for easy integration of Logtalk with selected Prolog 
compilers.

Improved user manual section on defining object and category metapredicates 
and on calling non-standard Prolog built-in metapredicates. Improved the 
description of some compiler options.

Added some files missing from the "errors" example (which should have been 
included in the previous release).

Added basic syntax coloring support for the BBEdit 8.x text editor.


2.20.1 - August 19, 2004
========================

Added Windows JScript scripts for easy integration of Logtalk with CIAO and 
GNU Prolog.

Added encoding attribute (set to UTF-8) to the xsl:output tag in the 
lgtxml.xsl, lgthtml.xsl, and lgtxhtml.xsl files.

Replaced the Windows JScript scripts lgt2pdf.js and lgt2html.js by their 
final versions (the wrong ones shipped with Logtalk version 2.20.0).

Updated the "errors" example to illustrate possible conflict errors when 
using the uses/2 directive.

Updated the RPM logtalk.spec file to use the .tgz archive extension.


2.20.0 - August 16, 2004
========================

Added support for the uses/2 predicate directive (whose semantics is similar 
to C++ using-declarations). Updated the uses/1 entity directive to accept as 
argument a single object identifier.

Improved installation instructions for Windows users.

Added four new sample bash shell scripts and Windows JScript scripts for 
converting XML documenting files to PDF, HTML, and XHTML using several XSL 
processors.

Added missing namespace to XSL files in order to generated valid (X)HTML 
files with recent versions of XSLT processors.

Updated the User Manual documentation on converting XML documenting files 
to other formats.

Removed the texml.xsl XSLT file as the TeXMLatte application it depends on 
is no longer available.

Added Windows JScript script for copying the Logtalk examples, library, and 
xml directories to the user directory.

Added Windows JScript scripts for easy integration of Logtalk with ECLiPSe, 
SWI-Prolog, SICStus Prolog, and YAP.

Added missing extension for source metafiles to the SWI-Prolog hook file.

Corrected a bug in the lgtxhtml.xsl XSLT file where a wrong reference to 
the Logtalk CSS file is being used in the xml-stylesheet tag.

The iso_initialization_dir/1 compiler option is now a read only flag, 
defined in the configuration files.


2.19.1 - August 2, 2004
=======================

Corrected a bug where entities could not be reloaded if they depend on 
other, not yet loaded entities.

Corrected a bug where compiler options would not be used when compiling 
source metafiles.

Corrected several typos on the Logtalk manuals.


2.19.0 - July 26, 2004
======================

Added support for defining more than one entity per file using Logtalk 
source metafiles (*.mlgt), which the runtime engine automatically splits 
on single entity source files plus loading and compiling helper files.

Updated the URL used on the automatically generated XML documenting files 
when compiling entities with the option doctype(web).

Improved error checking when compiling calls to the {}/1 control construct.

Corrected several typos and some outdated links on the Logtalk manuals.


2.18.0 - July 9, 2004
=====================

Added caching of method lookups in order to improve message processing 
performance (including messages to self and super calls). Applied several 
optimizations to runtime translation of messages in order to further 
improve performance.

Improved error checking while parsing and compiling messages to self.

Update ECLiPSe config files to compile the Prolog files generated by Logtalk 
(when compiling source files) in optimized mode instead of traceable mode.

Updated description of patches needed for running Logtalk with XSB, added 
a shell script for easy integration of Logtalk with this Prolog compiler, 
and corrected a problem with its corresponding config file.

Added multifile/1 directive to the syntax coloring configuration files of the 
supported text editors.


2.17.2 - June 14, 2004
======================

Updated the Logtalk compiler to allow compilation of source files which 
contain only directives and no entity definition.

Simplified loading of library and example entities by using Logtalk source 
files as loader files.

Some improvements to the documentation, including the QUICK_START file and 
the User Manual for first-time users, and to the example descriptions.

Corrected a bug in handling of local object operators that are also global 
operators.

Corrected a bug where dynamic directives are not being generated for dynamic 
predicates that lack a scope directive or an initial set of clauses.

Corrected a bug where local dynamic predicates would need a private scope 
directive in order to allow clauses of the predicate to be retracted.

Simplified compilation of dynamic and discontiguous predicate directives.

Added new "logic" example implementing a translator from logic propositions 
to conjunctive normal form.

Corrected a problem with wrong end-on-lines on some files.

Added a shell script for easy integration of Logtalk with CIAO Prolog.


2.17.1 - June 7, 2004
=====================

Added custom handling of specification of predicate exceptions in info/2 
documenting directives when writing XML documenting files (declared as 
"exceptions is [Cond1-Term1, Cond2-Term2, ...]"; each pair (Condi, Termi) 
represents an error condition and its respective exception). 

Changed debugger command "e" to print exception terms instead of exiting 
Logtalk session.

Corrected a compilation error with B-Prolog due to operator priorities.

Corrected a possible problem with the redefinition of loaded entities that 
define (possibly at runtime) dynamic predicates. All clauses for an entity 
dynamic predicates are retracted before loading the new entity definition.

Corrected a potential bug when pretty printing terms with variables in the 
debugger and in the created XML documenting files.

Added four more DCGs examples ("walker movements", "iterative shell command 
parsing", "bill of materials", and "command language").

New "puzzles" and "metainterpreters" examples.

Added a mode bundle for the SubEthaEdit 2.x (MacOS X) text editor providing 
syntax coloring for editing Logtalk source files. Optimized some of the 
regular expressions in the syntax coloring files for the Kate/Kwrite and Vim 
text editors.

Removed predicate nth/3 from library entities listp, list, and difflist. 
Replaced by the predicates nth0/3 and nth1/3. Added new predicates nth1/4 
and nth0/4.

Updated the config file for Quintus Prolog to make use of the "files" 
library.

Added experimental implementations for some Prolog compilers of the library 
portable protocol for operating system access (systemp.lgt). See the file 
library/experimental/NOTES for details.


2.17.0 - April 26, 2004
=======================

Added built-in debugging support implemented through the definition of a 
new pseudo-object named "debugger". Added "debug" compiler option. Renamed 
library entities "debuggerp" and "debugger" to, respectively, "event_dbgp" 
and "event_dbg".

Improved installation instructions for multi-user environments. Added 
a shell script for installing Logtalk in Unix and Unix-like operating 
systems. Added a shell script for copying Logtalk xml, examples, and 
library directories to the user home directory.

Added a logtalk.spec file for building Linux RPMs from sources.

Added a set of shell scripts for easy integration of Logtalk with ECLiPSe, 
GNU Prolog, Qu-Prolog, SICStus Prolog, SWI-Prolog, and YAP.

Corrected bug in the definition of the pseudo-object "user" that resulted 
in mixed up metapredicate and type predicate properties.

Removed config files for outdated versions of Qu-Prolog, SWI-Prolog, ECLiPSe,
K-Prolog, CIAO, YAP, Amzi! Prolog, LPA Win-Prolog, SICStus Prolog, and XSB.


2.16.2 - April 2, 2004
======================

Corrected a bug in the library category monitor (file library/monitor.lgt) 
that prevents its compilation.

Changed the possible values of the read-only flag startup_message (defined 
in the config files) to "flags" (print banner and flag values), "banner" 
(print only the banner), and "none" (do not print neither the banner nor the 
flag values). Default value is "flags".

Updated the "errors" example to illustrate the error messages thrown when 
trying to redefine Logtalk built-in control constructs.

Corrected a small problem with the Logtalk language specification file for 
Apple's Xcode IDE.

Added preliminary support for syntax coloring using the Windows text editor 
Crimson Editor.


2.16.1 - March 23, 2004
=======================

The local built-in method parameter/2 can now be used inside categories 
(but please read the warnings about such practice on the user manual).

Updated the Logtalk compiler to report an error when the user tries to 
redefine a message sending or external call control construct inside an 
object or category.

Corrected a bug in the compilation of metacalls whose meta-arguments are 
variables.

Removed references to file sax.jar from the scripts used to convert XML 
documenting files into (X)HTML. Updated scripts html.sh, htmlnt.cmd, and 
htmldos.bat to use XT 20020426a or later version.

Improved syntax coloring accuracy of built-in predicates and methods for 
the SubEthaEdit text editor.

Updated config file for Qu-Prolog 6.4.


2.16.0 - March 3, 2004
======================

Logtalk is now distributed under the Artistic License 2.0.

Operators declared inside an entity are now local to the entity, not 
affecting the global operator table (complying with the Logtalk language 
specification). Input and output of terms from inside objects and 
categories now work as expected in the presence of local operator 
declarations. Added a new example named "operators".

Updated built-in method predicate_property/2 for returning metapredicate/1 
properties for both Logtalk and Prolog metapredicates.

Added support for calls to non-ISO Prolog standard built-in metapredicates 
inside objects and categories. Added support for the declaration of non-ISO 
Prolog standard metapredicates in the config files.

Small change to the way the Logtalk compiler reports the compilation of
entities. Small optimizations to the compilation of Logtalk source files.

Corrected a bug where calls to Logtalk built-in predicates would be 
reported as non-portable when the value of compiler option portability is 
set to warning.

New config file for Qu-Prolog 6.4 (see the configs/NOTES file for details).

Corrected a problem with the syntax coloring of quoted atoms and strings 
in the SubEthaEdit text editor. Added more accurate syntax highlight for
built-in methods and built-in predicates.

Updated the syntax coloring configuration files for all supported text 
editors to recognize the file extension used by the config files.


2.15.6 - February 9, 2004
=========================

Added "xmlspec" compiler option in order to specify the extension (dtd 
or xsd) of the file describing the XML documenting files specification.

Renamed compiler option "named_anonymous_vars" to the more appropriated 
name "underscore_vars". Changed possible option values to "dont_care" and 
"singletons" (default).

Added XSLT file for converting XML documenting files to XHTML 1.0 Strict 
files. Set the default encoding of all XSLT files to UTF-8.

Added syntax coloring support for the KDE Kate and Kwrite text editors.

Improved syntax coloring configuration files for VIM, jEdit, NEdit, 
SubEthaEdit, and Emacs text editors.

Removed outdated support for MacOS X Project Builder and added support 
for the new MacOS X Xcode developer tool.

Corrected bug in the built-in predicate current_logtalk_flag/2 that 
prevented some flag values from being returned after using the built-in 
predicate set_logtalk_flag/2.

Corrected bug in the shapes example (wrong placement of the declaration 
of the predicate side/1).


2.15.5 - December 30, 2003
==========================

Make operator ^^/1 right-associative for consistency with remaining 
operator declarations.

Added file BIBLIOGRAPHY containing a list of Logtalk publications in 
BibTeX format.

Added a font-lock file for Emacs providing syntax coloring for editing 
Logtalk source files.

Added an implementation of the Smalltalk dependent mechanism to the 
standard Logtalk library.

Updated the config file for ECLiPSe 5.4~5.7 with missing ISO Prolog 
predicate definitions needed for successful compilation of all entities 
in the Logtalk standard library.

Updated manual pages to comply with XHTML 1.0 Strict and to provide 
better navigation.


2.15.4 - July 9, 2003
=====================

Corrected a spurious backtracking bug in the DCG rule translator. Added 
two more examples of DCGs.

New config file for XSB 2.6. Updated Logtalk compiler to compile cleanly 
under XSB after applying the patch described in configs/NOTES.

Updated SWI-Prolog config file to hide compiled entity predicates (requires 
SWI-Prolog 5.2.3 or later version). New optional file configs/swihook.pl 
contains hook code that allows Logtalk entities to be compiled and loaded 
using SWI-Prolog load_files/2 and consult/1 predicates.

Syntax definition file for the Hydra 1.1 (MacOS X) text editor providing 
syntax coloring for editing Logtalk source files.

Updated syntax coloring files for jEdit, NEdit, Vim, and TextPad in order 
to recognize character code constants (0'x) and the DCG operator -->/2.


2.15.3 - June 27, 2003
======================

Updated experimental support for DCG rules to prevent over-simplification 
of unification goals in the compilation of rules to clauses. Push-back 
lists can now be used on rule heads.

The compilation mode of an entity (static/dynamic) is now stored in the 
corresponding entity table (implying recompilation of all entities).

Updated GNU Prolog config file to hide compiled entity predicates.

Updated SWI-Prolog config file for better integration of Logtalk with 
this compiler.


2.15.2 - April 2, 2003
======================

Experimental support for DCG rules inside categories and objects. Added 
built-in methods phrase/2 and phrase/3.

Updated GNU Prolog config file to not hide compiled entity predicates in 
order to avoid incompatibilities with dynamic entities.


2.15.1 - March 8, 2003
======================

New example, msglog, of using events and monitors to record, replay, and 
print user messages.

Corrected a typo on the jEdit templates that resulted in syntax errors.
Corrected wrong year format in info/1 directive in all entity templates.
Added missing version info key to jEdit prototype template.

Corrected documentation of built-in local methods self/1 and sender/1.
Updated documentation of built-in local methods parameter/2 and this/1.


2.15.0 - February 5, 2003
=========================

Changed "authors" key in info/1 directive to "author".

Corrected documentation of built-in local method this/1.

New geometric shapes example implemented in two versions, one 
prototype-based and the other one class-based.

Improved support for jEdit text editor by adding a set of programming 
templates for use with the editor Templates plug-in.

Added basic support for syntax highlighting and programming templates 
for MacOS X Project builder.


2.14.7 - January 10, 2003
=========================

Corrected a bug in the built-in methods asserta/1 and assertz/1 when 
asserting predicate rules.

The built-in predicates logtalk_compile/1-2 and logtalk_load/1-2 now 
accept both an entity (an atom) or a list of entities (a list of atoms).

Optimized the code generated when compiling or asserting clauses for 
dynamic predicates.

Optimized protected inheritance performance on some Prolog compilers.


2.14.6 - December 31, 2002
==========================

Corrected a bug where the opening directive of an object that, 
simultaneously, implements a protocol, imports a category, instantiates 
a class, and specializes a superclass, will not be recognized.


2.14.5 - December 20, 2002
==========================

Simplified dynamic table of predicate declarations by removing two 
redundant arguments.

Corrected a bug where sending messages such as true/0 or !/0 to an 
unknown object succeeded instead of throwing the correct exception.

Simplified the code used to generate links in the lgtpdfa4.xsl and 
lgtpdfus.xsl XSLT files.


2.14.4 - November 5, 2002
=========================

Removed definition of deprecated built-in predicate logtalk_version/3.

Show flag names when printing at startup the default flag values.

Small change to messages printed on console with smart compilation of 
source files turned on.

Updated YAP 4.3.x config file to use the YAP "system" library to access 
the operating system for time, date, and file properties (enabling smart 
source code compilation).

Updated the lgtpdfa4.xsl and lgtpdfus.xsl XSLT files to workaround a bug 
in the PassiveTeX 1.21 XSL:FO processor.


2.14.3 - September 16, 2002
===========================

New compiler option, doctype, to set the doctype reference (if any) 
in the XML documenting files. Default value is "local" for backward 
compatibility. Updated the user manual section on compiler options.

The Logtalk built-in predicate logtalk_version/3 should be considered 
deprecated (use current_logtalk_flag/3 instead). All references to this 
predicate have been removed from the manuals.

Updated the jEdit syntax coloring config file for the new jEdit 4.1 
version.

Updated the lgtpdfa4.xsl and lgtpdfus.xsl XSLT files for compatibility 
with the XSL:FO processors Apache FOP 0.20.4, PassiveTeX, and RenderX 
and with the XSL W3C Recommendation of October 15, 2001. Updated the 
pdf.bat and pdf.sh scripts to reference the latest version of the 
Apache FOP processor (0.20.4).

Changed the shell scripts html.sh and pdf.sh for compatibility with the 
sh, bash, and zsh shells.


2.14.2 - August 26, 2002
========================

Calls to built-in method parameter/2 are now compiled inline, improving 
call performance.

Updated Logtalk compiler to clean temporary compilation predicates after 
compiling an entity instead of only before compilation.

Updated YAP 4.3.x config file for better Logtalk integration by hiding 
all internal compiler, runtime, and compiled entities static predicates.

Updated GNU Prolog config file for better Logtalk integration by hiding 
all internal compiler, runtime, and compiled entities predicates.

Updated the XSLT conversion scripts lgtxml.xsl and lgthtml.xsl to correct 
some HTML conformance errors in the generated pages.

Corrected some XHTML conformance errors in the manual pages.


2.14.1 - July 31, 2002
======================

New Windows NT script and updated Unix shell script for batch converting 
XML documenting files to HTML. Both scripts also generate an index.html 
file containing links to all converted XML documenting files.

Corrected wrong XHTML DOCTYPE declaration in manual pages.


2.14.0 - July 26, 2002
======================

Renamed all compiler and runtime internal predicates and all auxiliary 
predicates in the config files to start with a "$" character.

New compiler option, code_prefix, to set a prefix for all Prolog code 
functors generated by Logtalk when compiling entities. Default is ''.

New compiler option, named_anonymous_vars, that instructs the compiler to 
interpret variables that start with an underscore as anonymous variables 
(and to not report them as singleton variables). Default value is "off".

Directive info/2 was wrongly declared as a entity directive instead of a 
predicate directive.

Converted all manual pages to XHTML 1.0 format. Corrected a wrong link in 
the reference manual. Replaced GIF images by PNG versions.

Updated BProlog 6.x config file to workaround the new 6.2 built-in 
predicate ::/2 that conflicts with the same named Logtalk message sending 
operator.

Removed call to the obsolete built-in predicate nodbgcomp/0 from all 
ECLiPSe config files.


2.13.0 - June 15, 2002
======================

Logtalk now outputs Prolog code using write_canonical/2 instead of 
write_term/3. Goals are better portability of the generated Prolog files 
and avoid issues with clauses with long bodies for Prolog compilers that 
use a term print depth limit to prevent problems with cyclic terms.

Added report of default flag values at Logtalk startup.

Logtalk now prints a warning when redefining parametric objects.

Removed need of an abort/0 predicate from the Logtalk debugger example.
Removed any definition of this predicate from the config files.

Added missing definitions for some ISO built-in predicates to the Amzi! 
Prolog 6.2.2 config file.


2.12.0 - May 25, 2002
=====================

New read-only Logtalk flag "version". Corrected exception term generated 
when trying to modify a read-only flag using the set_logtalk_flag/2 
built-in predicate.

Updated config file for OpenProlog 1.1b5.

New config file for ECLiPSe 5.4.

Renamed config files for CIAO 1.7 (beta of 1.8) to "ciao_aux18.config" 
and "ciao18.config".

Updated config file for Amzi! Prolog 6.2.2.

New example of using some of the built-in database handling methods 
(dynpred).

Syntax configuration files for jEdit 4.0, VIM 6.1, NEdit 5.2, and 
TextPad 4.5 text editors providing syntax highlighting for editing 
Logtalk source files.


2.11.0 - April 22, 2002
=======================

Added a "smart compilation" feature to the Logtalk compiler, controlled 
by a "smart_compilation" flag. Only available in Prolog compilers that 
provide access to file modification dates.

Added a "startup_message" flag to control printing of the Logtalk banner 
at startup.

Reworked Logtalk pre-processor compilation and loading reports. Compiler 
option "report" now toggles between normal reporting (as specified by the 
other flags) and silent compilation/loading of source files.


2.10.0 - April 5, 2002
======================

Scope of object asserted predicates for which there is no declaration is 
now a function of the context instead of always being declared public. 
Asserting in this, the predicate is declared private, asserting in self, 
the predicate is declared protected, otherwise the predicate is declared 
public.

Throw an error if a category contains clauses for dynamic predicates.

Updated documentation on categories and built-in methods for database 
handling.

Retracting all clauses for a dynamic predicate from an object now allows 
an inherited definition to be called when sending the corresponding 
message. In previous versions the message just failed.

Added missing entries for the built-in predicates current_logtalk_flag/2 
and set_logtalk_flag/2 to the Logtalk compiler built-in predicates table.

Updated config file for Amzi! Prolog 6.1.74.

Updated notes on patching XSB to work with Logtalk.


2.9.3 - February 9, 2002
========================

New current_logtalk_flag/2 and set_logtalk_flag/2 built-in predicates.

Updated documentation on new built-in predicates.

Renamed SWI-Prolog config file swi330.config to swi.config.

Renamed config file predicate lgt_default_compiler_option/2 to lgt_flag/2.

New XSL style-sheets and shell scripts to convert Logtalk XML entity 
documenting files to PDF format using XSL Formating Objects.


2.9.2 - January 4, 2002
=======================

The Logtalk compiler can now print a warning when compiling source files 
that use non-ISO defined built-in predicates using a new portability/1 
compiler option. Updated the relevant sections of the user manual and the 
errors example.

Corrected a compiler bug where the entity relation tables only recorded 
the last entry per entity relation type.

Updated config file for CIAO 1.7#162.


2.9.1 - December 5, 2001
========================

Logtalk compiler now prints a warning when redefining or replacing an
existing entity.

Corrected a compiler bug in the error checking code for the info/1 
and info/2 directives.

Changed the order of object loading in the "mi" example to avoid 
spurious warning messages.

Added a new problem ("bridge") to the "searching" example. Improved 
"performance" monitor to give correct reports on alternative solutions. 
Corrected a bug in the "water jug" state space example.


2.9.0 - October 22, 2001
========================

Added config files for CIAO Prolog 1.7p115 and B-Prolog 6.0.

Compiling and loading an entity that contains references to unknown 
entities (by default) prints a warning. Updated user and reference 
manuals.

Rewritten all the compiler warning/error code and output messages.

Changed compiled code functors postfixes from "_sdcl" and "_sdef" to 
"_idcl"and "_idef" (implies recompilation of all objects, protocols, 
and categories).

Changed all occurrences and references to the term "entity_scope" to 
"scope".

Removed some redundant productions from the Logtalk grammar.

Updated documentation on the xml directory. Renamed some of the .xsl 
files. Added sample scripts to batch convert .xml files to .html files.

Added a new loader utility file, all.loader, to load all library files 
loaded by the other loader files.

Started work on documenting the Logtalk pre-processor/compiler source 
file.

New "errors" example. Updated the "inheritance" example.


2.8.4 - March 9, 2001
=====================

Updated config files for BinProlog 8.0, K-Prolog 5.01, XSB 2.3, and
Amzi! Prolog 4.103~5.0.

New config file for Amzi! Prolog 6.1 beta.

Corrected an incomplete entity definition in the Logtalk XML DTD.
Rewrite the Logtalk XSLT files for improved compatibility with XSLT 
processors regarding handling of whitespace.

A first cut of a XML Schema for the Logtalk XML documentation files
(file xml/logtalk.xsd).

Small improvements to the documentation.


2.8.3 - November 21, 2000
=========================

Corrected a bug where sending a message for a built-in method to an 
unknown object fails instead of generating the expected exception.

Put some occurrences of atoms public, mode, and type between ()'s to 
avoid compilation errors in BinProlog 8.0 and other Prolog compilers 
that declare these atoms as operators.

Corrected the definition of the state space of the "Missionaries and 
Cannibals" problem in the "searching" example that resulted in some 
wrong solutions.


2.8.2 - November 5, 2000
========================

New .xsl file to convert .xml files to TeXML files.

Fixed a problem with explicit compilation options being correctly parsed 
but not processed.

Corrected a bug regarding default init options definition for the 
points, polygons, and bricks examples. Updated category initialization 
in roots example.


2.8.1 - October 28, 2000
========================

New config file for K-Prolog 5.0.

Improved compiler error detection and reporting of invalid directives 
and clauses.

Corrected a problem with some Prolog compilers where compiling a file 
containing syntax errors may not close the file after the errors are 
reported.

Many small improvements to all documentation.

Small improvements to the .xsl files and to the output of .xml files.


2.8.0 - October 1, 2000
=======================

I have found just a few days ago that I have uploaded to the Logtalk web 
server a development version of Logtalk 2.7.0 instead of the final one!
To avoid confusions I decided to release a new version.

Changed implementation of the logtalk_compile/1-2 and logtalk_load/1-2 
predicates to only accept a list of entities instead of either an entity 
or a list of entities, simplifying and improving the performance of these 
predicates. Improved error reporting for the logtalk_compile/1 and 
logtalk_load/1 predicates. Updated relevant sessions in the user and 
reference manuals.

Simplified the implementation of the following predicates, by sharing 
the error checking code with the corresponding extended versions: 
implements_protocol/2, imports_category/2, instantiates_class/2, 
specializes_class/2, extends_protocol/2, and extends_object/2.

Completed some missing library file dependencies documentation in some 
examples.

New version of the QUICK_START help file. Updated tutorial.


2.7.0 - August 24, 2000
=======================

First release of the Logtalk standard library of objects, protocols and 
categories.

Rewritten all examples to use the new Logtalk standard library.

New logtalk_load/2 and logtalk_compile/2 Logtalk built-in predicates,
accepting a list of compiler options. See the User and Reference Manuals 
for details.

New XSLT file for viewing .xml files in Microsoft Internet Explorer 5.5 
for Windows after installing the latest Microsoft XML Parser Preview 
Release. Small improvements in all XSLT files.

Starting with this version all config files need to provide a definition 
for the compare/3 predicate if it is not built-in in the corresponding 
Prolog compiler. Also, default Logtalk compiler options are now also set 
in the config files using the lgt_default_compiler_option/2 predicate.

Updated config file for CIAO 1.6p1. See the configs/NOTES file for 
details.


2.6.2 - July 4, 2000
====================

Improved performance and error checking for the built-in predicates 
create_object/4, create_protocol/3 and create_category/4.

Updated config file for BinProlog 7.83.

Many small updates to all documentation.

New PDF versions of the documentation formated for printing (with page 
numbers, table of contents and index)are now available in both A4 and 
US letter formats.


2.6.1 - May 5, 2000
===================

Modified the structure of the automatically generated XML documenting 
files to improve XSL translation performance. Added a new, more standard 
compliant, XSL file to generate HTML files and renamed the old one to 
ie5.xsl. See the xml/NOTES file for details. Corrected an error in the 
logtalk.css CSS file.   

Modified the definitions of predicates lgt_file_extension/2 and 
lgt_file_name/2 in all config files.

Updated all the user manual sessions related to automatic documentation.

Updated all HTML documentation for future XHTML 1.0 conformance.

Start adding documenting directives to most examples.


2.6.0 - April 27, 2000
======================

Added support for documentation of objects, protocols, and categories, 
using automatic generation of XML files.

Added info/1 and info/2 directives for documenting objects, protocols,
categories, and predicates. Added definition of documentation file name
extension and default file names for the DTD and XSL files to the config 
files.

Improved error checking for the built-in predicates create_object/4,
create_protocol/3 and create_category/4 to also detect invalid entity 
identifiers.

Updated the user and reference manuals to describe the new automatic 
documenting features.

Updated all HTML documentation to conform to HTML 4.01 strict standard.

Corrected some wrong cross-reference links and titles in the reference 
and user manuals HTML pages.

PDF versions of the manuals and tutorial are now available in both A4 
and US letter formats.

Corrected two errors in the searching example, one in the definition of 
the farmer problem state space and the other when backtracking over
performance reports.


2.5.2 - March 7, 2000
=====================

Updated manuals to clarify some limitations of declaring and defining
dynamic predicates inside categories and documented a way of using the 
built-in local method this/1 to access object parameters.

Removed references to parametric categories from the Logtalk 
documentation (at runtime we can only access object parameters, 
not category parameters).

Corrected two wrong declarations of built-in methods (forall/2 and
retractall/1) in the Logtalk pre-processor.

Corrected bug where predicates declared in dynamic entities may have
a "static" instead of a "dynamic" property.

Corrected a bug in category compilation that prevented a dynamic category
from being abolished. Speedup predicate definition lookups for categories.


2.5.1 - February 18, 2000
=========================

Two new examples: birds, a bird identification expert system adopted 
(with permission) from an Amzi example, and viewpoints, a set of 
prototypes showing how to do property sharing and value sharing in 
Logtalk.

Renamed config file yap421.config to yap430.config to match the name of 
the new YAP 4.3.0 public release (named 4.2.1 while on beta testing).

Partial config file for Trinc Prolog R3.


2.5.0 - December 29, 1999
=========================

The Logtalk pre-processor now accepts arbitrary clauses and directives
to appear in a file before an opening entity directive. The clauses and
directives are added unchanged to the compiled file.

Improved performance for all kinds of message sending.

Two new examples: reflection, showing how to implement a simple 
class-based reflective system, and symdiff, showing how to use 
parametric objects to implement symbolic expression differentiation 
and simplification.

Updated config file for the beta 8 release of SWI-Prolog 3.3.0.


2.4.0 - December 1, 1999
========================

Logtalk is now an Open Source project, available under Perl's Artistic 
license.

Two new examples: instmethods, illustrating the use of instance defined 
methods, and classvars, showing how to implement class variables.

Updated Logtalk grammar to explicitly allow for user-defined types
in mode/2 directives.

New config files for SWI-Prolog 3.3.0, SICStus Prolog 3.8 and CIAO 
Prolog 1.4p0 (incomplete).

Updated config file for B-Prolog 4.0.

Updated config file for GNU Prolog to use the new call_with_args() 
built-in predicate added in version 1.1.0.

Updated config file for YAP Prolog to use the new call_with_args() 
built-in predicate added in version 4.2.1.


2.3.1 - September 22, 1999
==========================

Logtalk pre-processor updated to only report one warning per redefined
Logtalk or Prolog built-in predicate.

Changed some occurrences of atom "public" to "(public)" in 
compiler/logtalk.pl file to avoid syntax errors in Prolog compilers 
(like BinProlog) that define "public" as an operator. Also put some 
terms between ()'s to avoid syntax errors with ALS Prolog 3.1

Update config file for ALS Prolog to work with version 3.1.

Updated configs/NOTES file include a workaround to use Logtalk with
XSB 2.0, some guidelines on how to write loader utility files for
BinProlog 7.50, and a bug in ALS Prolog 3.1 that affects Logtalk.


2.3.0 - September 12, 1999
==========================

Metapredicate information is now stored with the other predicate 
properties, instead of being discarded after compiling the clauses of
a metapredicate. Added predicate property metapredicate/1. It is now 
possible to assert clauses for dynamic metapredicates. 

Corrected a bug in the processing of metacalls in pseudo-object user. 

Corrected a bug in the implementation of private inheritance.
Improved performance of protected inheritance.

Corrected failure of processing initialization/1 and op/3 directives in 
create_object/4, create_category/4 and create_protocol/3 built-in 
predicates.

Corrected a bug when calling private static category predicates from 
importing objects. Simplified code generated for categories.

Rewrite code for the built-in method current_predicate/1 to avoid 
duplicated or wrong results when overriding predicate declarations 
and to ensure that all possible argument type errors are detected.

Changed compilation of classes that do not instantiate any (meta)class
in order to throw the correct exception when a message is sent to them.

Changed compilation of root objects (that do not instantiate, specialize 
or extend other objects) so that calls to "super" (^^/1) fail (as they 
should) instead of generating a "predicate does not exist" error message.

Changed implementation of "super" calls (^^/1) to avoid a potential 
endless loop problem when using these calls from the inheritance root 
object of a reflexive class-based systems.

Pre-processor now checks for callable and object identifier errors.
Runtime now checks for non-instantiated messages.

Added new example (inheritance) about public, protected and private 
inheritance.

Updated metapredicates, lo, kernel, and types examples. Most of the 
code in the last two examples is being updated to form the basis of an 
upcoming Logtalk standard library.

Small changes in the pre-processor/runtime code for compatibility with
ECLiPSe 4.2.2. Updated config files for this Prolog compiler.

Rewrite some predicates in the config files for SWI-Prolog and LPA 
Mac & WIN Prologs for improved performance.

New tutorial session about events and monitors.

Updated documentation to match the changes made in this version.


2.2.0 - July 10, 1999
=====================

Implemented public, protected and private object inheritance, protocol
implementation and category importation.

New Logtalk built-in predicates instantiates_class/3, extends_object/3,
extends_protocol/3, specializes_class/3, imports_category/3 and
implements_protocol/3. The third argument returns the relation scope.

Pre-processor now checks most directive errors.

Changed ^^/1 (super call) implementation to only allow the use of a 
single predicate call for argument and to retain the original sender
of the message (see the updated sicstus example).

Fixed bug that prevented directives with a variable number of arguments
(like dynamic(), public(), uses(), ...) being recognized.

Changed definition of predicate read_term/3 in the configs files of Amzi!,
Aquarius, Arity, Eclipse, IC, K, LPA Mac&WIN, Master and XSB to always
instantiates the singleton variables list to the empty list. Needed to
prevent backtracking problems in logtalk_load/1 and logtalk_compile/1
predicates.

Removed choice-point in processing metacalls in predicate definitions.

A banner with the Logtalk version is printed after loading the runtime/
pre-processor.

Removed variables example. The category attributes is now part of the
kernel example. Corrected some harmless syntax errors in directives in
kernel/attributes.lgt, kernel/monitor.lgt and miscellaneous/queens.lgt.

Changed name of blocks example to bricks and object "block" to "brick" 
in order to avoid problems with some Prolog compilers that use the atom 
"block" for operators or built-in predicates.


2.1.0 - May 11, 1999
====================

Fixed some bugs in the definition of the pseudo-object user when sending 
the message predicate_property/2.

Renamed config file for Calypso to GNU Prolog.

New config file for LPA WinProlog 4.0.

Corrected the omission in the documentation of the Logtalk grammar of the 
built_in entity property.

New tutorial pages about building reflective class-based systems.

Modified pre-processor to compile throw/1 as a control structure to 
ensure compatibility with the GNU-Prolog compiler.

Modified pre-processor to ensure compatibility with Prolog compilers that
use immediate update semantics instead of the ISO logical database update.

Improved simplification of compiled object and category clauses.

Rewrite and clean up pre-processor code for better compiler performance.

Several updates to the examples files.

Changed all example loader files to wrap the call to logtalk_load/1 inside
an initialization/1 directive for ISO standard compliance.


2.0 GM - February 9, 1999
=========================

Removed some redundant choice-points that are being created when 
compiling Logtalk entities.

Small compilation speed optimizations.

Logtalk compiled files now contain the declarations for the message 
sending operators to ensure compatibility with some Prolog compilers.

Changed the way Logtalk pre-processor writes directives to ensure 
compatibility with some Prolog compilers. Corrected a bug in the
processing of the op/3 directive.

Updated PrologII+ config file for version 4.5.

Changed the definitions of catch/3 and throw/1 in the config files for 
LPA Prolog compilers.

New config file for MasterProlog 4.1; removed config file for the 
old BIM Prolog 4.0.

Corrected an error in the config file for OpenProlog in the definition
of the write_term/3 predicate.

Added a safer definition for write_term/3 predicate in the config  
files for Amzi, Aquarius, Arity, Eclipse, IC, K, LPA Mac, LPA Win, 
and XSB Prolog compilers.

Added a QUICK_START file.


2.0 Beta 3 - February 1, 1999
=============================

Closed some encapsulation holes in the implementation of super calls.

Changed Logtalk pre-processor to use write_term/3 instead of write/2 and 
writeq/2 to workaround some compatibility problems with some Prolog 
compilers.

Changed mode operators priority and type to be compatible with the ISO
standard.

Modified definition of predicate read_term/3 in the config file for Amzi 
compiler to return the atom end_of_file instead of '!EOF' at the end of
a file (this prevented Logtalk to complete compilation any file under 
this compiler). Improved detection of built-in predicates while compiling.

Removed config file for wamcc. This compiler have been replaced by 
Calypso (developed by the same author).

Updated K-Prolog config file for version 4.50.

Improved documentation of template config file.

Added SCRIPT files to the kernel and types examples.

Updated Tutorial and User and Reference Manuals.

New end user license.


2.0 Beta 2 - November 16, 1998
==============================

Built-ins logtalk_compile/1 and logtalk_load/1 updated to match 
the documentation regarding thrown errors and to accept a list of
entities to compile/load.

Modified the examples loader utility files to make only a call to
logtalk_load/1 by using a list of entities. This should make it easy to
modify these files for compilers that don't support arbitrary queries
in a file. 

Logtalk runtime no longer tries to catch some of the errors thrown 
by ::/2.

Added to all config files a Logtalk predicate to check if a file 
exists in the current working directory (used by the logtalk_load/1
and logtalk_compile/1 Logtalk built-in predicates).

New configs files for the K-Prolog 4.14 and Calypso 1.0b6 Prolog 
compilers.

Completed the config file for Open Prolog 1.03d38 (with the exception
of lgt_current_date/3 that still have a dummy definition).

Added a missing definition for Logtalk predicate forall/2 to the config
file for Prolog II+ 4.4.

Modified Logtalk pre-processor/runtime to avoid syntax errors when
running on Open Prolog or Calypso and to remove some redundant
choice-points that are being created when compiling categories and
protocols.

Modified some examples that use operators to avoid syntax errors in
some Prolog compilers.

Modified the implementation of the built-in method 
predicate_property/2 to avoid duplicate answers and to throw an
instantiation error if the first argument is not bound.

Modified definition of the pseudo-object user to hide Logtalk pre-
processor predicates from the current_predicate/1 built-in method and
to ensure that the predicate_property/2 built-in method returns
Logtalk defined predicate properties for built-in Logtalk and Prolog
predicates.

Modified Prolog code generated by the pre-processor to further minimize
possible conflicts with user defined predicates.

Added a lgt_predicate_property/2 predicate to the config files to
improve Logtalk portability.

Updated Tutorial and User and Reference Manuals.


2.0 Beta 1 - October 18, 1998
=============================

Added basic support for implementation multi-inheritance.

Logtalk pre-processor updated to try to detect misspelt local 
predicate calls.

First public beta.


2.0 Alpha 1 - July, 1998
========================

First release for registered users.
