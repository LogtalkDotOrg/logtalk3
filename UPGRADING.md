________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2014 Paulo Moura <pmoura@logtalk.org>

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


UPGRADING GUIDELINES
====================

* Always check carefully the release notes when updating to a new version,
specially, REMOVED, CHANGED, and RENAMED items.

* Changes in the Logtalk compiler between releases may render some files from
older versions incompatible with new ones. You may need to update your local
Logtalk user folder by running the `logtalk_user_setup` shell script.

* If your Logtalk applications depend on some of the example files, it is
advisable that you check your code against the new version.

* It's usually a good idea to recompile your source files with the compiler
flag `clean` turned on after upgrading to a new Logtalk release to ensure
that all files will be recompiled disregarding any existing intermediate
files.

* In the case of incompatible changes between major Logtalk versions, you
may be able to use conditional compilation directives to allow your code to
compile and run under multiple versions.


MAJOR LANGUAGE CHANGES FROM LOGTALK 2.x TO LOGTALK 3.x
======================================================

Structured message printing
---------------------------

The new structured message printing mechanism, an extended implementation
of the same mechanism available in some Prolog systems, allows fine grained
control over printing of informative, warning, and error messages. This
mechanism is fully customizable using user-defined hook predicates.

Structured question asking
--------------------------

The new structured question asking mechanism complements the structured
message printing mechanism and allows fine grained control over querying
the user for data. This mechanism is fully customizable using user-defined
hook predicates.

Source file paths
-----------------

Relative and absolute source file paths are now fully supported by the
compiling and loading built-in predicates. In addition, the compiling
and loading predicates also accept Prolog source files (with the Prolog
code being compiled as Logtalk code).

Removed directives
------------------

The `synchronized/0` entity directive is no longer supported. But any usage
of this directive can be replaced by using instead the `synchronized/1`
predicate directive.

Removed flags
-------------

The following Logtalk 2.x flags are no longer supported in Logtalk 3.x and
attempting to use them will result in compiler errors:

* `xmldir`, `xmldocs`, `xmlspec`, `xmlsref`, `xslfile`
* `break_predicate`
* `altdirs`
* `startup_message`
* `smart_compilation`
* `misspelt`

The XML related flags were removed as generating documenting files is now the
responsibility of the `lgtdoc` tool. The debugger is also no longer a built-in
object but a separate tool, `debugger`. Logtalk 3.x greatly improves support
for source file paths, removing the need for the `altdirs` flag. The flag
`startup_message` is removed due to the introduction on structured message
printing in Logtalk 3.x (see also the provided sample settings file). The
`smart_compilation` flag is no longer necessary and the old behavior can now
be controlled using the `clean` flag.

The `reload` flag, is no longer considered or required for static binding
optimizations, which now depend on the `optimize` flag. In addition, the
default value for this flag is now `changed`, i.e. a file will be reloaded
if changed since last loaded (provided that the any explicit flags are the
same as the first time it was loaded).

Deprecated directives, control constructs, and flags
----------------------------------------------------

The `:/1` control construct is deprecated in Logtalk 3.x. Its functionality
is subsumed by the `^^/1` control construct (aka "super" control construct),
which have been generalized to allow calling any imported or inherited
predicate. In addition, static binding support for the `^^/1` control
construct have been implemented (enabled by turning on the `optimize`
compiler flag).

The documentation only directives `calls/1` and `uses/1` are deprecated. The
reflection API in Logtalk 3.x now returns all object (and module) dependencies
found on sufficiently instantiated `::/2` and `:/2` calls when compiling an
object or a category.

The `alias/3` directive is deprecated and replaced by the new `alias/2`
directive.

The `version` flag is deprecated. New code that doesn't require compatibility
with Logtalk 2.x should use instead the new `version_data` flag.

The Logtalk 2.x `unknown`, `singletons`, and `tmpdir` are renamed in Logtalk
3.x for clarity. The old names are deprecated and should only be used when
compatibility with Logtalk 2.x is still required.

Stricter syntax
---------------

Logtalk 3.x enforces sanctioned syntax for directives. Notably, predicate
scope directives and entity relations specified in entity opening directives
are restricted to a single argument (which can be, as always, a single item,
a list of items, or a comma-separated sequence of items).

Semantic changes
----------------

The `{}/1` control construct is now opaque to cuts, thus ensuring the same
semantics when its argument is known at compile and when it's only known at
runtime.

Logtalk and Prolog built-in predicates can no longer be used as messages
in order to avoid code portability issues. The workaround is to wrap calls
to these predicates using the `{}/1` control construct when using them as
messages.

Meta-arguments are now always called with the meta-predicate caller full
execution context. In addition, when calling a meta-predicate from within
a category, meta-arguments are called in the context of the category instead
of the context of the object importing the category. This allows moving code
that calls meta-predicates between objects and categories without requiring
changes.

The `initialization/1` directive is only accepted as a source file directive
or as an object directive. Usage in protocols and categories is no longer
supported.

Reflection support
------------------

Logtalk 3.x includes major improvements to reflection support. In particular,
it adds entity properties such as `calls/2` (for retrieving predicate cross-
referencing information) but also removes some subsumed Logtalk 2.x properties
such as `uses/3` and `use_module/3`.

Debugging support
-----------------

The debugger is no longer a built-in pseudo-object. The debugging support have
been moved to a Logtalk application, `debugger`, available in the `tools`
directory. This new tool takes advantage of the improved reflection API.

Documenting support
-------------------

The compiler no longer outputs XML documenting files for the compiled source
files. The documenting support have been moved to a Logtalk application,
`lgtdoc`, available in the `tools` directory. This new tool takes advantage
of the improved reflection API.

New developer tools
-------------------

A new directory, `tools`, contains a comprehensive set of developer tools,
either new or greatly improved from Logtalk 2.x.
