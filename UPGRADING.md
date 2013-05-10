________________________________________________________________________

This file is part of Logtalk <http://logtalk.org/>  
Copyright (c) 1998-2013 Paulo Moura <pmoura@logtalk.org>

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
may be able to use conditional compilation directives to allow you code to
compile and run under multiple versions.


MAJOR CHANGES FROM LOGTALK 2.x TO LOGTALK 3.x
=============================================

Removed flags
-------------

The following Logtalk 2.x flags are no longer supported in Logtalk 3.x and
attempting to use them will result in compiler errors:

* `xmldir`, `xmldocs`, `xmlspec`, `xmlsref`, `xslfile`
* `break_predicate`
* `altdirs`
* `startup_message`
* `smart_compilation`
* `reload`

The XML related files were removed as generating documenting files is now the
responsibility of the `lgtdoc` tool. The debugger is also no longer a built-in
object but a separate tool, `debugger`. Logtalk 3.x greatly improves support
for source file paths, removing the need for the `altdirs` flag. The flag
`startup_message` is removed due to the introduction on structured message
printing in Logtalk 3.x, which allows fine grained control over message
printing. The `smart_compilation` flag is no longer necessary and the old
behavior can now be controlled using the `clean` flag. The `reload` flag,
which was mainly used for indirectly allowing static binding is no longer
required as static binding optimizations now depend on the `optimize` flag.


Deprecated directives and control constructs
--------------------------------------------

The `:/1` control construct is deprecated in Logtalk 3.x. Its functionality
subsumed by the `^^/1` control construct (aka "super" control construct),
which have been generalized to allow calling any imported or inherited
predicate. In addition, full support for the `^^/1` control construct have
been implemented (enabled by turning on the `optimize` compiler flag).

The documentation only directives `calls/1` and `uses/1` are deprecated. The
reflection API in Logtalk 3.x now returns all object (and module) dependencies
found on sufficiently instantiated `::/2` and `:/2` calls when compiling an
object.
