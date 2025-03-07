________________________________________________________________________

This file is part of Logtalk <https://logtalk.org/>  
SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>  
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
________________________________________________________________________


This folder contains adapter files for Prolog compilers that are not yet
officially supported or no longer supported due to lack of compliance
with standards and/or unfixed bugs. Follows some notes on these compilers.


Amzi! Prolog 7.6.1 and later versions
-------------------------------------

	amzi.pl

For Amzi! Prolog 7.6.1 and later versions. You need to patch the Logtalk
compiler (the "core/core.pl" file) by searching for all calls of
':'/2 and wrap them inside call/1 (search for the text "':'(" to locate
the calls). For better performance, use the Amzi! "acmp" command-line
compiler to compile the files "adapters/amzi.pl", "core/core.pl",
and "paths/paths.pl" and then load the resulting ".plm" files
using the predicate load/1 (you will need to edit the "paths.pl" file
by following the instructions on the "paths/NOTES.md" file).

You will probably need to increase some of the default values (e.g., the
"control" parameter) in the "amzi.cfg" file in order to be able to load
some of the libraries or some of the examples.

One potential problem is that the compiler definition of the compare/3
built-in predicate returns the atom == instead of = for identical terms
(contrary to every other Prolog compiler!). Search the library files for
all calls of the compare/3 predicate and make the necessary changes.
The built-in sort/2 predicate does not eliminate duplicates, which may
result in problems with some of the examples. Don't forget to use the
chdir/1 predicate to set the working directory before loading a library
or an example. Support for settings files not tested.


Bin-Prolog 8.x~10.x
-------------------

	bin.pl

Start BinProlog using "bp -l4". You will need to create project files to
workaround the restriction of only one top-level file per interactive
session. For instance, the project file for the "metapredicates" example
will look like (in the Unix version):

	:- ['$LOGTALKHOME/adapters/bin.pl'].
	:- ['$LOGTALKHOME/core/core.pl'].
	...
	:- ['$LOGTALKUSER/examples/metapredicates/meta.pl'].
	:- ['$LOGTALKUSER/examples/metapredicates/sort1.pl'].
	...

You will probably want to have a project file including only the adapter
and the compiler/runtime files (core.pl) in order to compile the
examples using logtalk_compile/1-2 (do NOT use logtalk_load/1-2 or the
provided loader files). Don't forget to call the cd/1 predicate to set
the working directory before compiling the library or an example.
Supports smart compilation of source files. Support for settings files
not tested.

Updated and tested with help of Arun Majumdar.


IF/Prolog 5.3 and later versions
--------------------------------

	if.pl

IF/Prolog 5.3 supports the ISO Prolog standard. No problems expected
but not tested (my email requests for an evaluation version were never
answered). Don't forget to use the chdir/1 predicate to set the
working directory before loading the library or an example. Supports
smart compilation of source files. Does not support the "altdirs"
compiler flag. Full support for settings files on POSIX operating-
systems. Support for settings files on Windows unknown.


Lean Prolog 4.5.7 and later versions
------------------------------------

	lean.pl

Experimental. Lean Prolog lacks compliance with both official and de facto
standards. Notably, it lacks support for stream aliases, for the `0'Char`
notation, for escape sequences in atoms, and its limited parser often
requires atoms to be quoted and written between ()'s. Moreover, due to
Lean Prolog limitation of compiling more than one file per session, the
compilation of Logtalk source files generates intermediate Prolog files
that are consulted (i.e., interpreted) instead of being compiled, severely
hurting performance. Thus, practical use requires embedding Logtalk and the
Logtalk application in Lean Prolog by including all necessary files in a
single file that can then be compiled. For example, you can collect all
Logtalk core files in a single file by following the steps (exemplified
for POSIX systems):

	$ lplgt
	...
	| ?- logtalk_compile([
			core(expanding),
			core(monitoring),
			core(forwarding),
			core(user),
			core(logtalk),
			core(core_messages)],
			[optimize(on), scratch_directory('$HOME/collect')]).

	$ cp $LOGTALKHOME/adapters/lean.pl $HOME/collect/lean.pl
	$ cp $LOGTALKHOME/paths/paths.pl $HOME/collect/paths.pl
	$ cp $LOGTALKHOME/core/core.pl $HOME/collect/core.pl

	$ cd $HOME/collect
	$ cat lean.pl paths.pl expanding*_lgt.pl monitoring*_lgt.pl forwarding*_lgt.pl user*_lgt.pl logtalk*_lgt.pl core_messages*_lgt.pl core.pl > logtalk_core.pl

Starting Lean Prolog with a Java allocation of at least 2GB is recommended
(by passing a `-Xmx2G` option to Java).


LPA MacProlog32 1.25
--------------------

	lpamac.pl

This is my old Prolog development environment. Two known problems: (1) an
LPA bug in operator handling that makes calls like "\+ ::Pred" be
interpreted like "::(\+ Pred)" instead of "\+ (::Pred)". A workaround is
to replace all occurrences of "\+ ::Pred" by "\+ (::Pred)"; (2) If you call
the \+ operator in your code, the `writeq/1` built-in don't always output a
space after the operator, resulting in calls like "\+(...)". Because \+ is
not defined as a predicate this will fail. Don't forget to use the `dvol/1`
predicate to set the working directory before loading an example.
Due to the size of the Logtalk compiler/runtime file, you must load it by
using the consult/1 predicate instead of using the File:Open menu option.
Be aware that this adapter file redefines some built-ins that
you may use in your own programs. You must be careful to not consult the
adapter file twice. Supports smart compilation of source files. Does not
support the "altdirs" compiler flag. Settings files are not supported
(must be manually loaded after starting Logtalk).


LPA WinProlog32 4.0x
--------------------

	lpawin.pl

Written with the help of the LPA support team. Of course, if you find
any bugs please direct your flames to me ;-). Be aware that this adapter
file redefines some built-ins that you may use in your programs. Don't
forget to use the chdir/1 predicate to set the working directory before
loading the library or an example. Be careful to not consult the adapter
file twice. Supports smart compilation of source files. Does not support
the "altdirs" compiler flag. Settings files are not supported (must be
manually loaded).


Qu-Prolog 9.7 and later versions
--------------------------------

	qp.pl

Known issues in Qu-Prolog 9.7: (1) this version doesn't support static
multifile predicates. This limitation may be fixed in later versions;
(2) a bug in the Windows version of Qu-Prolog in the expansion of file
paths containing environment variables prevents using it with Logtalk
in this operating-system; (3) the adapter file redefines the `(:)/2`
operator per its ISO Prolog standard definition.

Written and tested with the help of Peter Robinson, Qu-Prolog author.


Minerva 2.4
-----------

	minerva.pl

Incomplete. Missing features prevents Logtalk from running.


O-Prolog 1.12
-------------

	o.pl

Although version 1.12 is able to compile most of the Logtalk core files,
lack of standards compliance in key features and several bugs currently
prevent Logtalk from running.


tuProlog 2.9.0
--------------

	tu.pl

Incomplete. Missing tuProlog features include de facto standard predicates
(such as between/3, numbervars/3, and predicate_property/2) and operating-
system access predicates (notably for getting and setting the current
directory).
