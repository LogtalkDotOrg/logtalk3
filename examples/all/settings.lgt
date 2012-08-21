%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright (c) 1998-2012 Paulo Moura <pmoura@logtalk.org>
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%  
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%  
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>.
%  
%  Additional licensing terms apply per Section 7 of the GNU General
%  Public License 3. Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%  This is a sample settings file for Logtalk that can be used to override
%  the default flag values in the back-end Prolog compiler adapter files.
%  Using settings files allows Logtalk to easily support per-project
%  settings. Note that the settings defined here can always be overridden
%  by using the logtalk_compile/2 and logtalk_load/2 built-in predicates
%  or by using the set_logtalk_flag/2 directive within the source files.
%
%  To use this feature, copy this file to the directory containing your
%  project files, customize it (see the examples below), and start Logtalk
%  from the project directory. Note that, for setting Logtalk flag values,
%  you must use the set_logtalk_flag/2 predicate (wrapped in a directive
%  initialization/1) as the scope of the set_logtalk_flag/2 directive is
%  always local to the entity or the source file containing it.
%
%  If you use more than one back-end Prolog compiler and want to use
%  different settings per compiler you will need to use the Logtalk 
%  conditional compilation directives and the "prolog_dialect" compiler
%  flag. See the User and Reference Manuals for details.
%
%  Logtalk compiles and loads settings files silently, ignoring any errors.
%  Be sure to debug and test your settings files as normal Logtalk source
%  files before using them (you may use the logtalk_compile/1-2 built-in
%  predicates to compile the settings files without loading them).
%
%  Logtalk looks for a settings file first in the startup directory, If not
%  found, Logtalk looks for a settings file in the Logtalk user directory.
%  If no settings file is found, Logtalk will use the default flag values
%  defined in the back-end Prolog compiler adapter file.
%
%  Limitations of the back-end Prolog compilers may prevent settings files
%  to work from directories other than the Logtalk user directory, specially
%  when running on non-POSIX operating systems such as Windows. Check the 
%  "adapters/NOTES.txt" file for compatibility details.


%  To load the "help" example at startup, which provides basic on-line help
%  for Logtalk, uncomment the following lines:

/*
:- initialization(
	logtalk_load(help(loader), [report(off)])
).
*/


%  To define a "library" path for your projects, edit and uncomment the
%  following lines (the library path must end with a slash character):

/*
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_project, '$HOME/my_project/').
logtalk_library_path(my_project_examples, my_project('examples/')).
*/


%  To define a common directory for Logtalk compiler generated temporary
%  files and for Logtalk automatically generated documentation files for
%  compiled entities, edit and uncomment the following lines (the library
%  paths must end with a slash character):

/*
:- initialization((
	set_logtalk_flag(scratch_directory, '$HOME/logtalk/.lgt_tmp/')
)).
*/


%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:


:- initialization((
	set_logtalk_flag(report, off)
)).



%  To make Logtalk startup and compilation less verbose uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(report, warnings)
)).
*/


%  To compile all your source files for debugging uncomment the following
%  lines:

/*
:- initialization((
	set_logtalk_flag(debug, on),
	set_logtalk_flag(smart_compilation, off),
	set_logtalk_flag(reload, always),
	set_logtalk_flag(unknown_entities, warning),
	set_logtalk_flag(misspelt_calls, warning),
	set_logtalk_flag(singleton_variables, warning),
	set_logtalk_flag(context_switching_calls, allow)
)).
*/


%  To reduce clutter in the directory containing your source files uncomment
%  the following lines:

/*
:- initialization((
	set_logtalk_flag(clean, on)
)).
*/


%  To avoid recompilation of stable source files uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(smart_compilation, on),
	set_logtalk_flag(clean, off)
)).
*/


%  To collect all XML documenting files in the same place for generating 
%  (X)HTML or PDF documentation of your project edit and uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(xmldir, '$HOME/my_project_docs/')
)).
*/


%  To develop portable Logtalk applications uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(portability, warning)
)).
*/


%  To maximize performance by turning off relevant optional features
%  uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(events, deny),
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(dynamic_declarations, deny)
)).
*/


%  To prevent using the <</2 context-switching control construct to bypass
%  object encapsulation rules uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(context_switching_calls, deny)
)).
*/
