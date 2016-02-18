%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
%  
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%  
%      http://www.apache.org/licenses/LICENSE-2.0
%  
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
%  Logtalk compiles and loads settings files silently but a warning will
%  be printed if syntax errors are found. Be sure to debug and test your
%  settings files as regular Logtalk source files before using them (you
%  may use the logtalk_compile/1-2 built-in predicates to compile the
%  settings files without loading them).
%
%  Logtalk looks for a settings file first in the startup directory, If not
%  found, Logtalk looks for a settings file in the Logtalk user directory.
%  If no settings file is found, Logtalk will use the default flag values
%  defined in the back-end Prolog compiler adapter file.
%
%  Limitations of the back-end Prolog compilers may prevent settings files
%  to work from directories other than the Logtalk user directory, specially
%  when running on non-POSIX operating systems such as Windows. Check the 
%  "adapters/NOTES.md" file for compatibility details.


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
%  files, edit and uncomment the following lines (the library paths must
%  end with a slash character):

/*
:- initialization((
	set_logtalk_flag(scratch_directory, '$HOME/logtalk/.lgt_tmp/')
)).
*/


%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(report, off)
)).
*/


%  To make Logtalk startup and compilation less verbose uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(report, warnings)
)).
*/


%  To compile all your source files for debugging using the Logtalk built-in
%  debugger uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(debug, on),
	set_logtalk_flag(unknown_entities, warning),
	set_logtalk_flag(unknown_predicates, warning),
	set_logtalk_flag(undefined_predicates, warning),
	set_logtalk_flag(singleton_variables, warning),
	set_logtalk_flag(context_switching_calls, allow),
	set_logtalk_flag(optimize, off),
	set_logtalk_flag(source_data, on)
)).
*/


%  To compile all your source files for debugging using the SWI-Prolog
%  graphical tracer uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(debug, off),
	set_logtalk_flag(clean, on),
	set_logtalk_flag(unknown_entities, warning),
	set_logtalk_flag(unknown_predicates, warning),
	set_logtalk_flag(undefined_predicates, warning),
	set_logtalk_flag(singleton_variables, warning),
	set_logtalk_flag(context_switching_calls, allow),
	set_logtalk_flag(code_prefix, '.'),
	set_logtalk_flag(optimize, off),
	set_logtalk_flag(source_data, on),
	set_prolog_flag(optimise, off)
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
	set_logtalk_flag(clean, off)
)).
*/


%  To develop portable Logtalk applications uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(portability, warning)
)).
*/


%  To maximize performance by turning on all optimizations and by turning
%  off relevant optional features uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(optimize, on),
	set_logtalk_flag(source_data, off),
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
