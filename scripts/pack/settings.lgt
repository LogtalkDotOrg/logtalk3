%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  sample settings file
%  Last updated on February 16, 2015
%
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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
%  This is specially useful when Logtalk is installed system-wide in a
%  read-only directory. Using settings files allows Logtalk to easily support
%  user-specific settings. Note that the settings defined here can always
%  be overridden by using the `logtalk_compile/2` and `logtalk_load/2` built-in
%  predicates or by using the `set_logtalk_flag/2` directive within the source
%  files.
%
%  Logtalk looks for a settings file first in the startup directory (thus
%  supporting per-project settings files). If not found, Logtalk looks for
%  a settings file in the Logtalk user directory. If no settings file is
%  found, Logtalk will use the default flag values defined in the backend
%  Prolog compiler adapter file. It's however possible to restrict searching
%  of settings files to the Logtalk user directory and to disable settings
%  files by changing the definition of the read-only flag `settings_file`
%  in the used Prolog adapter file.
%
%  Logtalk uses the value of the `LOGTALK_STARTUP_DIRECTORY` environment
%  variable for the startup directory and the value of the `LOGTALKUSER`
%  environment variable for the Logtalk user directory. The POSIX integration
%  scripts automatically set the `LOGTALK_STARTUP_DIRECTORY` variable.
%  On Windows systems, the integration shortcuts `Start in` field is set by
%  default to `%LOGTALKUSER%` as the `%CD%` alternative only works on some
%  versions. A workaround to use per-project settings files is to copy the
%  shortcut to the project directory and to and edit its `Target` field to
%  prefix its content with:
%
%  C:\Windows\System32\cmd.exe /c set LOGTALK_STARTUP_DIRECTORY=%CD% &&
%
%  When the `LOGTALK_STARTUP_DIRECTORY` variable is not available, Logtalk
%  uses the current directory as returned by the backend Prolog compiler.
%
%  To use settings files, copy this file to your Logtalk user directory or to
%  the directory containing your project files, rename it to `settings.lgt`,
%  customize it (see the examples below), and start Logtalk from the desired
%  directory. Note that, for setting Logtalk flag values, you must use the
%  `set_logtalk_flag/2` predicate (wrapped in a `initialization/1` directive)
%  as the scope of the `set_logtalk_flag/2` directive is local to the entity
%  or the source file containing it.
%
%  If you use more than one back-end Prolog compiler and want to use per
%  compiler settings, you can use the Logtalk conditional compilation
%  directives and the `prolog_dialect` compiler flag. See the User and
%  Reference Manuals for details.
%
%  Logtalk compiles and loads settings files silently but a warning will
%  be printed by default if syntax errors are found. Be sure to debug and
%  test your settings files as regular Logtalk source files before using
%  them (you may use the `logtalk_compile/1-2` built-in predicates to compile
%  the settings files without loading them).
%
%  Limitations of the back-end Prolog compilers may prevent settings files
%  to work from directories other than the Logtalk user directory, specially
%  when running on non-POSIX operating systems such as Windows. Check the 
%  `adapters/NOTES.md` file for compatibility details.


%  To load the `help` example at startup, which provides basic on-line help
%  for Logtalk, uncomment the following lines:

/*
:- initialization(
	logtalk_load(help(loader), [report(off)])
).
*/


%  To define a "library" alias for your projects, edit and uncomment the
%  following lines (the library path must end with a slash character):

/*
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_project, '$HOME/my_project/').
logtalk_library_path(my_project_libraries, my_project('libraries/')).
logtalk_library_path(my_project_examples, my_project('examples/')).
*/


%  To define a common directory for Logtalk compiler generated temporary
%  Prolog files, edit and uncomment the following lines (the directory
%  path must end with a slash character):

/*
:- initialization((
	set_logtalk_flag(scratch_directory, '$LOGTALKUSER/logtalk/scratch/')
)).
*/


%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:

/*
:- initialization((
	%set_logtalk_flag(prolog_loader, [silent(true)]),   % for SWI-Prolog and YAP
	%set_stream(log_output, null),                      % for ECLiPSe
	%set_prolog_flag(informational, off),               % SICStus Prolog
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


%  To compile all your source files for debugging using the Logtalk
%  default debugger, uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(debug, on),
	set_logtalk_flag(clean, on),
	set_logtalk_flag(reload, always),
	set_logtalk_flag(unknown_entities, warning),
	set_logtalk_flag(unknown_predicates, warning),
	set_logtalk_flag(undefined_predicates, warning),
	set_logtalk_flag(singleton_variables, warning),
	set_logtalk_flag(context_switching_calls, allow),
	set_logtalk_flag(optimize, off),
	set_logtalk_flag(source_data, on)
)).
*/


% To take advantage of ECLiPSe `.eco` files, uncomment the
% following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(prolog_loader, [output:eco])
)).
*/


% To take advantage of SWI-Prolog `.qlf` files, uncomment the
% following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(prolog_loader, [qcompile(auto)])
)).
*/


%  To compile all your source files for debugging using the SWI-Prolog
%  graphical tracer (stable version 6.2.0 or later; development version
%  6.1.11 or later), uncomment the following lines:

/*
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- initialization((
		set_logtalk_flag(debug, off),
		set_logtalk_flag(clean, on),
		set_logtalk_flag(reload, always),
		set_logtalk_flag(unknown_entities, warning),
		set_logtalk_flag(unknown_predicates, warning),
		set_logtalk_flag(undefined_predicates, warning),
		set_logtalk_flag(singleton_variables, warning),
		set_logtalk_flag(context_switching_calls, allow),
		set_logtalk_flag(code_prefix, '.'),
		set_logtalk_flag(optimize, off),
		set_logtalk_flag(source_data, on),
		set_prolog_flag(optimise, off),
		set_prolog_flag(logtalk_source_location_data, true)
	)).

:- endif.
*/


%  To compile all your source files for profiling using the SWI-Prolog
%  graphical profiler (stable version 6.2.0 or later; development version
%  6.1.11 or later), uncomment the following lines:

/*
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- use_module(library(statistics)).

	:- initialization((
		set_logtalk_flag(code_prefix, '.'),
	)).

:- endif.
*/


%  To use PDT for Logtalk development, uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(debug, off),
	set_logtalk_flag(clean, on),
	set_logtalk_flag(code_prefix, '.'),
	set_logtalk_flag(optimize, off),
	set_logtalk_flag(source_data, on),
	set_prolog_flag(optimise, off)
)).
*/


%  To automatically delete temporary files generated during the compilation
%  of source files (strongly advised when alternating between backend Prolog
%  compilers), uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(clean, on)
)).
*/


%  To avoid recompilation of stable source files (assuming a single backend
%  Prolog compiler is being used), uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(reload, changed)
)).
*/


%  To develop portable Logtalk applications uncomment the following lines
%  to help you catch possible non-portable built-in predicate calls, use
%  of non-standard flags or non-standard flag values, and missing predicate
%  directives:

/*
:- initialization((
	set_logtalk_flag(unknown_predicates, warning),
	set_logtalk_flag(portability, warning),
	set_logtalk_flag(missing_directives, warning)
)).
*/


%  To maximize performance when deploying an application by turning on all
%  optimizations and turning off relevant optional features and collecting
%  source data for integration with development tools, uncomment the following
%  lines:

/*
:- initialization((
	set_logtalk_flag(debug, off),
	set_logtalk_flag(optimize, on),
	set_logtalk_flag(source_data, off),
	set_logtalk_flag(events, deny),
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(dynamic_declarations, deny)
)).
*/


%  To prevent using the <</2 debugging context-switching control construct
%  to bypass object encapsulation rules uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(context_switching_calls, deny)
)).
*/


%  To lock your entities to prevent breaking encapsulation, uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(context_switching_calls, deny),
	set_logtalk_flag(dynamic_declarations, deny),
	set_logtalk_flag(source_data, off)
)).
*/


%  To suppress some or all startup messages, uncomment the following lines:
%  (you can use in alternative the `report` flag but this flag also affects
%  source file compilation and loading reports)


:- category(my_logtalk_startup_settings).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% uncomment the next line to suppress the startup banner
	logtalk::message_hook(banner, banner, core, _).

	% uncomment the next line to suppress the startup printing of default flags
	logtalk::message_hook(default_flags, comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of the loaded settings file
	logtalk::message_hook(loaded_settings_file(_), comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of settings information (except warnings and errors)
	logtalk::message_hook(_, comment(settings), core, _).

:- end_category.



%  To print all otherwise silent compiler messages, uncomment the following
%  lines:

/*
:- category(my_logtalk_message_settings).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, silent, core, Tokens) :-
		logtalk::message_prefix_stream(comment, core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

	logtalk::message_hook(_Message, silent(Key), core, Tokens) :-
		logtalk::message_prefix_stream(comment(Key), core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

:- end_category.
*/
