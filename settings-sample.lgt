%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Sample settings file
%  Last updated on January 17, 2021
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


/*
About
-----

This is a sample settings file for Logtalk that can be used to override
the default flag values in the backend Prolog compiler adapter files, to
automatically load at startup libraries and tools, and to perform other
initializations (including backend specific ones).

Settings files are specially useful when Logtalk is installed system-wide
in a read-only directory. Using settings files allows Logtalk to easily
support user-specific and project-specific settings. Note that the settings
defined here can always be overridden by using the `logtalk_compile/2` and
`logtalk_load/2` built-in predicates or by using the `set_logtalk_flag/2`
directive within the source files.

Settings files are also a convenient place to define your own shortcuts
for commonly used queries (e.g. load the debugger and start tracing).


Settings file locations
-----------------------

Logtalk looks for a settings file in the following directories and order:

- Startup directory (``$LOGTALK_STARTUP_DIRECTORY``)
- Logtalk user directory (``$LOGTALKUSER``)
- User home directory (``$HOME``; ``%USERPROFILE%`` on Windows if ``%HOME%`` is not defined)
- Application data directory (``%APPDATA%\Logtalk``; only on Windows)
- Config directory (``$XDG_CONFIG_HOME/logtalk``)
- Default config directory (``$HOME/.config/logtalk/``)

The `LOGTALK_STARTUP_DIRECTORY` environment variable provides support for
per-project setting files. The POSIX integration scripts automatically set
this variable. On Windows systems, the integration shortcuts "Start in"
field is set by default to `%LOGTALKUSER%` as the `%CD%` alternative only
works on some Windows versions. A workaround to use per-project settings
files is to copy the shortcut to the project directory and and edit its
"Target" field to prefix its content with:

	C:\Windows\System32\cmd.exe /c set LOGTALK_STARTUP_DIRECTORY=%CD% &&

When the `LOGTALK_STARTUP_DIRECTORY` variable is not available, Logtalk
uses the current directory as returned by the backend Prolog compiler.

If no settings file is found, Logtalk will use the default flag values
defined in the backend Prolog compiler adapter file. It's however possible
to restrict searching of settings files to the Logtalk user directory and
to the home directory or to disable settings files by changing the
definition of the read-only flag `settings_file` in the used Prolog
adapter file from `allow` to, respectively, `restrict` or `deny`.


Defining a settings file
------------------------

To use settings files, copy this file to one of the supported locations
listed above, rename it to `settings.lgt`, customize it (see the sample
code below), and start Logtalk from the project directory (if you're
using a project specific settings file) or from anywhere (when using a
default settings file).

Note that, for setting Logtalk flag values, you must use the predicate
`set_logtalk_flag/2` (wrapped in a `initialization/1` directive) as the
scope of the `set_logtalk_flag/2` directive is local to the entity or
the source file containing it.

If you use multiple backend Prolog compilers and want to define per
compiler settings, you can use the Logtalk conditional compilation
directives and the `prolog_dialect` compiler flag. See the User and
Reference Manuals for details.


Caveats
-------

Logtalk compiles and loads settings files silently but a warning will
be printed by default if syntax errors are found. Be sure to debug and
test your settings files as regular Logtalk source files before using
them (you may use the `logtalk_compile/1-2` built-in predicates to compile
the settings files without loading them to check for errors and warnings).

Limitations of the backend Prolog compilers may prevent settings files
to work from directories other than the Logtalk user directory, specially
when running on non-POSIX operating systems such as Windows. Check the
`adapters/NOTES.md` file for compatibility details.
*/


%  To load the `help` tool at startup, which provides basic on-line help
%  for Logtalk, uncomment the following lines:

/*
:- initialization(
	logtalk_load(help(loader))
).
%*/


%  To load the `tutor` tool at startup to get additional explanations and
%  suggestions for the compiler warning and error messages, uncomment the
%  following lines:

/*
:- initialization(
	logtalk_load(tutor(loader))
).
%*/


%  To load most of developer tools at startup (including the `help` tool),
%  uncomment the following lines:

/*
:- initialization(
	logtalk_load(tools(loader))
).
%*/


%  To call ECLiPSe/SWI-Prolog make/0 predicate when calling the logtalk_make/0
%  (or logtalk_make/1 with the target all) predicate, uncomment the following
%  lines:

/*
:- if((
	current_logtalk_flag(prolog_dialect, Dialect),
	(Dialect == eclipse; Dialect == swi)
)).

	:- multifile(logtalk_make_target_action/1).
	:- dynamic(logtalk_make_target_action/1).

	logtalk_make_target_action(all) :-
		make.

:- endif.
%*/


%  To define a "library" alias for your projects, edit and uncomment the
%  following lines (the library paths must end with a slash character):

/*
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_project, home('my_project/')).
logtalk_library_path(my_project_libraries, my_project('libraries/')).
logtalk_library_path(my_project_examples, my_project('examples/')).
%*/


%  To define a "library" alias for your project while making it
%  relocatable, use a project specific settings file and edit and
%  uncomment the following lines:

/*
:- initialization((
	logtalk_load_context(directory, Directory),
	assertz(logtalk_library_path(my_project, Directory))
)).
%*/


%  To easily load multiple personal Logtalk projects without having to
%  first define a library alias for each one, create a directory for
%  storing their directories (e.g. `$HOME/my_logtalk_projects`), and
%  edit and uncomment the following lines:

/*
:- initialization((
	logtalk_load(types(loader)),
	logtalk_load(os(loader))
)).

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_logtalk_projects, home('my_logtalk_projects/')).
logtalk_library_path(Project, my_logtalk_projects(ProjectPath)) :-
	logtalk::expand_library_path(my_logtalk_projects, MyProjectsPath),
	os::directory_files(MyProjectsPath, Projects, [type(directory), dot_files(false)]),
	list::member(Project, Projects),
	atom_concat(Project, '/', ProjectPath).
%*/


%  To easily load third-party libraries without having to first define
%  a library alias for each one, create a directory for storing their
%  directories (e.g. `$HOME/logtalk_third_party_libraries`), and edit
%  and uncomment the following lines:

/*
:- initialization((
	logtalk_load(basic_types(loader)),
	logtalk_load(os(loader))
)).

:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(third_party_libraries, home('logtalk_third_party_libraries/')).
logtalk_library_path(Library, third_party_libraries(LibraryPath)) :-
	logtalk::expand_library_path(third_party_libraries, ThirdPartyLibrariesPath),
	os::directory_files(ThirdPartyLibrariesPath, Libraries, [type(directory), dot_files(false)]),
	list::member(Library, Libraries),
	atom_concat(Library, '/', LibraryPath).
%*/


%  To define a common directory for Logtalk compiler generated temporary
%  Prolog files, edit and uncomment the following lines (the directory
%  path must end with a slash character):

/*
:- initialization((
	set_logtalk_flag(scratch_directory, '$LOGTALKUSER/scratch/')
)).
%*/


%  To make Logtalk completely silent for batch processing uncomment the
%  following lines:

/*
:- initialization((
	%set_logtalk_flag(prolog_loader, [silent(true)]),   % for SWI-Prolog and YAP
	%set_stream(log_output, null),                      % for ECLiPSe
	%set_prolog_flag(informational, off),               % for SICStus Prolog
	set_logtalk_flag(report, off)
)).
%*/


%  To make Logtalk startup and compilation less verbose uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(report, warnings)
)).
%*/


%  To compile all your source files for debugging using the Logtalk
%  default debugger, uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(debug, on),
	set_logtalk_flag(clean, on),
	set_logtalk_flag(reload, always),
	set_logtalk_flag(source_data, on)
)).
%*/


% To take advantage of ECLiPSe `.eco` files, uncomment the
% following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(prolog_loader, [output:eco])
)).
%*/


% To take advantage of SWI-Prolog `.qlf` files, uncomment the
% following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(prolog_loader, [qcompile(auto)])
)).
%*/


%  To compile all your source files for debugging using the SWI-Prolog
%  graphical tracer, uncomment the following lines and ensure that this
%  initialization block is the first one in your settings file as it
%  changes the default value of the code_prefix flag:

/*
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- initialization((
		set_logtalk_flag(debug, off),
		set_logtalk_flag(clean, on),
		set_logtalk_flag(reload, always),
		set_logtalk_flag(code_prefix, '.'),
		set_logtalk_flag(optimize, off),
		set_logtalk_flag(source_data, on),
		set_prolog_flag(optimise, off),
		set_prolog_flag(logtalk_source_location_data, true)
	)).

:- endif.
%*/


%  To compile all your source files for profiling using the SWI-Prolog
%  graphical profiler, uncomment the following lines and ensure that
%  this initialization block is the first one in your settings file as
%  it changes the default value of the code_prefix flag:

/*
:- if(current_logtalk_flag(prolog_dialect, swi)).

	:- use_module(library(statistics)).

	:- initialization((
		set_logtalk_flag(code_prefix, '.'),
		set_prolog_flag(logtalk_source_location_data, true)
	)).

:- endif.
%*/


%  To use PDT for Logtalk development, uncomment the following lines and
%  ensure that this initialization block is the first one in your settings
%  file as it changes the default value of the code_prefix flag:

/*
:- initialization((
	set_logtalk_flag(debug, off),
	set_logtalk_flag(clean, on),
	set_logtalk_flag(code_prefix, '.'),
	set_logtalk_flag(optimize, off),
	set_logtalk_flag(source_data, on),
	set_prolog_flag(optimise, off)
)).
%*/


%  To automatically delete temporary files generated during the compilation
%  of source files (strongly advised when alternating between backend Prolog
%  compilers), uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(clean, on)
)).
%*/


%  To avoid recompilation of stable source files (assuming a single backend
%  Prolog compiler is being used), uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(clean, off),
	set_logtalk_flag(reload, changed)
)).
%*/


%  To develop portable Logtalk applications uncomment the following lines
%  to help you catch possible non-portable built-in predicate calls, use
%  of non-standard flags or non-standard flag values, and missing predicate
%  directives:

/*
:- initialization((
	set_logtalk_flag(portability, warning),
	set_logtalk_flag(unknown_entities, warning),
	set_logtalk_flag(unknown_predicates, warning),
	set_logtalk_flag(missing_directives, warning)
)).
%*/


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
%*/


%  To fully support hot-patching of compiled code at runtime uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(complements, allow)
)).
%*/


%  To support adding new features (but not patching existing ones) to
%  compiled code at runtime uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(complements, restrict)
)).
%*/


%  To prevent using the <</2 debugging context-switching control construct
%  to bypass object encapsulation rules uncomment the following lines:

/*
:- initialization((
	set_logtalk_flag(context_switching_calls, deny)
)).
%*/


%  To lock your entities to prevent breaking encapsulation, uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(context_switching_calls, deny),
	set_logtalk_flag(dynamic_declarations, deny),
	set_logtalk_flag(source_data, off)
)).
%*/


%  To suppress some or all startup messages, uncomment the following lines:
%  (you can use in alternative the `report` flag but this flag also affects
%  source file compilation and loading reports)

/*
:- category(my_terse_logtalk_startup_settings).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% uncomment the next line to suppress the startup banner
	%logtalk::message_hook(banner, banner, core, _).

	% uncomment the next line to suppress the startup printing of default flags
	%logtalk::message_hook(default_flags, comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of the loaded settings file
	%logtalk::message_hook(loaded_settings_file(_), comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of settings information (except warnings and errors)
	%logtalk::message_hook(_, comment(settings), core, _).

	% uncomment the next line to suppress the startup printing on the help tool
	%logtalk::message_hook(_, comment(help), core, _).

:- end_category.
%*/


%  To print all otherwise silent compiler messages, uncomment the following
%  lines:

/*
:- category(my_verbose_logtalk_message_settings).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, silent, core, Tokens) :-
		logtalk::message_prefix_stream(comment, core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

	logtalk::message_hook(_Message, silent(Key), core, Tokens) :-
		logtalk::message_prefix_stream(comment(Key), core, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

:- end_category.
%*/
