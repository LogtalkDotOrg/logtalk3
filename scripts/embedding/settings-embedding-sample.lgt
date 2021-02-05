%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Sample embedding settings file
%  Last updated on September 26, 2018
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


%  This is a sample embedding settings file for use when embedding Logtalk
%  applications.


%  To define a "library" alias for your application while making it
%  relocatable, edit and uncomment the following lines:

/*
:- initialization((
	logtalk_load_context(directory, Directory),
	assertz(logtalk_library_path(my_application, Directory))
)).
%*/


%  To define other "library" aliases for your application, edit and uncomment
%  the following lines (the library path must end with a slash character):

/*
:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

logtalk_library_path(my_application_core, my_application('core/')).
logtalk_library_path(my_application_libraries, my_application('libraries/')).
%*/


%  To make Logtalk silent for application deployment, uncomment the
%  following lines:

/*
:- initialization((
	%set_logtalk_flag(prolog_loader, [silent(true)]),   % for SWI-Prolog and YAP
	%set_stream(log_output, null),                      % for ECLiPSe
	%set_prolog_flag(informational, off),               % SICStus Prolog
	set_logtalk_flag(report, off)
)).
%*/


%  Common settings for embedded applications (note that these default
%  values can always be overridden in source files and entities):

:- initialization((
	% be silent except for warnings (set to "off" for final deliverable)
	set_logtalk_flag(report, warnings),
	% prevent reloading of embedded code
	set_logtalk_flag(reload, skip),
	% optimize performance
	set_logtalk_flag(optimize, on),
	% do not save source file data
	set_logtalk_flag(source_data, off),
	% lock your entities to prevent breaking encapsulation
	set_logtalk_flag(events, deny),
	set_logtalk_flag(complements, deny),
	set_logtalk_flag(dynamic_declarations, deny),
	set_logtalk_flag(context_switching_calls, deny)
)).


%  To fully support hot-patching of compiled code at runtime uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(complements, allow)
)).
%*/


%  To support adding new features to compiled code at runtime uncomment the
%  following lines:

/*
:- initialization((
	set_logtalk_flag(complements, restrict)
)).
%*/


%  Suppress most startup messages:

:- category(my_terse_logtalk_startup_settings).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% uncomment the next line to suppress the startup banner
	%logtalk::message_hook(banner, banner, core, _).

	% uncomment the next line to suppress the startup printing of default flags
	logtalk::message_hook(default_flags, comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of the loaded settings file
	logtalk::message_hook(loaded_settings_file(_), comment(settings), core, _).

	% uncomment the next line to suppress the startup printing of settings information (except warnings and errors)
	logtalk::message_hook(_, comment(settings), core, _).

	% uncomment the next line to suppress the startup printing on the help tool
	logtalk::message_hook(_, comment(help), core, _).

:- end_category.
