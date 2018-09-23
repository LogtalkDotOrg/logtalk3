%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- multifile(logtalk_library_path/2).
:- dynamic(logtalk_library_path/2).

:- if(current_prolog_flag(dialect, swi)).

	% usage: swilgt -f parallel_logtalk_processes_setup.pl

	:- use_module(library(uuid), []).

	logtalk_library_path(scratch_directory, Directory) :-
    	uuid:uuid(UUID),
    	atom_concat('/tmp/', UUID, Directory).

:- elif(current_prolog_flag(dialect, gprolog)).

	% usage: gplgt --init-goal "consult('parallel_logtalk_processes_setup.pl')"

	logtalk_library_path(scratch_directory, Directory) :-
		temporary_name(lgtXXXXXX, Name),
		decompose_file_name(Name, _, Prefix, _),
		atom_concat('/tmp/', Prefix, Directory),
		make_directory(Directory).

:- elif(current_prolog_flag(dialect, yap)).

	% usage: yaplgt -f parallel_logtalk_processes_setup.pl

	:- use_module(library(system)).

	logtalk_library_path(scratch_directory, Directory) :-
		tmpnam(Directory),
		delete_file(Directory),
		make_directory(Directory).

:- elif(current_prolog_flag(dialect, eclipse)).

	% usage: cat parallel_logtalk_processes_setup.pl "$LOGTALKHOME/integration/logtalk_eclipse.pl" > combined.pl && eclipse  -L iso -t user -f combined.pl

	logtalk_library_path(scratch_directory, Directory) :-
		get_flag(tmp_dir, TMP_DIR0),
		atom_string(TMP_DIR, TMP_DIR0),
		get_flag(unix_time, Time),
		number_codes(Time, TimeCodes),
		atom_codes(TimeAtom, TimeCodes),
		atom_concat(TMP_DIR, logtalk, Directory0),
		atom_concat(Directory0, TimeAtom, Directory),
		mkdir(Directory).

:- elif(current_prolog_flag(dialect, sicstus)).

	% usage: cat parallel_logtalk_processes_setup.pl "$LOGTALKHOME/integration/logtalk_sicstus.pl" > combined.pl && sicstus -l combined.pl

	:- use_module(library(system), [environ/2, now/1]).
	:- use_module(library(file_systems), [make_directory/1]).

	logtalk_library_path(scratch_directory, Directory) :-
		environ('SP_TEMP_DIR', SP_TEMP_DIR),
		now(Time),
		number_codes(Time, TimeCodes),
		atom_codes(TimeAtom, TimeCodes),
		atom_concat(SP_TEMP_DIR, logtalk, Directory0),
		atom_concat(Directory0, TimeAtom, Directory),
		make_directory(Directory).

:- endif.
