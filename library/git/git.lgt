%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- object(git,
	implements(git_protocol)).

	:- info([
		version is 2:1:0,
		author is 'Paulo Moura',
		date is 2023-04-11,
		comment is 'Predicates for accessing a git project current branch and latest commit data.'
	]).

	:- uses(os, [
		delete_file/1, internal_os_path/2, path_concat/3,
		pid/1, temporary_directory/1, shell/1
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- if(os::operating_system_type(windows)).

		branch(Directory, Branch) :-
			temporary_file(Temporary),
			internal_os_path(Temporary, NativeTemporary),
			atomic_list_concat(['git -C ', Directory, ' rev-parse --abbrev-ref HEAD 2>nul > ', NativeTemporary], Command),
			(	shell(Command) ->
				data_clean(Temporary, Branch),
				Branch \== ''
			;	delete_file(Temporary),
				fail
			).

		commit_log(Directory, Format, Output) :-
			temporary_file(Temporary),
			internal_os_path(Temporary, NativeTemporary),
			atomic_list_concat(['git -C ', Directory, ' log --oneline -n 1 --pretty=format:', Format, ' 2>nul > ', NativeTemporary], Command),
			(	shell(Command) ->
				data_raw(Temporary, Output)
			;	delete_file(Temporary),
				fail
			).

	:- else.

		branch(Directory, Branch) :-
			temporary_file(Temporary),
			atomic_list_concat(['git -C ', Directory, ' rev-parse --abbrev-ref HEAD 2>/dev/null > ', Temporary], Command),
			(	shell(Command) ->
				data_clean(Temporary, Branch),
				Branch \== ''
			;	delete_file(Temporary),
				fail
			).

		commit_log(Directory, Format, Output) :-
			temporary_file(Temporary),
			atomic_list_concat(['git -C ', Directory, ' log --oneline -n 1 --pretty=format:"', Format, '" 2>/dev/null > ', Temporary], Command),
			(	shell(Command) ->
				data_raw(Temporary, Output)
			;	delete_file(Temporary),
				fail
			).

	:- endif.

	commit_author(Directory, Author) :-
		commit_log(Directory, '%an', Author).

	commit_date(Directory, Date) :-
		commit_log(Directory, '%aI', Date).

	commit_hash(Directory, Hash) :-
		commit_log(Directory, '%H', Hash).

	commit_hash_abbreviated(Directory, Hash) :-
		commit_log(Directory, '%h', Hash).

	commit_message(Directory, Message) :-
		commit_log(Directory, '%B', Message).

	% auxiliary predicates

	temporary_file(Temporary) :-
		pid(PID),
		atomic_list_concat([logtalk_git_data_access_, PID], Basename),
		temporary_directory(Directory),
		path_concat(Directory, Basename, Temporary).

	data_clean(File, Data) :-
		open(File, read, Stream),
		get_codes_clean(Stream, Codes),
		atom_codes(Data, Codes),
		close(Stream),
		delete_file(File).

	data_raw(File, Data) :-
		open(File, read, Stream),
		get_codes_raw(Stream, Codes),
		atom_codes(Data, Codes),
		close(Stream),
		delete_file(File).

	get_codes_clean(Stream, Codes) :-
		get_code(Stream, Code),
		(	Code =:= -1 ->
			Codes = []
		;	Code =:= 10 ->
			get_codes_clean(Stream, Codes)
		;	Code =:= 13 ->
			get_codes_clean(Stream, Codes)
		;	Codes = [Code| Rest],
			get_codes_clean(Stream, Rest)
		).

	get_codes_raw(Stream, Codes) :-
		get_code(Stream, Code),
		(	Code =:= -1 ->
			Codes = []
		;	Codes = [Code| Rest],
			get_codes_raw(Stream, Rest)
		).

:- end_object.
