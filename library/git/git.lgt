%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-11-12,
		comment is 'Predicates for accessing a git project current branch and latest commit data.'
	]).

	:- uses(os, [
		shell/1, temporary_directory/1, pid/1, delete_file/1, path_concat/3
	]).

	branch(Directory, Branch) :-
		temporary_file(Temporary),
		atom_concat('cd ', Directory, Command0),
		atom_concat(Command0, ' && git rev-parse --abbrev-ref HEAD | tr -d \'\\n\' | tr -d \'\\r\' > ', Command1),
		atom_concat(Command1, Temporary, Command),
		shell(Command),
		data(Temporary, Branch).

	commit_author(Directory, Hash) :-
		commit_log(Directory, '%an', Hash).

	commit_date(Directory, Hash) :-
		commit_log(Directory, '%aI', Hash).

	commit_hash(Directory, Hash) :-
		commit_log(Directory, '%H', Hash).

	commit_hash_abbreviated(Directory, Hash) :-
		commit_log(Directory, '%h', Hash).

	commit_message(Directory, Message) :-
		commit_log(Directory, '%B', Message).

	commit_log(Directory, Format, Output) :-
		temporary_file(Temporary),
		atom_concat('cd ', Directory, Command0),
		atom_concat(Command0, ' && git log --oneline -n 1 --pretty=format:"', Command1),
		atom_concat(Command1, Format, Command2),
		atom_concat(Command2, '" > ', Command3),
		atom_concat(Command3, Temporary, Command),
		shell(Command),
		data(Temporary, Output).

	% auxiliary predicates

	temporary_file(Temporary) :-
		pid(PID),
		number_codes(PID, Codes),
		atom_codes(PIDAtom, Codes),
		atom_concat(logtalk_git_data_access_, PIDAtom, Basename),
		temporary_directory(Directory),
		path_concat(Directory, Basename, Temporary).

	data(File, Data) :-
		open(File, read, Stream),
		get_codes(Stream, Codes),
		atom_codes(Data, Codes),
		close(Stream),
		delete_file(File).

	get_codes(Stream, Codes) :-
		get_code(Stream, Code),
		(	Code =:= -1 ->
			Codes = []
		;	Codes = [Code| Rest],
			get_codes(Stream, Rest)
		).

:- end_object.
