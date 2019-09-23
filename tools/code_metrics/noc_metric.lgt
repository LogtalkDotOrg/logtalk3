%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(noc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0.11,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2019/09/23,
		comment is 'Number of entity clauses metric. The score is represented using the compound term ``number_of_clauses(Total, User)``.'
	]).

	entity_score(Entity, Score) :-
		^^current_entity(Entity),
		^^entity_kind(Entity, Kind),
		entity_score(Kind, Entity, Score).

	entity_score(object, Entity, number_of_clauses(Total, User)) :-
		object_property(Entity, number_of_clauses(Total)),
		object_property(Entity, number_of_user_clauses(User)).
	entity_score(category, Entity, number_of_clauses(Total, User)) :-
		category_property(Entity, number_of_clauses(Total)),
		category_property(Entity, number_of_user_clauses(User)).
	entity_score(protocol, _, number_of_clauses(0, 0)).

	process_entity(Kind, Entity) :-
		entity_score(Kind, Entity, Score),
		logtalk::print_message(information, code_metrics, Score).

	file_score(File, Score) :-
		findall(
			EntityScore,
			(	logtalk::loaded_file_property(File, object(Object)),
				entity_score(object, Object, EntityScore)
			;	logtalk::loaded_file_property(File, category(Category)),
				entity_score(category, Category, EntityScore)
			),
			EntityScores
		),
		sum_scores(EntityScores, Score).

	process_file(File) :-
		file_score(File, Score),
		logtalk::print_message(information, code_metrics, Score).

	directory_score(Directory, Score) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore), FileScores),
		sum_scores(FileScores, Score).

	process_directory(Directory) :-
		directory_score(Directory, Score),
		logtalk::print_message(information, code_metrics, Score).

	directory_file_score(Directory, File, Nocs) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(File, directory(DirectorySlash)),
		file_score(File, Nocs).

	rdirectory_score(Directory, Score) :-
		directory_score(Directory, DirectoryScore),
		(	setof(
				SubDirectory,
				^^sub_directory(Directory, SubDirectory),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		findall(
			FileScore,
			(	list::member(SubDirectory, SubDirectories),
				directory_file_score(SubDirectory, _, FileScore)
			),
			FileScores
		),
		sum_scores([DirectoryScore| FileScores], Score).

	process_rdirectory(Directory) :-
		rdirectory_score(Directory, Score),
		logtalk::print_message(information, code_metrics, Score).

	library_score(Library, Score) :-
		logtalk::expand_library_path(Library, Directory),
		directory_score(Directory, Score).

	process_library(Library) :-
		library_score(Library, Score),
		logtalk::print_message(information, code_metrics, Score).

	rlibrary_score(Library, Score) :-
		library_score(Library, LibraryScore),
		(	setof(
				SubLibrary,
				^^sub_library(Library, SubLibrary),
				SubLibraries
			) ->
			true
		;	SubLibraries = []
		),
		findall(
			SubLibraryScore,
			(	list::member(SubLibrary, SubLibraries),
				library_score(SubLibrary, SubLibraryScore)
			),
			SubLibraryScores
		),
		sum_scores([LibraryScore| SubLibraryScores], Score).

	process_rlibrary(Library) :-
		rlibrary_score(Library, Score),
		logtalk::print_message(information, code_metrics, Score).

	all_score(Score) :-
		findall(
			FileScore,
			(	logtalk::loaded_file(File),
				file_score(File, FileScore)
			),
			FileScores
		),
		sum_scores(FileScores, Score).

	process_all :-
		all_score(Score),
		logtalk::print_message(information, code_metrics, Score).

	entity_score(_Entity, number_of_clauses(Total,User)) -->
		['Number of Clauses: ~w'-[Total], nl],
		['Number of User Clauses: ~w'-[User], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(number_of_clauses(Total,User), code_metrics) -->
		['Number of Clauses: ~w'-[Total], nl],
		['Number of User Clauses: ~w'-[User], nl].

	sum_scores([], number_of_clauses(0,0)).
	sum_scores([number_of_clauses(Total0,User0)| Numbers], number_of_clauses(Total,User)) :-
		sum_scores(Numbers, Total0, Total, User0, User).

	sum_scores([], Total, Total, User, User).
	sum_scores([number_of_clauses(Total1,User1)| Numbers], Total0, Total, User0, User) :-
		Total2 is Total0 + Total1,
		User2 is User0 + User1,
		sum_scores(Numbers, Total2, Total, User2, User).

:- end_object.
