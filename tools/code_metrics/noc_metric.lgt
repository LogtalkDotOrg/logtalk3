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
		version is 0.8,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2018/02/02,
		comment is 'Number of clauses defined for a predicate in an object or category.'
	]).

	entity_score(Entity, Nocs) :-
		^^current_entity(Entity),
		^^entity_kind(Entity, Kind),
		(	Kind == protocol ->
			Nocs = 0
		;	Kind == object ->
			object_property(Entity, number_of_clauses(Nocs))
		;	Kind == category ->
			category_property(Entity, number_of_clauses(Nocs))
		;	fail
		).

	process_entity(_, Entity) :-
		entity_score(Entity, Nocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(Nocs)).

	file_score(File, TotalNocs) :-
		findall(
			Nocs,
			(	logtalk::loaded_file_property(File, object(Object)),
				object_property(Object, number_of_clauses(Nocs))
			;	logtalk::loaded_file_property(File, category(Category)),
				category_property(Category, number_of_clauses(Nocs))
			),
			NocsList
		),
		numberlist::sum(NocsList, TotalNocs).

	process_file(File) :-
		file_score(File, TotalNocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(TotalNocs)).

	process_directory(Directory) :-
		findall(Nocs, directory_file_nocs(Directory, _, Nocs), NocsList),
		numberlist::sum(NocsList, TotalNocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(TotalNocs)).

	directory_file_nocs(Directory, File, Nocs) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		logtalk::loaded_file_property(File, directory(DirectorySlash)),
		file_score(File, Nocs).

	process_rdirectory(Directory) :-
		setof(
			SubDirectory,
			^^sub_directory(Directory, SubDirectory),
			SubDirectories
		),
		findall(
			Nocs,
			(	list::member(SubDirectory, SubDirectories),
				directory_file_nocs(SubDirectory, _, Nocs)
			),
			NocsList
		),
		numberlist::sum(NocsList, TotalNocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(TotalNocs)).

	process_rlibrary(Library) :-
		setof(
			Path,
			^^sub_library(Library, Path),
			Paths
		),
		findall(
			Nocs,
			(	list::member(Path, Paths),
				directory_file_nocs(Path, _, Nocs)
			),
			NocsList
		),
		numberlist::sum(NocsList, TotalNocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(TotalNocs)).		

	process_all :-
		findall(
			Nocs,
			(	logtalk::loaded_file(File),
				file_score(File, Nocs)
			),
			NocsList
		),
		numberlist::sum(NocsList, TotalNocs),
		logtalk::print_message(information, code_metrics, number_of_clauses(TotalNocs)).

	entity_score(_Entity, Nocs) -->
		['Number of Clauses: ~w'-[Nocs], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(number_of_clauses(Nocs), code_metrics) -->
		['Number of Clauses: ~w'-[Nocs], nl].

:- end_object.
