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


:- object(cc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2018/07/17,
		comment is 'Cyclomatic complexity metric. All defined predicates that are not called or updated are counted as graph connected components (the reasoning being that these predicates can be considered entry points). The score is represented by a non-negative integer.'
	]).

	:- uses(list, [length/2, member/2, memberchk/2]).
	:- uses(numberlist, [sum/2]).
	:- uses(logtalk, [expand_library_path/2, loaded_file/1, loaded_file_property/2, print_message/3]).

	entity_score(Entity, Score) :-
		^^current_entity(Entity),
		^^entity_kind(Entity, Kind),
		entity_score(Kind, Entity, Score).

	entity_score(protocol, _, 0) :-
		!.
	entity_score(Kind, Entity, Score) :-
		predicate_edges(Kind, Entity, Edges),
		predicate_nodes(Kind, Entity, Nodes),
		connected_components(Kind, Entity, Components),
		Score is Edges - Nodes + 2 * Components,
		writeq(Edges - Nodes + 2 * Components), nl.

	predicate_edges(object, Entity, Edges) :-
		findall(
			Caller-Callee,
			(	object_property(Entity, calls(Callee, CallProperties)),
				ground(Callee),
				memberchk(caller(Caller), CallProperties)
			),
			CallEdges
		),
		findall(
			Updater-Dynamic,
			(	object_property(Entity, updates(Dynamic, UpdateProperties)),
				ground(Dynamic),
				memberchk(updater(Updater), UpdateProperties)
			),
			AllEdges,
			CallEdges
		),
		sort(AllEdges, AllEdgesSorted),
		length(AllEdgesSorted, Edges).
	predicate_edges(category, Entity, Edges) :-
		findall(
			Caller-Callee,
			(	category_property(Entity, calls(Callee, CallProperties)),
				ground(Callee),
				memberchk(caller(Caller), CallProperties)
			),
			CallEdges
		),
		findall(
			Updater-Dynamic,
			(	category_property(Entity, updates(Dynamic, UpdateProperties)),
				ground(Dynamic),
				memberchk(updater(Updater), UpdateProperties)
			),
			AllEdges,
			CallEdges
		),
		sort(AllEdges, AllEdgesSorted),
		length(AllEdgesSorted, Edges).

	predicate_nodes(object, Entity, Nodes) :-
		findall(Predicate, object_property(Entity, defines(Predicate, _)), Predicates),
		length(Predicates, Nodes).
	predicate_nodes(category, Entity, Nodes) :-
		findall(Predicate, category_property(Entity, defines(Predicate, _)), Predicates),
		length(Predicates, Nodes).

	connected_components(object, Entity, Components) :-
		findall(
			Predicate,
			(	object_property(Entity, defines(Predicate, _)),
				\+ object_property(Entity, calls(Predicate, _)),
				\+ object_property(Entity, updates(Predicate, _))
			),
			Predicates
		),
		length(Predicates, Components).
	connected_components(category, Entity, Components) :-
		findall(
			Predicate,
			(	category_property(Entity, defines(Predicate, _)),
				\+ category_property(Entity, calls(Predicate, _)),
				\+ category_property(Entity, updates(Predicate, _))
			),
			Predicates
		),
		length(Predicates, Components).

	process_entity(Kind, Entity) :-
		entity_score(Kind, Entity, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	file_score(File, Score) :-
		findall(
			EntityScore,
			(	loaded_file_property(File, object(Object)),
				entity_score(object, Object, EntityScore)
			;	loaded_file_property(File, category(Category)),
				entity_score(category, Category, EntityScore)
			),
			EntityScores
		),
		sum(EntityScores, Score).

	process_file(File) :-
		file_score(File, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	directory_score(Directory, Score) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore), FileScores),
		sum(FileScores, Score).

	process_directory(Directory) :-
		directory_score(Directory, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	directory_file_score(Directory, File, Nocs) :-
		(	sub_atom(Directory, _, 1, 0, '/') ->
			DirectorySlash = Directory
		;	atom_concat(Directory, '/', DirectorySlash)
		),
		loaded_file_property(File, directory(DirectorySlash)),
		file_score(File, Nocs).

	rdirectory_score(Directory, Score) :-
		setof(
			SubDirectory,
			^^sub_directory(Directory, SubDirectory),
			SubDirectories
		),
		findall(
			DirectoryScore,
			(	member(SubDirectory, SubDirectories),
				directory_file_score(SubDirectory, _, DirectoryScore)
			),
			DirectoryScores
		),
		sum(DirectoryScores, Score).

	process_rdirectory(Directory) :-
		rdirectory_score(Directory, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	library_score(Library, Score) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score).

	process_library(Library) :-
		library_score(Library, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).		

	rlibrary_score(Library, Score) :-
		setof(
			Path,
			^^sub_library(Library, Path),
			Paths
		),
		findall(
			DirectoryScore,
			(	member(Path, Paths),
				directory_file_score(Path, _, DirectoryScore)
			),
			DirectoryScores
		),
		sum(DirectoryScores, Score).

	process_rlibrary(Library) :-
		rlibrary_score(Library, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).		

	all_score(Score) :-
		findall(
			FileScore,
			(	loaded_file(File),
				file_score(File, FileScore)
			),
			FileScores
		),
		sum(FileScores, Score).

	process_all :-
		all_score(Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	entity_score(_Entity, cyclomatic_complexity(Total)) -->
		['Cyclomatic complexity: ~w'-[Total], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(cyclomatic_complexity(Total), code_metrics) -->
		['Cyclomatic complexity: ~w'-[Total], nl].

:- end_object.
