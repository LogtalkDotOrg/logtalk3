%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2024 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com>
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


:- object(cc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0:5:2,
		author is 'Paulo Moura',
		date is 2024-05-15,
		comment is 'Cyclomatic complexity metric. All defined predicates that are not called or updated are counted as graph connected components (the reasoning being that these predicates can be considered entry points). The score is represented by a non-negative integer.'
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, loaded_file_property/2, print_message/3
	]).

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
		Score is Edges - Nodes + 2 * Components.

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
		findall(Caller, (object_property(Entity, defines(Caller, DefinesProperties)), \+ member(auxiliary, DefinesProperties)), Bag0),
		findall(Callee, (object_property(Entity, calls(Callee, _)), ground(Callee)), Bag1, Bag0),
		findall(Dynamic, (object_property(Entity, updates(Dynamic, _)), ground(Dynamic)), Bag, Bag1),
		sort(Bag, Sorted),
		length(Sorted, Nodes).
	predicate_nodes(category, Entity, Nodes) :-
		findall(Caller, (category_property(Entity, defines(Caller, DefinesProperties)), \+ member(auxiliary, DefinesProperties)), Bag0),
		findall(Callee, (category_property(Entity, calls(Callee, _)), ground(Callee)), Bag1, Bag0),
		findall(Dynamic, (category_property(Entity, updates(Dynamic, _)), ground(Dynamic)), Bag, Bag1),
		sort(Bag, Sorted),
		length(Sorted, Nodes).

	connected_components(object, Entity, Components) :-
		findall(
			Predicate,
			(	object_property(Entity, provides(Predicate, Other, Properties)),
				\+ member(auxiliary, Properties),
				\+ (object_property(Entity, calls(Object::Predicate, _)), callable(Object), Object = Other),
				\+ (object_property(Entity, updates(Object::Predicate, _)), callable(Object), Object = Other)
			),
			Predicates0
		),
		findall(
			Predicate,
			(	object_property(Entity, defines(Predicate, Properties)),
				\+ member(auxiliary, Properties),
				\+ object_property(Entity, calls(Predicate, _)),
				\+ object_property(Entity, updates(Predicate, _))
			),
			Predicates,
			Predicates0
		),
		length(Predicates, Components).
	connected_components(category, Entity, Components) :-
		findall(
			Predicate,
			(	category_property(Entity, provides(Predicate, Other, Properties)),
				\+ member(auxiliary, Properties),
				\+ (category_property(Entity, calls(Object::Predicate, _)), callable(Object), Object = Other),
				\+ (category_property(Entity, updates(Object::Predicate, _)), callable(Object), Object = Other)
			),
			Predicates0
		),
		findall(
			Predicate,
			(	category_property(Entity, defines(Predicate, Properties)),
				\+ member(auxiliary, Properties),
				\+ category_property(Entity, calls(Predicate, _)),
				\+ category_property(Entity, updates(Predicate, _))
			),
			Predicates,
			Predicates0
		),
		length(Predicates, Components).

	process_entity(Kind, Entity) :-
		entity_score(Kind, Entity, Score),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	file_score(File, Score, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		findall(
			EntityScore,
			(	loaded_file_property(File, object(Object)),
				\+ member(Object, ExcludedEntities),
				entity_score(object, Object, EntityScore)
			;	loaded_file_property(File, category(Category)),
				\+ member(Category, ExcludedEntities),
				entity_score(category, Category, EntityScore)
			),
			EntityScores
		),
		sum(EntityScores, Score).

	process_file(File, Options) :-
		file_score(File, Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	directory_score(Directory, Score, Options) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore, Options), FileScores),
		sum(FileScores, Score).

	process_directory(Directory, Options) :-
		directory_score(Directory, Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	directory_file_score(Directory, File, Nocs, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		loaded_file_property(File, directory(Directory)),
		loaded_file_property(File, basename(Basename)),
		^^not_excluded_file(ExcludedFiles, File, Basename),
		file_score(File, Nocs, Options).

	rdirectory_score(Directory, Score, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		setof(
			SubDirectory,
			^^sub_directory(Directory, SubDirectory),
			SubDirectories
		),
		findall(
			DirectoryScore,
			(	member(SubDirectory, SubDirectories),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(SubDirectory, 0, _, _, ExcludedDirectory)
				),
				directory_file_score(SubDirectory, _, DirectoryScore, Options)
			),
			DirectoryScores
		),
		sum(DirectoryScores, Score).

	process_rdirectory(Directory, Options) :-
		rdirectory_score(Directory, Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	library_score(Library, Score, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score, Options).

	process_library(Library, Options) :-
		library_score(Library, Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	rlibrary_score(Library, Score, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		setof(
			Path,
			^^sub_library(Library, Path),
			Paths
		),
		findall(
			DirectoryScore,
			(	member(Path, Paths),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(Path, 0, _, _, ExcludedDirectory)
				),
				directory_file_score(Path, _, DirectoryScore, Options)
			),
			DirectoryScores
		),
		sum(DirectoryScores, Score).

	process_rlibrary(Library, Options) :-
		rlibrary_score(Library, Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	all_score(Score, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		findall(
			FileScore,
			(	loaded_file_property(File, basename(Basename)),
				^^not_excluded_file(ExcludedFiles, File, Basename),
				file_score(File, FileScore, Options)
			),
			FileScores
		),
		sum(FileScores, Score).

	process_all(Options) :-
		all_score(Score, Options),
		print_message(information, code_metrics, cyclomatic_complexity(Score)).

	format_entity_score(_Entity, Total) -->
		['Cyclomatic complexity: ~w'-[Total], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(cyclomatic_complexity(Total), code_metrics) -->
		['Cyclomatic complexity: ~w'-[Total], nl].

:- end_object.
