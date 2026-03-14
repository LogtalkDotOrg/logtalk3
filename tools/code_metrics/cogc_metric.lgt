%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(cogc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-14,
		comment is 'Cognitive complexity metric (approximation). For each non-auxiliary predicate, the score contribution is the number of extra clauses (i.e. number of clauses minus one, for multi-clause branching) plus one if the predicate is directly recursive. The entity score is the sum of all predicate contributions. Protocols are not scored as they cannot define predicates.',
		remarks is [
			'Limitations' - 'The reflection API does not expose control constructs such as cuts, conditionals, or disjunctions. This metric approximates cognitive complexity using only clause-level branching and direct recursion.',
			'Branching' - 'A predicate with N clauses contributes N-1 to the score, representing the N-1 extra choices a reader must track.',
			'Recursion' - 'A predicate that directly calls itself (within the same entity) contributes an additional 1 to the score.',
			'Score interpretation' - 'The score is represented by a non-negative integer. Higher scores indicate predicates or entities that are harder to understand due to more branching choices or self-recursion. A score of 0 means all predicates are single-clause and non-recursive.',
			'Aggregation' - 'File, directory, and library scores are computed by summing the individual entity scores.'
		]
	]).

	:- uses(list, [
		member/2, memberchk/2
	]).

	:- uses(numberlist, [
		sum/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, loaded_file_property/2, print_message/3
	]).

	entity_score(Entity, Score) :-
		(	var(Entity) ->
			^^current_entity(Entity)
		;	true
		),
		^^entity_kind(Entity, Kind),
		Kind \== protocol,
		entity_score_(Kind, Entity, Score).

	entity_score_(Kind, Entity, Score) :-
		findall(
			Contribution,
			predicate_contribution(Kind, Entity, Contribution),
			Contributions
		),
		sum(Contributions, Score).

	predicate_contribution(object, Entity, Contribution) :-
		object_property(Entity, defines(_/_, Properties)),
		\+ member(auxiliary, Properties),
		memberchk(number_of_clauses(N), Properties),
		Branching is N - 1,
		(	member(recursive, Properties) ->
			Recursion = 1
		;	Recursion = 0
		),
		Contribution is Branching + Recursion.
	predicate_contribution(category, Entity, Contribution) :-
		category_property(Entity, defines(_/_, Properties)),
		\+ member(auxiliary, Properties),
		memberchk(number_of_clauses(N), Properties),
		(	member(recursive, Properties) ->
			Recursion = 1
		;	Recursion = 0
		),
		Branching is N - 1,
		Contribution is Branching + Recursion.

	process_entity(Kind, Entity) :-
		entity_score_(Kind, Entity, Score),
		print_message(information, code_metrics, cogc(Score)).

	file_score(File, Score, Options) :-
		^^option(exclude_entities(ExcludedEntities), Options),
		findall(
			EntityScore,
			(	loaded_file_property(File, object(Object)),
				\+ member(Object, ExcludedEntities),
				entity_score_(object, Object, EntityScore)
			;	loaded_file_property(File, category(Category)),
				\+ member(Category, ExcludedEntities),
				entity_score_(category, Category, EntityScore)
			),
			EntityScores
		),
		sum(EntityScores, Score).

	process_file(File, Options) :-
		file_score(File, Score, Options),
		print_message(information, code_metrics, cogc(Score)).

	directory_score(Directory, Score, Options) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore, Options), FileScores),
		sum(FileScores, Score).

	process_directory(Directory, Options) :-
		directory_score(Directory, Score, Options),
		print_message(information, code_metrics, cogc(Score)).

	directory_file_score(Directory, File, Score, Options) :-
		^^option(exclude_files(ExcludedFiles), Options),
		loaded_file_property(File, directory(Directory)),
		loaded_file_property(File, basename(Basename)),
		^^not_excluded_file(ExcludedFiles, File, Basename),
		file_score(File, Score, Options).

	rdirectory_score(Directory, Score, Options) :-
		^^option(exclude_directories(ExcludedDirectories), Options),
		directory_score(Directory, DirectoryScore, Options),
		(	setof(
				SubDirectory,
				^^sub_directory(Directory, SubDirectory),
				SubDirectories
			) ->
			true
		;	SubDirectories = []
		),
		findall(
			SubDirectoryScore,
			(	member(SubDirectory, SubDirectories),
				\+ (
					member(ExcludedDirectory, ExcludedDirectories),
					sub_atom(SubDirectory, 0, _, _, ExcludedDirectory)
				),
				directory_file_score(SubDirectory, _, SubDirectoryScore, Options)
			),
			SubDirectoryScores
		),
		sum([DirectoryScore| SubDirectoryScores], Score).

	process_rdirectory(Directory, Options) :-
		rdirectory_score(Directory, Score, Options),
		print_message(information, code_metrics, cogc(Score)).

	library_score(Library, Score, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score, Options).

	process_library(Library, Options) :-
		library_score(Library, Score, Options),
		print_message(information, code_metrics, cogc(Score)).

	rlibrary_score(Library, Score, Options) :-
		^^option(exclude_libraries(ExcludedLibraries), Options),
		library_score(Library, LibraryScore, Options),
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
			(	member(SubLibrary, SubLibraries),
				\+ member(SubLibrary, ExcludedLibraries),
				library_score(SubLibrary, SubLibraryScore, Options)
			),
			SubLibraryScores
		),
		sum([LibraryScore| SubLibraryScores], Score).

	process_rlibrary(Library, Options) :-
		rlibrary_score(Library, Score, Options),
		print_message(information, code_metrics, cogc(Score)).

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
		print_message(information, code_metrics, cogc(Score)).

	format_entity_score(_Entity, Score) -->
		['Cognitive complexity: ~w'-[Score], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(cogc(Score), code_metrics) -->
		['Cognitive complexity: ~w'-[Score], nl].

:- end_object.
