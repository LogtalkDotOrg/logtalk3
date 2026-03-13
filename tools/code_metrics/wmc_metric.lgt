%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2017-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(wmc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Weighted Methods per Class (WMC) metric. Uses unit weights, i.e., the score is the number of locally defined (non-auxiliary) predicates. Protocols are not scored as they cannot define predicates. The score is represented by a non-negative integer.',
		remarks is [
			'Unit weights' - 'Each predicate is assigned a weight of 1. A cyclomatic-complexity-weighted variant is not provided as the Logtalk reflection API does not expose intra-clause branching structure (if-then-else, disjunction), which would produce the same approximations as the ``cc_metric``.',
			'Interpretation' - 'Higher scores indicate an entity with more responsibilities that may benefit from being split.',
			'Aggregation' - 'When computing scores for files, directories, and libraries, the individual entity scores are summed.'
		]
	]).

	:- uses(list, [
		member/2, length/2
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
			Predicate,
			defined_predicate(Kind, Entity, Predicate),
			Bag
		),
		sort(Bag, Sorted),
		length(Sorted, Score).

	defined_predicate(object, Entity, Predicate) :-
		object_property(Entity, defines(Predicate, Properties)),
		\+ member(auxiliary, Properties),
		\+ member(number_of_clauses(0), Properties).
	defined_predicate(category, Entity, Predicate) :-
		category_property(Entity, defines(Predicate, Properties)),
		\+ member(auxiliary, Properties),
		\+ member(number_of_clauses(0), Properties).

	process_entity(Kind, Entity) :-
		entity_score_(Kind, Entity, Score),
		print_message(information, code_metrics, wmc(Score)).

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
		print_message(information, code_metrics, wmc(Score)).

	directory_score(Directory, Score, Options) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore, Options), FileScores),
		sum(FileScores, Score).

	process_directory(Directory, Options) :-
		directory_score(Directory, Score, Options),
		print_message(information, code_metrics, wmc(Score)).

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
		print_message(information, code_metrics, wmc(Score)).

	library_score(Library, Score, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score, Options).

	process_library(Library, Options) :-
		library_score(Library, Score, Options),
		print_message(information, code_metrics, wmc(Score)).

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
		print_message(information, code_metrics, wmc(Score)).

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
		print_message(information, code_metrics, wmc(Score)).

	format_entity_score(_Entity, Score) -->
		['WMC: ~w'-[Score], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(wmc(Score), code_metrics) -->
		['WMC: ~w'-[Score], nl].

:- end_object.
