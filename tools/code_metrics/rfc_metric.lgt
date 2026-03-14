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


:- object(rfc_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-13,
		comment is 'Response For a Class (RFC) metric. The score is the number of distinct predicates in the response set: the locally defined (non-auxiliary) predicates plus all distinct predicates they directly call or update. Protocols are not scored as they cannot define predicates. The score is represented by a non-negative integer.',
		remarks is [
			'Response set' - 'The response set RS(E) for an entity E is the union of its locally defined (non-auxiliary) predicates and all predicates directly called or updated by those predicates.',
			'Score interpretation' - 'Higher scores indicate entities with more potential execution paths in response to a message, which may increase testing effort.',
			'Aggregation' - 'When computing scores for files, directories, and libraries, the individual entity scores are summed.'
		]
	]).

	:- uses(list, [
		member/2, memberchk/2, length/2, subtract/3
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
		defined_predicates(Kind, Entity, Defined),
		external_callees(Kind, Entity, Defined, External),
		length(Defined, NM),
		length(External, NE),
		Score is NM + NE.

	defined_predicates(object, Entity, Defined) :-
		findall(
			Predicate,
			(	object_property(Entity, defines(Predicate, Properties)),
				\+ member(auxiliary, Properties)
			),
			Bag
		),
		sort(Bag, Defined).
	defined_predicates(category, Entity, Defined) :-
		findall(
			Predicate,
			(	category_property(Entity, defines(Predicate, Properties)),
				\+ member(auxiliary, Properties)
			),
			Bag
		),
		sort(Bag, Defined).

	% external_callees(+Kind, +Entity, +Defined, -External)
	% External = callees called or updated by predicates in Defined, not already in Defined
	external_callees(object, Entity, Defined, External) :-
		findall(
			NCallee,
			(	object_property(Entity, calls(Callee, Properties)),
				memberchk(caller(Caller), Properties),
				memberchk(Caller, Defined),
				normalize_callee(Callee, NCallee)
			),
			CalleeList
		),
		findall(
			Callee,
			(	object_property(Entity, updates(Callee, Properties)),
				memberchk(updater(Caller), Properties),
				memberchk(Caller, Defined)
			),
			UpdateList,
			CalleeList
		),
		sort(UpdateList, AllSorted),
		subtract(AllSorted, Defined, External).
	external_callees(category, Entity, Defined, External) :-
		findall(
			NCallee,
			(	category_property(Entity, calls(Callee, Properties)),
				memberchk(caller(Caller), Properties),
				memberchk(Caller, Defined),
				normalize_callee(Callee, NCallee)
			),
			CalleeList
		),
		findall(
			Callee,
			(	category_property(Entity, updates(Callee, Properties)),
				memberchk(updater(Caller), Properties),
				memberchk(Caller, Defined)
			),
			UpdateList,
			CalleeList
		),
		sort(UpdateList, AllSorted),
		subtract(AllSorted, Defined, External).

	% normalize_callee(+Callee, -Normalized)
	% Replace an unbound receiver in a message send with the atom '_'
	% so that all calls to an unknown target for the same predicate
	% collapse into one entry after sorting.
	normalize_callee(Receiver::Indicator, '_'::Indicator) :-
		\+ ground(Receiver),
		!.
	normalize_callee(Callee, Callee).

	process_entity(Kind, Entity) :-
		entity_score_(Kind, Entity, Score),
		print_message(information, code_metrics, rfc(Score)).

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
		print_message(information, code_metrics, rfc(Score)).

	directory_score(Directory, Score, Options) :-
		findall(FileScore, directory_file_score(Directory, _, FileScore, Options), FileScores),
		sum(FileScores, Score).

	process_directory(Directory, Options) :-
		directory_score(Directory, Score, Options),
		print_message(information, code_metrics, rfc(Score)).

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
		print_message(information, code_metrics, rfc(Score)).

	library_score(Library, Score, Options) :-
		expand_library_path(Library, Directory),
		directory_score(Directory, Score, Options).

	process_library(Library, Options) :-
		library_score(Library, Score, Options),
		print_message(information, code_metrics, rfc(Score)).

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
		print_message(information, code_metrics, rfc(Score)).

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
		print_message(information, code_metrics, rfc(Score)).

	format_entity_score(_Entity, Score) -->
		['RFC: ~w'-[Score], nl].

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(rfc(Score), code_metrics) -->
		['RFC: ~w'-[Score], nl].

:- end_object.
