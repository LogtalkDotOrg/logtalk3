%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(mutation_testing,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-07,
		comment is 'Mutation testing tool.'
	]).

	:- public(library/1).
	:- mode(library(+atom), zero_or_one).
	:- info(library/1, [
		comment is 'Runs mutation testing for all loaded entities from a given library using default options.',
		argnames is ['Library']
	]).

	:- public(library/2).
	:- mode(library(+atom, +list(compound)), zero_or_one).
	:- info(library/2, [
		comment is 'Runs mutation testing for all loaded entities from a given library using the given options.',
		argnames is ['Library', 'Options']
	]).

	:- public(directory/1).
	:- mode(directory(+atom), zero_or_one).
	:- info(directory/1, [
		comment is 'Runs mutation testing for all loaded entities from a given directory using default options.',
		argnames is ['Directory']
	]).

	:- public(directory/2).
	:- mode(directory(+atom, +list(compound)), zero_or_one).
	:- info(directory/2, [
		comment is 'Runs mutation testing for all loaded entities from a given directory using the given options.',
		argnames is ['Directory', 'Options']
	]).

	:- public(entity/1).
	:- mode(entity(+entity_identifier), zero_or_one).
	:- info(entity/1, [
		comment is 'Runs mutation testing for a loaded entity using default options.',
		argnames is ['Entity']
	]).

	:- public(entity/2).
	:- mode(entity(+entity_identifier, +list(compound)), zero_or_one).
	:- info(entity/2, [
		comment is 'Runs mutation testing for a loaded entity using the given options.',
		argnames is ['Entity', 'Options']
	]).

	:- public(predicate/2).
	:- mode(predicate(+entity_identifier, +predicate_indicator), zero_or_one).
	:- info(predicate/2, [
		comment is 'Runs mutation testing for a loaded entity predicate using default options.',
		argnames is ['Entity', 'Predicate']
	]).

	:- public(predicate/3).
	:- mode(predicate(+entity_identifier, +predicate_indicator, +list(compound)), zero_or_one).
	:- info(predicate/3, [
		comment is 'Runs mutation testing for a loaded entity predicate using the given options.',
		argnames is ['Entity', 'Predicate', 'Options']
	]).

	:- public(report_entity/3).
	:- mode(report_entity(+entity_identifier, -compound, +list(compound)), zero_or_one).
	:- info(report_entity/3, [
		comment is 'Runs mutation testing for an entity and returns a structured report term.',
		argnames is ['Entity', 'Report', 'Options']
	]).

	:- public(report_predicate/4).
	:- mode(report_predicate(+entity_identifier, +predicate_indicator, -compound, +list(compound)), zero_or_one).
	:- info(report_predicate/4, [
		comment is 'Runs mutation testing for an entity predicate and returns a structured report term.',
		argnames is ['Entity', 'Predicate', 'Report', 'Options']
	]).

	:- public(report_library/3).
	:- mode(report_library(+atom, -compound, +list(compound)), zero_or_one).
	:- info(report_library/3, [
		comment is 'Runs mutation testing for loaded entities from a library and returns structured report terms.',
		argnames is ['Library', 'Report', 'Options']
	]).

	:- public(report_directory/3).
	:- mode(report_directory(+atom, -compound, +list(compound)), zero_or_one).
	:- info(report_directory/3, [
		comment is 'Runs mutation testing for loaded entities from a directory and returns structured report terms.',
		argnames is ['Directory', 'Report', 'Options']
	]).

	:- public(format_report/3).
	:- mode(format_report(+stream_or_alias, +atom, +compound), one).
	:- info(format_report/3, [
		comment is 'Formats a mutation testing report term to the given stream using the given format.',
		argnames is ['Stream', 'Format', 'Report']
	]).

	:- public(format_report/2).
	:- mode(format_report(+atom, +compound), one).
	:- info(format_report/2, [
		comment is 'Formats a mutation testing report term to the current output stream using the given format.',
		argnames is ['Format', 'Report']
	]).

	:- public(format_report/1).
	:- mode(format_report(+compound), one).
	:- info(format_report/1, [
		comment is 'Formats a mutation testing report term to the current output stream using the text format.',
		argnames is ['Report']
	]).

	:- public(entity_mutants/2).
	:- mode(entity_mutants(+entity_identifier, -list(compound)), zero_or_one).
	:- info(entity_mutants/2, [
		comment is 'Returns the deterministic list of mutants for an entity using default options.',
		argnames is ['Entity', 'Mutants']
	]).

	:- public(entity_mutants/3).
	:- mode(entity_mutants(+entity_identifier, -list(compound), +list(compound)), zero_or_one).
	:- info(entity_mutants/3, [
		comment is 'Returns the deterministic list of mutants for an entity using the given options.',
		argnames is ['Entity', 'Mutants', 'Options']
	]).

	:- public(predicate_mutants/3).
	:- mode(predicate_mutants(+entity_identifier, +predicate_indicator, -list(compound)), zero_or_one).
	:- info(predicate_mutants/3, [
		comment is 'Returns the deterministic list of mutants for an entity predicate using default options.',
		argnames is ['Entity', 'Predicate', 'Mutants']
	]).

	:- public(predicate_mutants/4).
	:- mode(predicate_mutants(+entity_identifier, +predicate_indicator, -list(compound), +list(compound)), zero_or_one).
	:- info(predicate_mutants/4, [
		comment is 'Returns the deterministic list of mutants for an entity predicate using the given options.',
		argnames is ['Entity', 'Predicate', 'Mutants', 'Options']
	]).

	:- public(library_mutants/2).
	:- mode(library_mutants(+atom, -list(compound)), zero_or_one).
	:- info(library_mutants/2, [
		comment is 'Returns the deterministic list of mutants for loaded entities from a given library using default options.',
		argnames is ['Library', 'Mutants']
	]).

	:- public(library_mutants/3).
	:- mode(library_mutants(+atom, -list(compound), +list(compound)), zero_or_one).
	:- info(library_mutants/3, [
		comment is 'Returns the deterministic list of mutants for loaded entities from a given library using the given options.',
		argnames is ['Library', 'Mutants', 'Options']
	]).

	:- public(directory_mutants/2).
	:- mode(directory_mutants(+atom, -list(compound)), zero_or_one).
	:- info(directory_mutants/2, [
		comment is 'Returns the deterministic list of mutants for loaded entities from a given directory using default options.',
		argnames is ['Directory', 'Mutants']
	]).

	:- public(directory_mutants/3).
	:- mode(directory_mutants(+atom, -list(compound), +list(compound)), zero_or_one).
	:- info(directory_mutants/3, [
		comment is 'Returns the deterministic list of mutants for loaded entities from a given directory using the given options.',
		argnames is ['Directory', 'Mutants', 'Options']
	]).

	:- private(probe_mutation_happened_/0).
	:- dynamic(probe_mutation_happened_/0).
	:- mode(probe_mutation_happened_, zero_or_one).
	:- info(probe_mutation_happened_/0, [
		comment is 'True iff a mutation happened.'
	]).

	:- private(probing_/0).
	:- dynamic(probing_/0).
	:- mode(probing_, zero_or_one).
	:- info(probing_/0, [
		comment is 'True iff we are currently probing for mutations (suppresses printing).'
	]).

	:- private(capturing_mutated_terms_/0).
	:- dynamic(capturing_mutated_terms_/0).
	:- mode(capturing_mutated_terms_, zero_or_one).
	:- info(capturing_mutated_terms_/0, [
		comment is 'True iff we are capturing original and mutated terms while formatting reports.'
	]).

	:- private(captured_mutated_terms_/5).
	:- dynamic(captured_mutated_terms_/5).
	:- mode(captured_mutated_terms_(-callable, -callable, -list, -atom, -compound), zero_or_one).
	:- info(captured_mutated_terms_/5, [
		comment is 'Captured original and mutated terms, variable names, and source location for one mutant.'
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, take/3
	]).

	:- uses(logtalk, [
		print_message/3
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	:- uses(os, [
		directory_exists/1, make_directory_path/1,
		path_concat/3, pid/1, shell/2, temporary_directory/1, wall_time/1,
		is_absolute_file_name/1
	]).

	:- uses(fast_random(xoshiro128pp), [
		get_seed/1, permutation/2, randomize/1, reset_seed/0, set_seed/1
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(mutated_term(_Mutator, _Original, _Mutation, _Variables, _File, _StartLine-_EndLine), _, mutation_testing, _) :-
		retractall(probe_mutation_happened_),
		assertz(probe_mutation_happened_),
		(   probing_ ->
			true
		;   false
		).

	logtalk::message_hook(mutated_term(_Mutator, Original, Mutation, Variables, File, StartLine-EndLine), _, mutation_testing, _) :-
		capturing_mutated_terms_,
		retractall(captured_mutated_terms_(_, _, _, _, _)),
		assertz(captured_mutated_terms_(Original, Mutation, Variables, File, StartLine-EndLine)).

	library(Library) :-
		library(Library, []).

	library(Library, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_library_entities(Library, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		reports_for_entities(Entities, Options, Reports),
		maybe_format_report(report(library(Library), Reports), Options),
		reports_passed(Reports, true, Passed),
		Passed == true,
		!.

	directory(Directory) :-
		directory(Directory, []).

	directory(Directory, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_directory_entities(Directory, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		reports_for_entities(Entities, Options, Reports),
		maybe_format_report(report(directory(Directory), Reports), Options),
		reports_passed(Reports, true, Passed),
		Passed == true,
		!.

	entity(Entity) :-
		entity(Entity, []).

	entity(Entity, UserOptions) :-
		report_entity(Entity, Report, UserOptions),
		maybe_format_report(Report, UserOptions),
		Report = report(_, summary(_, _, _, _, _, _, _, _, _, Passed), _),
		Passed == true,
		!.

	predicate(Entity, Predicate) :-
		predicate(Entity, Predicate, []).

	predicate(Entity, Predicate, UserOptions) :-
		report_predicate(Entity, Predicate, Report, UserOptions),
		maybe_format_report(Report, UserOptions),
		Report = report(_, summary(_, _, _, _, _, _, _, _, _, Passed), _),
		Passed == true,
		!.

	report_entity(Entity, Report, UserOptions) :-
		entity_exists(Entity),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		entity_predicates(Entity, Predicates),
		report_for_predicates(Entity, Predicates, Options, Report),
		!.

	report_predicate(Entity, Predicate, Report, UserOptions) :-
		entity_exists(Entity),
		entity_has_predicate(Entity, Predicate),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		report_for_predicates(Entity, [Predicate], Options, Report),
		!.

	report_library(Library, report(library(Library), Reports), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_library_entities(Library, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		reports_for_entities(Entities, Options, Reports),
		!.

	report_directory(Directory, report(directory(Directory), Reports), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_directory_entities(Directory, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		reports_for_entities(Entities, Options, Reports),
		!.

	entity_mutants(Entity, Mutants) :-
		entity_mutants(Entity, Mutants, []).

	entity_mutants(Entity, Mutants, UserOptions) :-
		entity_exists(Entity),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		entity_predicates(Entity, Predicates),
		mutants_for_predicates(Entity, Predicates, Options, Mutants).

	predicate_mutants(Entity, Predicate, Mutants) :-
		predicate_mutants(Entity, Predicate, Mutants, []).

	predicate_mutants(Entity, Predicate, Mutants, UserOptions) :-
		entity_exists(Entity),
		entity_has_predicate(Entity, Predicate),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		mutants_for_predicates(Entity, [Predicate], Options, Mutants).

	library_mutants(Library, Mutants) :-
		library_mutants(Library, Mutants, []).

	library_mutants(Library, Mutants, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_library_entities(Library, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		entities_mutants(Entities, Options, Mutants).

	directory_mutants(Directory, Mutants) :-
		directory_mutants(Directory, Mutants, []).

	directory_mutants(Directory, Mutants, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		loaded_directory_entities(Directory, Entities0),
		apply_entity_filters(Entities0, Options, Entities),
		entities_mutants(Entities, Options, Mutants).

	report_for_predicates(Entity, Predicates, Options, report(Entity, Summary, Results)) :-
		print_message(comment, mutation_testing, phase_mutations_discovery),
		mutants_for_predicates(Entity, Predicates, Options, Mutants0),
		apply_sample_mutants(Mutants0, Options, Mutants),
		print_message(comment, mutation_testing, phase_mutations_testing),
		run_mutants(Mutants, Options, 1, [], Results0),
		reverse(Results0, Results),
		summary(Results, Options, Summary).

	mutants_for_predicates(Entity, Predicates, Options, Mutants) :-
		mutators_for_options(Options, Mutators),
		build_mutants(Entity, Predicates, Mutators, Options, Mutants).

	entities_mutants([], _, []).
	entities_mutants([Entity| Entities], Options, Mutants) :-
		entity_predicates(Entity, Predicates),
		mutants_for_predicates(Entity, Predicates, Options, EntityMutants),
		append(EntityMutants, OtherMutants, Mutants),
		entities_mutants(Entities, Options, OtherMutants).

	reports_for_entities([], _, []).
	reports_for_entities([Entity| Entities], Options, [Report| Reports]) :-
		entity_predicates(Entity, Predicates),
		report_for_predicates(Entity, Predicates, Options, Report),
		reports_for_entities(Entities, Options, Reports).

	run_entities([], _, Passed, Passed).
	run_entities([Entity| Entities], Options, Passed0, Passed) :-
		report_entity(Entity, Report, Options),
		maybe_format_report(Report, Options),
		(   Report = report(_, summary(_, _, _, _, _, _, _, _, _, true), _) ->
			Passed1 = Passed0
		;   Passed1 = false
		),
		run_entities(Entities, Options, Passed1, Passed).

	reports_passed([], Passed, Passed).
	reports_passed([report(_, summary(_, _, _, _, _, _, _, _, _, true), _)| Reports], Passed0, Passed) :-
		reports_passed(Reports, Passed0, Passed).
	reports_passed([report(_, summary(_, _, _, _, _, _, _, _, _, false), _)| Reports], _Passed0, Passed) :-
		reports_passed(Reports, false, Passed).

	maybe_format_report(Report, UserOptions) :-
		^^merge_options(UserOptions, Options),
		^^option(format(Format), Options),
		(   Format == none ->
			true
		;   format_report(Format, Report),
			report_file_path(Report, Options, File),
			write_report_file(File, Format, Report)
		).

	report_file_path(Report, Options, File) :-
		^^option(format(Format), Options),
		report_extension(Format, Extension),
		^^option(report_file_name(FileName), Options),
		base_report_file_path(Report, Options, FileName, BasePath),
		with_extension(BasePath, Extension, File).

	base_report_file_path(_Report, _Options, FileName, FileName) :-
		is_absolute_file_name(FileName),
		!.
	base_report_file_path(Report, Options, FileName, File) :-
		report_tester_directory(Report, Options, Directory),
		path_concat(Directory, FileName, File).

	report_tester_directory(Report, Options, Directory) :-
		report_source_file(Report, SourceFile),
		!,
		tester_directory(SourceFile, Directory, Options).
	report_tester_directory(_Report, Options, Directory) :-
		(   ^^option(tester_directory(Directory), Options),
			os::directory_exists(Directory) ->
			true
		;   logtalk::expand_library_path(startup, Directory)
		).

	report_source_file(report(Entity, _Summary, _Results), SourceFile) :-
		entity_file(Entity, SourceFile).
	report_source_file(report(_Container, [Report| _]), SourceFile) :-
		report_source_file(Report, SourceFile).

	report_extension(text, '.txt').
	report_extension(json, '.json').

	with_extension(Path, Extension, File) :-
		os::decompose_file_name(Path, Directory, Name, _),
		atom_concat(Name, Extension, FileName),
		path_concat(Directory, FileName, File).

	write_report_file(File, Format, Report) :-
		os::decompose_file_name(File, Directory, _Name, _Extension),
		make_directory_path(Directory),
		open(File, write, Stream),
		format_report(Stream, Format, Report),
		close(Stream).

	loaded_entities(Entities) :-
		findall(Entity, (current_object(Entity), atom(Entity)), Objects),
		findall(Entity, (current_category(Entity), atom(Entity)), Categories),
		append(Objects, Categories, Unsorted),
		sort(Unsorted, Entities).

	loaded_library_entities(Library, Entities) :-
		(   logtalk::loaded_file_property(_, library(Library)) ->
			loaded_entities(LoadedEntities),
			include_library_entities(LoadedEntities, Library, Entities)
		;   print_message(error, mutation_testing, unknown(library, Library)),
			fail
		).

	loaded_directory_entities(Directory, Entities) :-
		normalize_directory(Directory, NormalizedDirectory),
		(   logtalk::loaded_file_property(_, directory(LoadedDirectory)),
			directory_matches(LoadedDirectory, NormalizedDirectory) ->
			loaded_entities(LoadedEntities),
			include_directory_entities(LoadedEntities, NormalizedDirectory, Entities)
		;   print_message(error, mutation_testing, unknown(directory, Directory)),
			fail
		).

	include_library_entities([], _, []).
	include_library_entities([Entity| Entities], Library, [Entity| Included]) :-
		entity_in_library(Entity, Library),
		!,
		include_library_entities(Entities, Library, Included).
	include_library_entities([_| Entities], Library, Included) :-
		include_library_entities(Entities, Library, Included).

	include_directory_entities([], _, []).
	include_directory_entities([Entity| Entities], Directory, [Entity| Included]) :-
		entity_in_directory(Entity, Directory),
		!,
		include_directory_entities(Entities, Directory, Included).
	include_directory_entities([_| Entities], Directory, Included) :-
		include_directory_entities(Entities, Directory, Included).

	entity_in_library(Entity, Library) :-
		entity_file(Entity, File),
		logtalk::loaded_file_property(File, library(Library)).

	entity_in_directory(Entity, Directory) :-
		entity_file(Entity, File),
		logtalk::loaded_file_property(File, directory(LoadedDirectory)),
		directory_matches(LoadedDirectory, Directory).

	normalize_directory(Directory, NormalizedDirectory) :-
		(   atom_concat(Directory0, '/', Directory) ->
			NormalizedDirectory = Directory0
		;   NormalizedDirectory = Directory
		).

	directory_matches(Directory1, Directory2) :-
		normalize_directory(Directory1, NormalizedDirectory1),
		normalize_directory(Directory2, NormalizedDirectory2),
		NormalizedDirectory1 == NormalizedDirectory2.

	entity_file(Entity, File) :-
		object_property(Entity, file(File)).
	entity_file(Entity, File) :-
		category_property(Entity, file(File)).
	entity_file(Entity, File) :-
		protocol_property(Entity, file(File)).

	apply_entity_filters(Entities0, Options, Entities) :-
		^^option(include_entities(IncludedEntities), Options),
		^^option(exclude_entities(ExcludedEntities), Options),
		(   IncludedEntities == [] ->
			Entities1 = Entities0
		;   include_list(Entities0, IncludedEntities, Entities1)
		),
		exclude_list(Entities1, ExcludedEntities, Entities).

	include_list([], _, []).
	include_list([Entity| Entities], IncludedEntities, [Entity| Included]) :-
		member(Entity, IncludedEntities),
		!,
		include_list(Entities, IncludedEntities, Included).
	include_list([_| Entities], IncludedEntities, Included) :-
		include_list(Entities, IncludedEntities, Included).

	exclude_list([], _, []).
	exclude_list([Entity| Entities], ExcludedEntities, Excluded) :-
		member(Entity, ExcludedEntities),
		!,
		exclude_list(Entities, ExcludedEntities, Excluded).
	exclude_list([Entity| Entities], ExcludedEntities, [Entity| Excluded]) :-
		exclude_list(Entities, ExcludedEntities, Excluded).

	entity_exists(Entity) :-
		(   current_object(Entity)
		;   current_category(Entity)
		),
		!.
	entity_exists(Entity) :-
		print_message(error, mutation_testing, unknown(entity, Entity)),
		fail.

	entity_has_predicate(Entity, Predicate) :-
		entity_predicates(Entity, Predicates),
		member(Predicate, Predicates),
		!.
	entity_has_predicate(Entity, Predicate) :-
		print_message(error, mutation_testing, unknown(predicate, Entity, Predicate)),
		fail.

	entity_predicates(Entity, Predicates) :-
		(   setof(Predicate, entity_predicate(Entity, Predicate), Predicates) ->
			true
		;   Predicates = []
		).

	entity_predicate(Entity, Predicate) :-
		entity_defines(Entity, Predicate0, Properties),
		\+ member(auxiliary, Properties),
		(   member(non_terminal(NonTerminal), Properties) ->
			Predicate = NonTerminal
		;   Predicate = Predicate0
		).

	entity_defines(Entity, Predicate, Properties) :-
		current_object(Entity),
		object_property(Entity, defines(Predicate, Properties)).
	entity_defines(Entity, Predicate, Properties) :-
		current_category(Entity),
		category_property(Entity, defines(Predicate, Properties)).

	mutators_for_options(Options, Mutators) :-
		^^option(mutators(SelectedMutators), Options),
		(   SelectedMutators == [] ->
			discover_mutators(DiscoveredMutators),
			^^option(max_mutators(MaxMutators), Options),
			select_mutators(MaxMutators, DiscoveredMutators, Mutators)
		;   Mutators = SelectedMutators
		).

	discover_mutators(Mutators) :-
		(   setof(Mutator, discovered_mutator(Mutator), Mutators) ->
			true
		;   Mutators = []
		).

	discovered_mutator(Name) :-
		conforms_to_protocol(Mutator, mutator_protocol),
		current_object(Mutator),
		functor(Mutator, Name, 4).

	select_mutators(all, Mutators, Mutators) :-
		!.
	select_mutators(MaxMutators, Mutators, SelectedMutators) :-
		take(MaxMutators, Mutators, SelectedMutators).

	build_mutants(Entity, Predicates, Mutators, Options, Mutants) :-
		^^option(max_mutations_per_mutator(Limit), Options),
		build_mutants(Predicates, Entity, Mutators, Limit, [], Mutants0),
		reverse(Mutants0, Mutants).

	build_mutants([], _, _, _, Mutants, Mutants).
	build_mutants([Predicate| Predicates], Entity, Mutators, Limit, Mutants0, Mutants) :-
		build_predicate_mutants(Mutators, Entity, Predicate, Limit, Mutants0, Mutants1),
		build_mutants(Predicates, Entity, Mutators, Limit, Mutants1, Mutants).

	build_predicate_mutants([], _, _, _, Mutants, Mutants).
	build_predicate_mutants([Mutator| Mutators], Entity, Predicate, Limit, Mutants0, Mutants) :-
		build_occurrence_mutants(Limit, 1, Entity, Predicate, Mutator, Mutants0, Mutants1, 0, _Generated),
		build_predicate_mutants(Mutators, Entity, Predicate, Limit, Mutants1, Mutants).

	build_occurrence_mutants(0, _Occurrence, _Entity, _Predicate, _Mutator, Mutants, Mutants, Generated, Generated) :-
		!.
	build_occurrence_mutants(Remaining, Occurrence, Entity, Predicate, Mutator, Mutants0, Mutants, Generated0, Generated) :-
		(   probe_mutation_occurrence(Entity, Predicate, Mutator, Occurrence) ->
			Mutants1 = [mutant(Entity, Predicate, Mutator, Occurrence)| Mutants0],
			NextOccurrence is Occurrence + 1,
			decrement_remaining(Remaining, NextRemaining),
			Generated1 is Generated0 + 1,
			build_occurrence_mutants(NextRemaining, NextOccurrence, Entity, Predicate, Mutator, Mutants1, Mutants, Generated1, Generated)
		;   Mutants = Mutants0,
			Generated = Generated0
		).

	decrement_remaining(all, all) :-
		!.
	decrement_remaining(Remaining, NextRemaining) :-
		NextRemaining is max(0, Remaining - 1).

	probe_mutation_occurrence(Entity, Predicate, Mutator, Occurrence) :-
		entity_file(Entity, File),
		retractall(probe_mutation_happened_),
		assertz(probing_),
		mutator_hook(Mutator, Entity, Predicate, Occurrence, true, Hook),
		prepare_mutator_hook(Hook),
		load_options(LoadOptions),
		revert_options(RevertOptions),
		(   catch(logtalk_load(File, [hook(Hook)| LoadOptions]), _, fail) ->
			retractall(probing_),
			catch(logtalk_load(File, RevertOptions), _, true),
			probe_mutation_happened_
		;   retractall(probing_),
			false
		).

	apply_sample_mutants(Mutants, Options, SelectedMutants) :-
		^^option(sampling(all), Options),
		!,
		SelectedMutants = Mutants.
	apply_sample_mutants(Mutants, Options, SelectedMutants) :-
		^^option(sampling(count(Count)), Options),
		!,
		length(Mutants, Total),
		(   Count >= Total ->
			Max = Total
		;   Max = Count
		),
		^^option(seed(Seed), Options),
		seeded_shuffle(Mutants, Seed, Shuffled),
		take(Max, Shuffled, SelectedMutants).
	apply_sample_mutants(Mutants, Options, SelectedMutants) :-
		^^option(sampling(rate(Rate)), Options),
		length(Mutants, Total),
		Max0 is round(Rate * Total),
		(   Max0 >= Total ->
			Max = Total
		;   Max = Max0
		),
		^^option(seed(Seed), Options),
		seeded_shuffle(Mutants, Seed, Shuffled),
		take(Max, Shuffled, SelectedMutants).

	seeded_shuffle(Mutants, Seed, Shuffled) :-
		get_seed(PreviousSeed),
		catch(
			(   randomize(Seed),
				permutation(Mutants, Shuffled)
			),
			Error,
			(   set_seed(PreviousSeed),
				throw(Error)
			)
		),
		set_seed(PreviousSeed).

	run_mutants([], _, _, Results, Results).
	run_mutants([Mutant| Mutants], Options, Index, Results0, Results) :-
		print_message(comment, mutation_testing, testing_mutation(Mutant)),
		(   ^^option(verbose(true), Options) ->
			print_message(comment, mutation_testing, running_mutant(Index, Mutant)),
			print_mutant_reproduction(Mutant)
		;   true
		),
		execute_mutant(Mutant, Options, Status),
		Result = mutant_result(Index, Mutant, Status),
		(   ^^option(verbose(true), Options) ->
			print_message(comment, mutation_testing, mutant_result(Index, Mutant, Status))
		;   true
		),
		NextIndex is Index + 1,
		run_mutants(Mutants, Options, NextIndex, [Result| Results0], Results).

	print_mutant_reproduction(Mutant) :-
		(   mutant_reproduction_commands(Mutant, ApplyCommand, RevertCommand) ->
			print_message(comment, mutation_testing, mutant_reproduction(ApplyCommand, RevertCommand))
		;   true
		).

	mutant_reproduction_commands(mutant(Entity, Predicate, Mutator, Occurrence), ApplyCommand, RevertCommand) :-
		entity_file(Entity, File),
		mutator_hook(Mutator, Entity, Predicate, Occurrence, false, Hook),
		load_options(LoadOptions),
		revert_options(RevertOptions),
		ApplyCommand = logtalk_load(File, [hook(Hook)| LoadOptions]),
		RevertCommand = logtalk_load(File, RevertOptions).
	mutant_reproduction_commands(mutant(Entity, Predicate, Point, Mutator), ApplyCommand, RevertCommand) :-
		mutant_occurrence(Point, Occurrence),
		mutant_reproduction_commands(mutant(Entity, Predicate, Mutator, Occurrence), ApplyCommand, RevertCommand).

	execute_mutant(mutant(Entity, Predicate, Mutator, Occurrence), Options, Status) :-
		^^option(timeout(Timeout), Options),
		^^option(verbose(Verbose), Options),
		^^option(print_mutated_term(PrintMutatedTerm), Options),
		entity_file(Entity, File),
		% when both verbose(true) and print_mutated_term(true), apply the
		% mutator hook in the main process to print original and mutated terms;
		% reset the random seed first so that the mutation is deterministic and
		% matches the one produced by the subprocess (which starts fresh)
		(	Verbose == true, PrintMutatedTerm == true ->
			reset_seed,
			print_mutated_terms(Entity, Predicate, Mutator, Occurrence, File)
		;	true
		),
		% set up unique temp directory for this mutant
		create_temp_dir(TempDir),
		% write config file
		write_mutation_config(TempDir, Entity, Predicate, Mutator, Occurrence, File),
		% build and run the subprocess
		(	catch(
				run_subprocess(TempDir, File, Timeout, Verbose, ExitStatus, Options),
				SubprocessError,
				(	cleanup_temp_dir(TempDir),
					Status = error(subprocess_error(SubprocessError))
				)
			) ->
			(	var(Status) ->
				read_mutant_status(TempDir, ExitStatus, Status),
				cleanup_temp_dir(TempDir)
			;	true
			)
		;	cleanup_temp_dir(TempDir),
			Status = error(subprocess_failed)
		).

	print_mutated_terms(Entity, Predicate, Mutator, Occurrence, File) :-
		mutator_hook(Mutator, Entity, Predicate, Occurrence, true, Hook),
		prepare_mutator_hook(Hook),
		load_options(LoadOptions),
		revert_options(RevertOptions),
		(	catch(logtalk_load(File, [hook(Hook)| LoadOptions]), _, true) ->
			catch(logtalk_load(File, RevertOptions), _, true)
		;	catch(logtalk_load(File, RevertOptions), _, true)
		).

	mutator_hook(Mutator, Entity, Predicate, Occurrence, PrintMutatedTerm, Hook) :-
		Hook =.. [Mutator, Entity, Predicate, Occurrence, PrintMutatedTerm].

	mutant_occurrence(point(_Predicate, _Mutator, occurrence(Occurrence)), Occurrence) :-
		integer(Occurrence),
		Occurrence > 0,
		!.
	mutant_occurrence(_Point, 1).

	prepare_mutator_hook(Hook) :-
		catch(Hook::reset, _, true).

	load_options([reload(always), source_data(on)]).

	revert_options([reload(always), source_data(on)]).

	% Subprocess execution infrastructure

	create_temp_dir(TempDir) :-
		temporary_directory(SysTmp),
		pid(Pid),
		wall_time(Time),
		TimeInt is truncate(Time * 1000),
		atomic_list_concat([logtalk_mutation, Pid, TimeInt], '_', DirName),
		path_concat(SysTmp, DirName, TempDir),
		make_directory_path(TempDir).

	write_mutation_config(TempDir, Entity, Predicate, Mutator, Occurrence, SourceFile) :-
		path_concat(TempDir, 'mutation_config.pl', ConfigFile),
		path_concat(TempDir, 'status.txt', StatusFile),
		% find the mutator file path
		mutator_hook(Mutator, _E, _P, _O, false, Hook),
		object_property(Hook, file(MutatorFile)),
		open(ConfigFile, write, Stream),
		writeq(Stream, mutation_entity(Entity)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_predicate(Predicate)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_mutator(Mutator)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_occurrence(Occurrence)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_source_file(SourceFile)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_status_file(StatusFile)), write(Stream, '.'), nl(Stream),
		writeq(Stream, mutation_mutator_file(MutatorFile)), write(Stream, '.'), nl(Stream),
		close(Stream).

	run_subprocess(TempDir, SourceFile, Timeout, Verbose, ExitStatus, Options) :-
		% derive test directory from source file or startup directory
		tester_directory(SourceFile, Directory, Options),
		^^option(tester_file_name(Tester), Options),
		os::decompose_file_name(Tester, _, TesterName, _),
		% auto-detect backend
		current_logtalk_flag(prolog_dialect, Dialect),
		% build paths
		path_concat(TempDir, 'mutation_config.pl', ConfigFile),
		path_concat(TempDir, 'logs', LogsDir),
		make_directory_path(LogsDir),
		% build initialization goal
		initialization_goal(ConfigFile, Goal),
		% build the full command; redirect output to /dev/null to silence subprocess
		% unless verbose mode is enabled
		(	Verbose == true ->
			Redirect = ''
		;	Redirect = ' > /dev/null 2>&1'
		),
		atomic_list_concat([
			'cd "', Directory, '" && logtalk_tester',
			' -p ', Dialect,
			' -n ', TesterName,
			' -g "', Goal, '"',
			' -d "', LogsDir, '"',
			' -t ', Timeout,
			Redirect
		], Command),
		print_message(silent, mutation_testing, subprocess_command(Command)),
		% run subprocess and collect exit status
		shell(Command, ExitStatus).
%		{process:process_create(Command, [], [wait(Exit)]), Exit = exit(ExitStatus)}.

	tester_directory(_SourceFile, Directory, Options) :-
		^^option(tester_directory(Directory), Options),
		os::directory_exists(Directory),
		!.
	tester_directory(SourceFile, Directory, Options) :-
		^^option(tester_file_name(Tester), Options),
		(	logtalk::loaded_file_property(SourceFile, directory(Directory))
		;	logtalk::expand_library_path(startup, Directory)
		),
		path_concat(Directory, Tester, Path),
		os::file_exists(Path),
		!.

	initialization_goal(ConfigFile, Goal) :-
		atomic_list_concat([
			'logtalk_load(mutation_testing(loader)),',
			'logtalk_load(mutation_testing(subprocess_mutation_hook)),',
			'subprocess_mutation_hook::load_config(''', ConfigFile, ''')'
		], Goal).

	read_mutant_status(TempDir, ExitStatus, Status) :-
		path_concat(TempDir, 'status.txt', StatusFile),
		(	catch(read_status_file(StatusFile, FileStatus), _, fail) ->
			Status = FileStatus
		;	% fall back to exit code
			exit_code_to_status(ExitStatus, Status)
		).

	read_status_file(StatusFile, Status) :-
		open(StatusFile, read, Stream),
		catch(
			read_term(Stream, Status, []),
			Error,
			(close(Stream), throw(Error))
		),
		close(Stream),
		ground(Status).

	exit_code_to_status(0, survived) :- !.
	exit_code_to_status(1, killed) :- !.
	exit_code_to_status(3, timeout) :- !.
	exit_code_to_status(Code, error(exit_status(Code))).

	cleanup_temp_dir(TempDir) :-
		(	directory_exists(TempDir) ->
			catch(os::delete_directory_and_contents(TempDir), _, true)
		;	true
		).

	summary(Results, Options, summary(Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, Score, Threshold, Passed)) :-
		count_results(Results, 0, 0, 0, 0, 0, 0, 0, Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors),
		Scored is Killed + Survived,
		(   Scored =:= 0 ->
			Score = 0.0
		;   Score is 100.0 * Killed / Scored
		),
		^^option(threshold(Threshold), Options),
		(   Score >= Threshold ->
			Passed = true
		;   Passed = false
		).

	count_results([], Total0, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
								Total0, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0).
	count_results([mutant_result(_, _, Status)| Results], Total0, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
								Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors) :-
		Total1 is Total0 + 1,
		increment_status(Status, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
			Killed1, Survived1, Untested1, Timeout1, NoCoverage1, Errors1),
		count_results(Results, Total1, Killed1, Survived1, Untested1, Timeout1, NoCoverage1, Errors1,
								Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors).

	increment_status(killed, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed, Survived0, Untested0, Timeout0, NoCoverage0, Errors0) :-
		Killed is Killed0 + 1.
	increment_status(survived, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived, Untested0, Timeout0, NoCoverage0, Errors0) :-
		Survived is Survived0 + 1.
	increment_status(timeout, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived0, Untested0, Timeout, NoCoverage0, Errors0) :-
		Timeout is Timeout0 + 1.
	increment_status(no_coverage, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived0, Untested0, Timeout0, NoCoverage, Errors0) :-
		NoCoverage is NoCoverage0 + 1.
	increment_status(untested(_), Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived0, Untested, Timeout0, NoCoverage0, Errors0) :-
		Untested is Untested0 + 1.
	increment_status(untested, Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived0, Untested, Timeout0, NoCoverage0, Errors0) :-
		Untested is Untested0 + 1.
	increment_status(error(_), Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors0,
		Killed0, Survived0, Untested0, Timeout0, NoCoverage0, Errors) :-
		Errors is Errors0 + 1.

	format_report(Report) :-
		format_report(text, Report).

	format_report(Format, Report) :-
		current_output(Stream),
		format_report(Stream, Format, Report).

	format_report(Stream, text, report(Container, Reports)) :-
		valid(list, Reports),
		!,
		write(Stream, 'Mutation testing report for '),
		writeq(Stream, Container),
		nl(Stream),
		nl(Stream),
		format_reports(Reports, Stream).
	format_report(Stream, text, report(Entity, summary(Total, Killed, Survived, Untested, Timeout, NoCoverage, Errors, Score, Threshold, Passed), Results)) :-
		write(Stream, 'Mutation testing report for '),
		writeq(Stream, Entity),
		nl(Stream),
		write(Stream, 'Run result: '),
		(   Passed == true ->
			write(Stream, 'passed')
		;   write(Stream, 'failed')
		),
		nl(Stream),
		write(Stream, 'Summary: total='), write(Stream, Total),
		write(Stream, ', killed='), write(Stream, Killed),
		write(Stream, ', survived='), write(Stream, Survived),
		write(Stream, ', untested='), write(Stream, Untested),
		write(Stream, ', timeout='), write(Stream, Timeout),
		write(Stream, ', no_coverage='), write(Stream, NoCoverage),
		write(Stream, ', errors='), write(Stream, Errors),
		nl(Stream),
		write(Stream, 'Score: '), write(Stream, Score),
		write(Stream, ' (threshold '), write(Stream, Threshold), write(Stream, ')'),
		nl(Stream),
		write(Stream, 'Mutants:'),
		nl(Stream),
		format_mutant_results(Results, Stream),
		nl(Stream),
		!.

	format_report(Stream, json, Report) :-
		stryker_json_report(Report, JSON),
		json(list, dash, atom)::generate(stream(Stream), JSON),
		nl(Stream),
		!.

	stryker_json_report(report(Container, Reports), JSON) :-
		valid(list, Reports),
		!,
		container_reports_file_results(Reports, FileResults0),
		merge_file_results(FileResults0, MergedFileResults),
		file_results_pairs(MergedFileResults, FilePairs),
		container_thresholds(Reports, High, Low),
		container_framework(Container, Framework),
		JSON = json([
			'schemaVersion'-'2.0',
			'thresholds'-json(['high'-High, 'low'-Low]),
			'files'-json(FilePairs),
			'framework'-json(['name'-Framework])
		]).
	stryker_json_report(report(Entity, summary(_Total, _Killed, _Survived, _Untested, _Timeout, _NoCoverage, _Errors, _Score, Threshold, _Passed), Results), JSON) :-
		report_file_result(report(Entity, summary(_, _, _, _, _, _, _, _, Threshold, _), Results), FileResult),
		FileResult = file_result(File, Language, Source, Mutants),
		High is max(0, min(100, round(Threshold))),
		JSON = json([
			'schemaVersion'-'2.0',
			'thresholds'-json(['high'-High, 'low'-0]),
			'files'-json([
				File-json([
					'language'-Language,
					'source'-Source,
					'mutants'-Mutants
				])
			]),
			'framework'-json(['name'-'Logtalk "mutation_testing" tool'])
		]).

	container_reports_file_results([], []).
	container_reports_file_results([Report| Reports], [FileResult| FileResults]) :-
		report_file_result(Report, FileResult),
		container_reports_file_results(Reports, FileResults).

	report_file_result(report(Entity, _Summary, Results), file_result(File, logtalk, Source, Mutants)) :-
		entity_file(Entity, File),
		file_source(File, Source),
		results_mutants_json(Results, Mutants).

	file_source(File, Source) :-
		(   catch(read_file_codes(File, Codes), _, fail) ->
			atom_codes(Source, Codes)
		;   Source = ''
		).

	read_file_codes(File, Codes) :-
		open(File, read, Stream),
		read_stream_codes(Stream, Codes),
		close(Stream).

	read_stream_codes(Stream, Codes) :-
		get_code(Stream, Code),
		(   Code == -1 ->
			Codes = []
		;   Codes = [Code| Rest],
			read_stream_codes(Stream, Rest)
		).

	results_mutants_json([], []).
	results_mutants_json([mutant_result(Index, Mutant, Status)| Results], [JSONMutant| JSONMutants]) :-
		mutant_json(Index, Mutant, Status, JSONMutant),
		results_mutants_json(Results, JSONMutants).

	mutant_json(Index, Mutant, Status, json(Pairs)) :-
		normalize_mutant(Mutant, Entity, Predicate, Mutator, Occurrence),
		index_id(Index, Id),
		status_name(Status, StatusName),
		mutant_location(Entity, Predicate, Mutator, Occurrence, Location),
		Pairs = [
			'id'-Id,
			'mutatorName'-Mutator,
			'location'-Location,
			'status'-StatusName
		].

	normalize_mutant(mutant(Entity, Predicate, Mutator, Occurrence), Entity, Predicate, Mutator, Occurrence) :-
		!.
	normalize_mutant(mutant(Entity, Predicate, Point, Mutator), Entity, Predicate, Mutator, Occurrence) :-
		mutant_occurrence(Point, Occurrence).

	index_id(Index, Id) :-
		number_codes(Index, Codes),
		atom_codes(Id, Codes).

	status_name(killed, 'Killed').
	status_name(survived, 'Survived').
	status_name(no_coverage, 'NoCoverage').
	status_name(timeout, 'Timeout').
	status_name(untested(_), 'Pending').
	status_name(untested, 'Pending').
	status_name(error(_), 'RuntimeError').

	mutant_location(Entity, Predicate, Mutator, Occurrence, Location) :-
		(   mutant_terms(mutant(Entity, Predicate, Mutator, Occurrence), _Original, _Mutation, _Variables, _File, StartLine0-EndLine0) ->
			StartLine is max(1, StartLine0),
			EndLine is max(StartLine, EndLine0)
		;   StartLine = 1,
			EndLine = 1
		),
		(   StartLine == EndLine ->
			EndLineForJSON is EndLine + 1
		;   EndLineForJSON = EndLine
		),
		Location = json([
			'start'-json(['line'-StartLine, 'column'-0]),
			'end'-json(['line'-EndLineForJSON, 'column'-0])
		]).

	container_thresholds([], 0, 0).
	container_thresholds(Reports, High, 0) :-
		findall(
			Rounded,
			(
				member(report(_Entity, summary(_, _, _, _, _, _, _, _, Threshold, _), _), Reports),
				Rounded is max(0, min(100, round(Threshold)))
			),
			Thresholds
		),
		(   Thresholds == [] ->
			High = 0
		;   thresholds_maximum(Thresholds, High)
		).

	thresholds_maximum([Threshold| Thresholds], Maximum) :-
		thresholds_maximum(Thresholds, Threshold, Maximum).

	thresholds_maximum([], Maximum, Maximum).
	thresholds_maximum([Threshold| Thresholds], Maximum0, Maximum) :-
		(   Threshold > Maximum0 ->
			Maximum1 = Threshold
		;   Maximum1 = Maximum0
		),
		thresholds_maximum(Thresholds, Maximum1, Maximum).

	container_framework(library(_), 'Logtalk mutation_testing').
	container_framework(directory(_), 'Logtalk mutation_testing').
	container_framework(_Container, 'Logtalk mutation_testing').

	merge_file_results([], []).
	merge_file_results([file_result(File, Language, Source, Mutants)| FileResults], Merged) :-
		merge_file_results(FileResults, Merged0),
		merge_one_file_result(file_result(File, Language, Source, Mutants), Merged0, Merged).

	merge_one_file_result(file_result(File, Language, Source, Mutants), [], [file_result(File, Language, Source, Mutants)]).
	merge_one_file_result(file_result(File, _Language, _Source, Mutants), [file_result(File, Language0, Source0, Mutants0)| Tail], [file_result(File, Language0, Source0, Mutants1)| Tail]) :-
		!,
		append(Mutants0, Mutants, Mutants1).
	merge_one_file_result(FileResult, [Head| Tail], [Head| MergedTail]) :-
		merge_one_file_result(FileResult, Tail, MergedTail).

	file_results_pairs([], []).
	file_results_pairs([file_result(File, Language, Source, Mutants)| FileResults], [File-FileJSON| Pairs]) :-
		FileJSON = json([
			'language'-Language,
			'source'-Source,
			'mutants'-Mutants
		]),
		file_results_pairs(FileResults, Pairs).

	format_reports([], _Stream).
	format_reports([Report| Reports], Stream) :-
		format_report(Stream, text, Report),
		format_reports(Reports, Stream).

	format_mutant_results([], Stream) :-
		write(Stream, '  (none)'),
		nl(Stream).
	format_mutant_results([mutant_result(Index, Mutant, Status)| Results], Stream) :-
		write(Stream, '  #'), write(Stream, Index), write(Stream, ' '),
		writeq(Stream, Mutant), write(Stream, ' => '), writeq(Stream, Status),
		nl(Stream),
		format_mutant_terms(Stream, Mutant),
		format_mutant_results_nonempty(Results, Stream).

	format_mutant_results_nonempty([], _).
	format_mutant_results_nonempty([mutant_result(Index, Mutant, Status)| Results], Stream) :-
		write(Stream, '  #'), write(Stream, Index), write(Stream, ' '),
		writeq(Stream, Mutant), write(Stream, ' => '), writeq(Stream, Status),
		nl(Stream),
		format_mutant_terms(Stream, Mutant),
		format_mutant_results_nonempty(Results, Stream).

	format_mutant_terms(Stream, Mutant) :-
		(   mutant_terms(Mutant, Original, Mutation, Variables, File, StartLine-EndLine) ->
			write(Stream, '     original: '),
			write_term(Stream, Original, [quoted(true), variable_names(Variables)]),
			nl(Stream),
			write(Stream, '     mutation: '),
			write_term(Stream, Mutation, [quoted(true), variable_names(Variables)]),
			nl(Stream),
			write(Stream, '     location: '),
			writeq(Stream, File),
			write(Stream, ':'),
			write(Stream, StartLine),
			write(Stream, '-'),
			write(Stream, EndLine),
			nl(Stream)
		;   true
		).

	mutant_terms(mutant(Entity, Predicate, Mutator, Occurrence), Original, Mutation, Variables, File, StartLine-EndLine) :-
		entity_file(Entity, SourceFile),
		retractall(captured_mutated_terms_(_, _, _, _, _)),
		assertz(capturing_mutated_terms_),
		mutator_hook(Mutator, Entity, Predicate, Occurrence, true, Hook),
		prepare_mutator_hook(Hook),
		load_options(LoadOptions),
		revert_options(RevertOptions),
		(   catch(logtalk_load(SourceFile, [hook(Hook)| LoadOptions]), _, fail) ->
			retractall(capturing_mutated_terms_),
			catch(logtalk_load(SourceFile, RevertOptions), _, true),
			captured_mutated_terms_(Original, Mutation, Variables, File, StartLine-EndLine)
		;   retractall(capturing_mutated_terms_),
			fail
		).

	% by default, include all loaded entities:
	default_option(include_entities([])).
	% by default, don't exclude entities:
	default_option(exclude_entities([])).
	% by default, include all discovered mutators:
	default_option(max_mutators(all)).
	% by default, don't limit mutations generated per mutator:
	default_option(max_mutations_per_mutator(5)).
	% by default, don't select mutators:
	default_option(mutators([])).
	% by default, no minimum threshold:
	default_option(threshold(0.0)).
	% by default, print summary only:
	default_option(verbose(false)).
	% by default, don't print mutated terms:
	default_option(print_mutated_term(false)).
	% by default, cap each lgtunit runner mutant execution:
	default_option(timeout(300)).
	% by default, execute all mutants:
	default_option(sampling(all)).
	% default pseudo-random generator seed:
	default_option(seed(123456789)).
	% by default, print text report output:
	default_option(format(text)).
	% default report file base name:
	default_option(report_file_name('mutation_test_report')).
	% default tester file name
	default_option(tester_file_name('tester.lgt')).

	valid_option(include_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(exclude_entities(Entities)) :-
		valid(list(atom), Entities).
	valid_option(max_mutators(all)).
	valid_option(max_mutators(MaxMutators)) :-
		integer(MaxMutators),
		MaxMutators > 0.
	valid_option(max_mutations_per_mutator(all)).
	valid_option(max_mutations_per_mutator(MaxMutations)) :-
		integer(MaxMutations),
		MaxMutations > 0.
	valid_option(mutators(Mutators)) :-
		valid(list(atom), Mutators),
		forall(
			(	member(Mutator, Mutators),
				mutator_hook(Mutator, _Entity, _Predicate, _Occurrence, _PrintMutatedTerm, Hook)
			),
			conforms_to_protocol(Hook, mutator_protocol)
		).
	valid_option(threshold(Threshold)) :-
		number(Threshold),
		Threshold >= 0.0,
		Threshold =< 100.0.
	valid_option(verbose(Verbose)) :-
		valid(boolean, Verbose).
	valid_option(print_mutated_term(PrintMutatedTerm)) :-
		valid(boolean, PrintMutatedTerm).
	valid_option(timeout(Timeout)) :-
		number(Timeout),
		Timeout > 0.
	valid_option(sampling(all)).
	valid_option(sampling(count(Count))) :-
		integer(Count),
		Count >= 0.
	valid_option(sampling(rate(Rate))) :-
		number(Rate),
		Rate >= 0.0,
		Rate =< 1.0.
	valid_option(seed(Seed)) :-
		integer(Seed).
	valid_option(format(Format)) :-
		member(Format, [none, text, json]).
	valid_option(report_file_name(FileName)) :-
		atom(FileName).
	valid_option(tester_file_name(Tester)) :-
		atom(Tester).
	valid_option(tester_directory(Directory)) :-
		atom(Directory).

:- end_object.
