%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(linter_reporter,
	imports((tool_diagnostics_common, tutor_explanations, options))).

	:- info([
		version is 1:0:2,
		author is 'Paulo Moura',
		date is 2026-07-19,
		comment is 'Intercepts compiler linter warnings and caches them as machine-readable diagnostics.',
		remarks is [
			'Usage' - 'Load this tool before compiling code to be checked by the built-in linter. Call ``enable/0-1`` before compiling code, ``disable/0`` when finished collecting warnings, and then query the cached warnings using either the legacy warning predicates or the diagnostics protocol predicates. The standalone ``sarif`` tool can generate SARIF reports by querying these diagnostics.',
			'Diagnostics targets' - 'The diagnostics predicates accept the targets ``all``, ``entity(Entity)``, ``file(File)``, ``directory(Directory)``, ``rdirectory(Directory)``, ``library(Library)``, and ``rlibrary(Library)``. These targets simply filter the cached diagnostics collected in the current warning collection session.'
		]
	]).

	:- public(enable/0).
	:- mode(enable, one).
	:- info(enable/0, [
		comment is 'Enables warning collection and starts a fresh warning collection session using the default options.'
	]).

	:- public(enable/1).
	:- mode(enable(+list(compound)), one_or_error).
	:- info(enable/1, [
		comment is 'Enables warning collection and starts a fresh warning collection session using the given options.',
		argnames is ['Options'],
		exceptions is [
			'``Options`` is a variable' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is a variable' - instantiation_error,
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(disable/0).
	:- mode(disable, one).
	:- info(disable/0, [
		comment is 'Disables warning collection while preserving the cached warnings for later querying.'
	]).

	:- public(reset/0).
	:- mode(reset, one).
	:- info(reset/0, [
		comment is 'Clears all cached warnings collected in the current session.'
	]).

	:- public(warning/1).
	:- mode(warning(-compound), zero_or_more).
	:- info(warning/1, [
		comment is 'Enumerates normalized linter warnings collected in the current session.',
		argnames is ['Warning']
	]).

	:- public(warnings/1).
	:- mode(warnings(-list(compound)), one).
	:- info(warnings/1, [
		comment is 'Returns the collected normalized linter warnings.',
		argnames is ['Warnings']
	]).

	:- public(summary/1).
	:- mode(summary(-compound), one).
	:- info(summary/1, [
		comment is 'Returns a machine-readable summary for the collected linter warnings.',
		argnames is ['Summary']
	]).

	:- private(enabled_/0).
	:- dynamic(enabled_/0).
	:- mode(enabled_, zero_or_one).
	:- info(enabled_/0, [
		comment is 'True when warning collection is enabled.'
	]).

	:- private(warning_sequence_/1).
	:- dynamic(warning_sequence_/1).
	:- mode(warning_sequence_(-integer), zero_or_one).
	:- info(warning_sequence_/1, [
		comment is 'Stores the last assigned warning sequence number.',
		argnames is ['Sequence']
	]).

	:- private(recorded_warning_/4).
	:- dynamic(recorded_warning_/4).
	:- mode(recorded_warning_(?integer, ?atom, ?compound, ?list(compound)), zero_or_more).
	:- info(recorded_warning_/4, [
		comment is 'Caches collected warnings together with their sequence number, flag, normalized message term, and printed message tokens.',
		argnames is ['Sequence', 'Flag', 'Message', 'Tokens']
	]).

	:- private(collection_options_/1).
	:- dynamic(collection_options_/1).
	:- mode(collection_options_(-list(compound)), zero_or_one).
	:- info(collection_options_/1, [
		comment is 'Stores the merged options for the current warning collection session.',
		argnames is ['Options']
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(logtalk, [
		expand_library_path/2, file_type_extension/2, print_message_tokens/3
	]).

	:- uses(os, [
		absolute_file_name/2,
		decompose_file_name/3, decompose_file_name/4,
		internal_os_path/2
	]).

	:- uses(term_io, [
		with_output_to/2
	]).

	:- uses(type, [
		valid/2
	]).

	:- uses(user, [
		atomic_list_concat/2, atomic_list_concat/3
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, warning(Flag), core, Tokens) :-
		enabled_,
		catch(handle_warning(Flag, Message, Tokens), _, true),
		fail.

	enable :-
		enable([]).

	enable(UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		retractall(enabled_),
		retractall(collection_options_(_)),
		assertz(collection_options_(Options)),
		assertz(enabled_),
		reset.

	disable :-
		retractall(enabled_).

	reset :-
		retractall(recorded_warning_(_, _, _, _)),
		retractall(warning_sequence_(_)),
		assertz(warning_sequence_(0)).

	warning(Warning) :-
		recorded_warning_(_Sequence, Flag, Message, Tokens),
		normalized_warning(Flag, Message, Tokens, Warning).

	warnings(Warnings) :-
		findall(Warning, warning(Warning), Warnings).

	summary(summary(TotalWarnings, RuleCounts, FlagCounts)) :-
		warnings(Warnings),
		length(Warnings, TotalWarnings),
		rule_counts(Warnings, RuleCounts),
		flag_counts(Warnings, FlagCounts).

	diagnostics_tool(linter_reporter, linter_reporter, Version, 'https://logtalk.org/', [
		guid('4fe3f47d-85c6-4b19-9bb5-07828934f2cb'),
		fingerprint_algorithm(canonical_warning_v1),
		count_key(totalWarnings),
		include_invocations(true),
		include_git_metadata(true),
		include_version_control_provenance(true)
	]) :-
		this(This),
		object_property(This, info(Info)),
		memberchk(version(Major:Minor:Patch), Info),
		atomic_list_concat([Major, Minor, Patch], '.', Version).

	diagnostic_rule(RuleId, ShortDescription, FullDescription, warning, []) :-
		captured_rule_id(RuleId),
		!,
		rule_descriptions(RuleId, ShortDescription, FullDescription).

	diagnostic_rules(Rules) :-
		(	setof(RuleId, captured_rule_id(RuleId), RuleIds) ->
			diagnostic_rules_from_ids(RuleIds, Rules)
		;	Rules = []
		).

	diagnostic_rules_from_ids([], []).
	diagnostic_rules_from_ids([RuleId| RuleIds], [diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties)| Rules]) :-
		diagnostic_rule(RuleId, ShortDescription, FullDescription, DefaultSeverity, Properties),
		diagnostic_rules_from_ids(RuleIds, Rules).

	validate_diagnostics_options(UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, _).

	diagnostic(Target, Diagnostic, UserOptions) :-
		validate_diagnostics_options(UserOptions),
		target_diagnostic(Target, Diagnostic).

	diagnostic(Target, Diagnostic) :-
		diagnostic(Target, Diagnostic, []).

	diagnostics(Target, Diagnostics, UserOptions) :-
		validate_diagnostics_options(UserOptions),
		findall(Diagnostic, target_diagnostic(Target, Diagnostic), Diagnostics).

	diagnostics(Target, Diagnostics) :-
		diagnostics(Target, Diagnostics, []).

	diagnostics_summary(Target, diagnostics_summary(Target, TotalContexts, TotalDiagnostics, Breakdown, ContextSummaries), UserOptions) :-
		diagnostics(Target, Diagnostics, UserOptions),
		length(Diagnostics, TotalDiagnostics),
		^^diagnostics_breakdown(Diagnostics, Breakdown),
		^^context_summaries(Diagnostics, ContextSummaries),
		length(ContextSummaries, TotalContexts).

	diagnostics_summary(Target, Summary) :-
		diagnostics_summary(Target, Summary, []).

	diagnostics_preflight(_Target, [], UserOptions) :-
		validate_diagnostics_options(UserOptions).

	diagnostics_preflight(Target, Issues) :-
		diagnostics_preflight(Target, Issues, []).

	handle_warning(Flag, Message, Tokens) :-
		next_warning_sequence(Sequence),
		ground_term_copy(Message, GroundMessage),
		ground_term_copy(Tokens, GroundTokens),
		assertz(recorded_warning_(Sequence, Flag, GroundMessage, GroundTokens)).

	next_warning_sequence(Sequence) :-
		( retract(warning_sequence_(Current)) -> true ; Current = 0 ),
		Sequence is Current + 1,
		assertz(warning_sequence_(Sequence)).

	normalized_warning(Flag, Message, Tokens, linter_warning(Flag, RuleId, File, Lines, Context, Properties)) :-
		warning_data(Message, RuleId, File, Lines, Context, Details),
		warning_text(Tokens, MessageText),
		warning_properties(Message, Lines, Details, MessageText, Properties).

	warning_data(Message, RuleId, File, Lines, Context, Details) :-
		functor(Message, RuleId, _),
		Message =.. [_Functor| Arguments],
		(	Arguments = [File0, Lines0, Type, Entity| Details0],
			atom(File0),
			nonvar(Lines0),
			Lines0 = _-_,
			entity_context_kind(Type) ->
			% assume entity context but note that this assessment is heuristic
			File = File0,
			Lines = Lines0,
			Context = context(Type, Entity),
			Details = Details0
		;	Arguments = [File0, Lines0| Details0],
			atom(File0),
			nonvar(Lines0),
			Lines0 = _-_ ->
			File = File0,
			Lines = Lines0,
			Context = context(file, File0),
			Details = Details0
		;	File = '',
			Lines = 0-0,
			Context = context(file, ''),
			Details = Arguments
		).

	entity_context_kind(object).
	entity_context_kind(category).
	entity_context_kind(protocol).

	warning_properties(Message, Lines, Details, MessageText, Properties) :-
		BaseProperties = [message(MessageText), raw_term(Message), details(Details)],
		(	Lines == 0-0 ->
			Properties0 = [auxiliary_clause(true)| BaseProperties]
		;	Properties0 = BaseProperties
		),
		(	explanation_text(Message, ExplanationText) ->
			Properties = [explanation(ExplanationText)| Properties0]
		;	Properties = Properties0
		).

	warning_text(Tokens, Text) :-
		with_output_to(atom(Text), print_tokens(Tokens)).

	print_tokens(Tokens) :-
		current_output(Stream),
		logtalk::print_message_tokens(Stream, '', Tokens).

	explanation_text(Message, ExplanationText) :-
		collection_options_(Options),
		^^option(explanations(true), Options),
		phrase(^^explain(Message), Explanation),
		warning_text(Explanation, ExplanationText).

	captured_rule_id(RuleId) :-
		recorded_warning_(_Sequence, _Flag, Message, _Tokens),
		functor(Message, RuleId, _).

	target_diagnostic(Target, diagnostic(RuleId, warning, not_applicable, MessageText, Context, File, Lines, [flag(Flag)| Properties])) :-
		warning(linter_warning(Flag, RuleId, File, Lines, Context, Properties)),
		target_matches(Target, Context, File),
		memberchk(message(MessageText), Properties).

	target_matches(all, _Context, _File).
	target_matches(entity(Entity), context(Kind, Entity), _File) :-
		Kind \== file.
	target_matches(file(Source), _Context, File) :-
		locate_target_file(Source, TargetFile),
		diagnostic_file_path(File, TargetFile).
	target_matches(directory(Directory0), _Context, File) :-
		normalize_directory_path(Directory0, Directory),
		diagnostic_file_directory(File, Directory).
	target_matches(rdirectory(Directory0), _Context, File) :-
		normalize_directory_path(Directory0, Directory),
		diagnostic_file_directory(File, FileDirectory),
		sub_atom(FileDirectory, 0, _, _, Directory).
	target_matches(library(Library), _Context, File) :-
		expand_library_path(Library, Directory),
		diagnostic_file_directory(File, Directory).
	target_matches(rlibrary(Library), _Context, File) :-
		expand_library_path(Library, TopPath),
		diagnostic_file_directory(File, Directory),
		once((	Directory == TopPath
		;	sub_library(TopPath, _SubLibrary, Directory)
		)).

	locate_target_file(LibraryNotation, Path) :-
		compound(LibraryNotation),
		!,
		LibraryNotation =.. [Library, Name],
		expand_library_path(Library, LibraryPath),
		atom_concat(LibraryPath, Name, Source),
		locate_target_file(Source, Path).
	locate_target_file(Source, Path) :-
		add_extension(Source, Basename),
		captured_file(Path, Basename),
		\+ (
			captured_file(OtherPath, Basename),
			Path \== OtherPath
		),
		!.
	locate_target_file(Source, Path) :-
		add_extension(Source, SourceWithExtension),
		captured_file(Path, Basename),
		diagnostic_file_directory(Path, Directory),
		atom_concat(Directory, Basename, SourceWithExtension),
		!.

	captured_file(Path, Basename) :-
		recorded_warning_(_Sequence, _Flag, Message, _Tokens),
		warning_data(Message, _RuleId, File, _Lines, _Context, _Details),
		diagnostic_file_path(File, Path),
		decompose_file_name(Path, _Directory, Basename).

	diagnostic_file_path(File0, File) :-
		File0 \== '',
		internal_os_path(File1, File0),
		absolute_file_name(File1, File).

	diagnostic_file_directory(File0, Directory) :-
		diagnostic_file_path(File0, File),
		decompose_file_name(File, Directory, _).

	add_extension(Source, SourceWithExtension) :-
		atom(Source),
		decompose_file_name(Source, _, _, SourceExtension),
		(	file_type_extension(source, SourceExtension) ->
			SourceWithExtension = Source
		;	file_type_extension(source, Extension),
			atom_concat(Source, Extension, SourceWithExtension)
		).

	sub_library(TopPath, Library, LibraryPath) :-
		logtalk_library_path(Library, _),
		expand_library_path(Library, LibraryPath),
		atom_concat(TopPath, _RelativePath, LibraryPath).

	normalize_directory_path(Directory0, Directory) :-
		internal_os_path(Directory1, Directory0),
		absolute_file_name(Directory1, Directory2),
		(	sub_atom(Directory2, _, _, 0, '/') ->
			Directory = Directory2
		;	atom_concat(Directory2, '/', Directory)
		).

	rule_descriptions(RuleId, ShortDescription, FullDescription) :-
		atomic_list_concat(['Logtalk linter warning: ', RuleId, '.'], ShortDescription),
		( 	once((
				warning(linter_warning(_Flag, RuleId, _File, _Lines, _Context, Properties)),
				member(explanation(Explanation), Properties)
			)) ->
			FullDescription = Explanation
		;	atomic_list_concat(['Warnings emitted by the Logtalk compiler linter for rule family ', RuleId, '.'], FullDescription)
		).

	rule_counts(Warnings, RuleCounts) :-
		(	setof(RuleId, Flag^File^Lines^Context^Properties^member(linter_warning(Flag, RuleId, File, Lines, Context, Properties), Warnings), RuleIds) ->
			rule_counts(RuleIds, Warnings, RuleCounts)
		;	RuleCounts = []
		).

	rule_counts([], _Warnings, []) :-
		!.
	rule_counts([RuleId| RuleIds], Warnings, [rule_count(RuleId, Count)| RuleCounts]) :-
		rule_count(Warnings, RuleId, Count),
		rule_counts(RuleIds, Warnings, RuleCounts).

	rule_count([], _RuleId, 0).
	rule_count([linter_warning(_Flag, RuleId, _File, _Lines, _Context, _Properties)| Warnings], RuleId, Count) :-
		!,
		rule_count(Warnings, RuleId, Count0),
		Count is Count0 + 1.
	rule_count([_| Warnings], RuleId, Count) :-
		rule_count(Warnings, RuleId, Count).

	flag_counts(Warnings, FlagCounts) :-
		(	setof(Flag, RuleId^File^Lines^Context^Properties^member(linter_warning(Flag, RuleId, File, Lines, Context, Properties), Warnings), Flags) ->
			flag_counts(Flags, Warnings, FlagCounts)
		;	FlagCounts = []
		).

	flag_counts([], _Warnings, []).
	flag_counts([Flag| Flags], Warnings, [flag_count(Flag, Count)| FlagCounts]) :-
		flag_count(Warnings, Flag, Count),
		flag_counts(Flags, Warnings, FlagCounts).

	flag_count([], _Flag, 0).
	flag_count([linter_warning(Flag, _RuleId, _File, _Lines, _Context, _Properties)| Warnings], Flag, Count) :-
		!,
		flag_count(Warnings, Flag, Count0),
		Count is Count0 + 1.
	flag_count([_| Warnings], Flag, Count) :-
		flag_count(Warnings, Flag, Count).

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

	default_option(explanations(false)).

	valid_option(explanations(Boolean)) :-
		valid(boolean, Boolean).

:- end_object.
