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
	imports((tutor_explanations, options))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-29,
		comment is 'Intercepts compiler linter warnings and exports them as SARIF reports.',
		remarks is [
			'Usage' - 'Load this tool before compiling code to be checked by the built-in linter. Call ``enable/0-1`` before compiling code, ``disable/0`` when finished collecting warnings, and ``report/0-1`` to generate the SARIF report from the cached warnings.'
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
		argnames is ['Options']
	]).

	:- public(disable/0).
	:- mode(disable, one).
	:- info(disable/0, [
		comment is 'Disables warning collection while preserving the cached warnings for later reporting.'
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

	:- public(report/0).
	:- mode(report, one).
	:- info(report/0, [
		comment is 'Writes a SARIF report for the currently cached warnings to the default file ``./linter_warnings.sarif``.'
	]).

	:- public(report/1).
	:- mode(report(++compound), one).
	:- info(report/1, [
		comment is 'Writes a SARIF report for the currently cached warnings to the given sink accepted by the ``json::generate/2`` predicate.',
		argnames is ['Sink']
	]).

	:- private(enabled_/0).
	:- dynamic(enabled_/0).
	:- mode(enabled_, zero_or_one).
	:- info(enabled_/0, [
		comment is 'True when warning collection is enabled.'
	]).

	:- private(reporting_/0).
	:- dynamic(reporting_/0).
	:- mode(reporting_, zero_or_one).
	:- info(reporting_/0, [
		comment is 'True while a SARIF report is being generated.'
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

	:- uses(git, [
		branch/2 as git_branch/2, commit_hash/2 as git_commit_hash/2
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(logtalk, [
		print_message_tokens/3
	]).

	:- uses(os, [
		decompose_file_name/3, internal_os_path/2
	]).

	:- uses(term_io, [
		with_output_to/2, write_term_to_atom/3
	]).

	:- uses(url(atom), [
		generate/2 as url_generate/2, normalize/2 as url_normalize/2
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	:- uses(uuid, [
		uuid_v4/1
	]).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, warning(Flag), core, Tokens) :-
		enabled_,
		\+ reporting_,
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
		retractall(reporting_),
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

	report :-
		report(file('./linter_warnings.sarif')).

	report(Sink) :-
		assertz(reporting_),
		catch(
			(report_term(Term), json::generate(Sink, Term)),
			Error,
			(retractall(reporting_), throw(Error))
		),
		retractall(reporting_).

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
			Lines0 = _-_ ->
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
			Context = context(file),
			Details = Details0
		;	File = '',
			Lines = 0-0,
			Context = context(none),
			Details = Arguments
		).

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

	report_term({
		'$schema'-'https://json.schemastore.org/sarif-2.1.0.json',
		version-'2.1.0',
		runs-[Run]
	}) :-
		warnings(Warnings),
		sarif_rule_ids(Warnings, RuleIds),
		sarif_rules(RuleIds, Warnings, Rules),
		sarif_results(Warnings, RuleIds, Results),
		uuid_v4(RunGUID),
		run_properties(Warnings, RunProperties),
		Driver = {
			name-linter_reporter,
			informationUri-'https://logtalk.org/',
			version-'0.1.0',
			rules-Rules
		},
		Run = {
			tool-{driver-Driver},
			automationDetails-{id-linter_reporter, guid-RunGUID},
			properties-RunProperties,
			results-Results
		}.

	run_properties(Warnings, {
		totalWarnings-TotalWarnings,
		fingerprintAlgorithm-canonical_warning_v1,
		gitBranch-Branch,
		gitCommitHash-CommitHash
	}) :-
		length(Warnings, TotalWarnings),
		run_git_metadata(Warnings, Branch, CommitHash),
		!.
	run_properties(Warnings, {
		totalWarnings-TotalWarnings,
		fingerprintAlgorithm-canonical_warning_v1
	}) :-
		length(Warnings, TotalWarnings).

	run_git_metadata([linter_warning(_Flag, _RuleId, File, _Lines, _Context, _Properties)| _], Branch, CommitHash) :-
		File \== '',
		decompose_file_name(File, Directory, _BaseName),
		git_branch(Directory, Branch),
		git_commit_hash(Directory, CommitHash).
	run_git_metadata([_| Warnings], Branch, CommitHash) :-
		run_git_metadata(Warnings, Branch, CommitHash).

	sarif_rule_ids(Warnings, RuleIds) :-
		(	setof(RuleId, Flag^File^Lines^Context^Properties^member(linter_warning(Flag, RuleId, File, Lines, Context, Properties), Warnings), RuleIds0) ->
			RuleIds = RuleIds0
		;	RuleIds = []
		).

	sarif_rules([], _Warnings, []).
	sarif_rules([RuleId| RuleIds], Warnings, [Rule| Rules]) :-
		sarif_rule(RuleId, Warnings, Rule),
		sarif_rules(RuleIds, Warnings, Rules).

	sarif_rule(RuleId, Warnings, {
		id-RuleId,
		name-RuleId,
		shortDescription-{text-ShortDescription},
		fullDescription-{text-FullDescription},
		defaultConfiguration-{level-warning}
	}) :-
		rule_descriptions(RuleId, Warnings, ShortDescription, FullDescription).

	rule_descriptions(RuleId, Warnings, ShortDescription, FullDescription) :-
		atomic_list_concat(['Logtalk linter warning: ', RuleId, '.'], ShortDescription),
		( 	once((
				member(linter_warning(_Flag, RuleId, _File, _Lines, _Context, Properties), Warnings),
				member(explanation(Explanation), Properties)
			)) ->
			FullDescription = Explanation
		;	atomic_list_concat(['Warnings emitted by the Logtalk compiler linter for rule family ', RuleId, '.'], FullDescription)
		).

	sarif_results([], _RuleIds, []).
	sarif_results([Warning| Warnings], RuleIds, [Result| Results]) :-
		sarif_result(Warning, RuleIds, Result),
		sarif_results(Warnings, RuleIds, Results).

	sarif_result(linter_warning(Flag, RuleId, File, Lines, Context, Properties), RuleIds, {
		ruleId-RuleId,
		ruleIndex-RuleIndex,
		level-warning,
		message-{text-MessageText},
		partialFingerprints-PartialFingerprints,
		fingerprints-Fingerprints,
		properties-PropertiesJSON,
		locations-[Location]
	}) :-
		File \== '',
		rule_index(RuleId, RuleIds, RuleIndex),
		memberchk(message(MessageText), Properties),
		properties_json(Flag, Context, Properties, PropertiesJSON),
		sarif_file_uri(File, FileURI),
		sarif_location(FileURI, Lines, Location),
		sarif_fingerprints(RuleId, FileURI, Lines, Context, Properties, PartialFingerprints, Fingerprints),
		!.
	sarif_result(linter_warning(Flag, RuleId, _File, Lines, Context, Properties), RuleIds, {
		ruleId-RuleId,
		ruleIndex-RuleIndex,
		level-warning,
		message-{text-MessageText},
		partialFingerprints-PartialFingerprints,
		fingerprints-Fingerprints,
		properties-PropertiesJSON
	}) :-
		rule_index(RuleId, RuleIds, RuleIndex),
		memberchk(message(MessageText), Properties),
		properties_json(Flag, Context, Properties, PropertiesJSON),
		sarif_fingerprints(RuleId, '', Lines, Context, Properties, PartialFingerprints, Fingerprints).

	rule_index(RuleId, [RuleId| _], 0) :-
		!.
	rule_index(RuleId, [_| RuleIds], RuleIndex) :-
		rule_index(RuleId, RuleIds, RuleIndex0),
		RuleIndex is RuleIndex0 + 1.

	sarif_location(FileURI, 0-0, {
		physicalLocation-{
			artifactLocation-{uri-FileURI}
		}
	}).
	sarif_location(FileURI, Start-End, {
		physicalLocation-{
			artifactLocation-{uri-FileURI},
			region-{startLine-Start, endLine-End}
		}
	}).

	properties_json(Flag, Context, Properties, {
		flag-Flag,
		context-ContextAtom,
		rawTerm-RawTermAtom,
		details-DetailsAtoms,
		auxiliaryClause-AuxiliaryClauseJSON,
		hasExplanation-HasExplanationJSON,
		explanation-ExplanationText,
		warningProperties-PropertyAtoms
	}) :-
		to_atom(Context, ContextAtom),
		memberchk(raw_term(RawTerm), Properties),
		to_atom(RawTerm, RawTermAtom),
		memberchk(details(Details), Properties),
		terms_to_atoms(Details, DetailsAtoms),
		(	member(auxiliary_clause(true), Properties) ->
			AuxiliaryClauseJSON = @true
		;	AuxiliaryClauseJSON = @false
		),
		(	member(explanation(Explanation), Properties) ->
			HasExplanationJSON = @true,
			ExplanationText = Explanation
		;	HasExplanationJSON = @false,
			ExplanationText = ''
		),
		terms_to_atoms(Properties, PropertyAtoms).

	sarif_fingerprints(RuleId, FileURI, Lines, Context, Properties, {
		ruleLocationV1-RuleLocationFingerprint,
		contextV1-ContextFingerprint
	}, {
		canonicalWarningV1-CanonicalWarningFingerprint
	}) :-
		to_atom(rule_location(RuleId, FileURI, Lines), RuleLocationFingerprint),
		to_atom(context(Context), ContextFingerprint),
		to_atom(warning(RuleId, FileURI, Lines, Context, Properties), CanonicalWarningFingerprint).

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

	terms_to_atoms([], []).
	terms_to_atoms([Term| Terms], [Atom| Atoms]) :-
		to_atom(Term, Atom),
		terms_to_atoms(Terms, Atoms).

	to_atom(Term, Atom) :-
		(	atom(Term) ->
			Atom = Term
		;	copy_term(Term, Copy),
			numbervars(Copy, 0, _),
			write_term_to_atom(Copy, Atom, [numbervars(true), quoted(true)])
		).

	sarif_file_uri(File, URI) :-
		internal_os_path(File, Path0),
		file_url_path(Path0, Path),
		url_generate([scheme(file), authority(''), path(Path)], URI0),
		url_normalize(URI0, URI),
		!.

	file_url_path(Path0, Path) :-
		(	sub_atom(Path0, 0, 1, _, '/') ->
			Path = Path0
		;	sub_atom(Path0, 1, 2, _, ':/') ->
			atom_concat('/', Path0, Path)
		;	Path = Path0
		).

	ground_term_copy(Term, GroundTerm) :-
		copy_term(Term, GroundTerm),
		numbervars(GroundTerm, 0, _).

	default_option(explanations(false)).

	valid_option(explanations(true)).
	valid_option(explanations(false)).

:- end_object.
