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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-31,
		comment is 'Unit tests for the sarif tool.'
	]).

	cover(sarif).

	:- uses(sarif, [
		generate/4
	]).

	:- uses(dead_code_scanner, [
		diagnostics/2 as dcs_diagnostics/2,
		diagnostics/3 as dcs_diagnostics/3
	]).

	:- uses(linter_reporter, [
		enable/1, disable/0, reset/0, diagnostics/2
		as lr_diagnostics/2
	]).

	:- uses(lgtdoc, [
		diagnostics/2 as lgtdoc_diagnostics/2
	]).

	:- uses(lgtunit, [
		diagnostics/2 as lgtunit_diagnostics/2
	]).

	:- uses(json, [
		parse/2 as json_parse/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(lgtunit, [
		assertion/1
	]).

	:- uses(list, [
		length/2, memberchk/2
	]).

	:- uses(url(atom), [
		valid/1 as valid_url/1
	]).

	cleanup :-
		^^clean_file('aggregate_report.sarif'),
		^^clean_file('linter_warnings.sarif'),
		clean_xml_docs.

	test(sarif_dcs_01, deterministic) :-
		generate(dead_code_scanner, entity(category), atom(Atom), []),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {
			'$schema'-'https://json.schemastore.org/sarif-2.1.0.json',
			version-'2.1.0',
			runs-[Run]
		},
		sarif_dcs_run_ok(Run, Rules, Properties, Notifications, Results),
		sarif_dcs_rule_ok(Rules, local_dead_code, Rule),
		^^assertion(subsumes_term({
			id-local_dead_code,
			guid-'f6fd0e53-0c2d-45fd-a6dd-7b2f2af3e2a1',
			name-local_dead_code,
			shortDescription-{text-_},
			fullDescription-{text-_},
			defaultConfiguration-{level-warning}
		}, Rule)),
		sarif_dcs_run_properties_ok(Properties),
		sarif_original_uri_base_ids_ok(Run, 'APP_ROOT'),
		^^assertion(Notifications = [_]),
		Results = [FirstResult| _],
		^^assertion(subsumes_term({
			ruleId-local_dead_code,
			ruleIndex-0,
			level-note,
			message-{text-_},
			locations-_,
			partialFingerprints-{entityPredicateV1-_, locationV1-_},
			fingerprints-{canonicalFindingV1-_},
			properties-{class-local_dead_code, confidence-medium, findingProperties-_, entityKind-_, entity-_, predicate-_}
		}, FirstResult)),
		sarif_relative_result_locations_ok(Results, 'APP_ROOT'),
		category_property(category, file(CategoryFile)),
		os::decompose_file_name(CategoryFile, ApplicationRoot, _),
		sarif_location_fingerprints_ok(Results, 'APP_ROOT', ApplicationRoot),
		^^assertion(subsumes_term([_, _, _, _, _], Results)).

	test(sarif_dcs_02, deterministic) :-
		generate(dead_code_scanner, entity(predicate_directives), atom(Atom), []),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
		sarif_dcs_run_ok(Run, Rules, Properties, Notifications, Results),
		sarif_dcs_run_properties_ok(Properties),
		sarif_original_uri_base_ids_ok(Run, 'APP_ROOT'),
		sarif_relative_result_locations_ok(Results, 'APP_ROOT'),
		^^assertion(subsumes_term([_], Notifications)),
		sarif_dcs_result_ok(Results, unused_uses_resource, 1, error, high, _),
		(	current_logtalk_flag(modules, supported) ->
			sarif_dcs_rule_ok(Rules, unused_uses_resource, UsesRule),
			sarif_dcs_rule_ok(Rules, unused_use_module_resource, UseModuleRule),
			sarif_dcs_result_ok(Results, unused_use_module_resource, 2, error, high, _),
			^^assertion(subsumes_term({id-unused_uses_resource, guid-_, name-unused_uses_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UsesRule)),
			^^assertion(subsumes_term({id-unused_use_module_resource, guid-_, name-unused_use_module_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UseModuleRule)),
			^^assertion(length(Results, 5))
		;	sarif_dcs_rule_ok(Rules, unused_uses_resource, UsesRule),
			^^assertion(subsumes_term({id-unused_uses_resource, guid-_, name-unused_uses_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UsesRule)),
			^^assertion(length(Results, 3))
		).

	test(sarif_dcs_03, deterministic) :-
		generate(dead_code_scanner, entity(category), atom(Atom), [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-_, runs-[Run]},
		sarif_dcs_run_results(Run, Results),
		^^assertion(subsumes_term([_, _, _], Results)),
		!.

	test(sarif_dcs_file_01, deterministic) :-
		object_property(lgtunit, file(File)),
		once((
			dcs_diagnostics(file(File), Diagnostics),
			generate(dead_code_scanner, file(File), atom(Atom), []),
			json_parse(atom(Atom), SARIF),
			^^assertion(ground(SARIF)),
			SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
			sarif_dcs_run_ok(Run, _Rules, Properties, _, Results),
			sarif_dcs_run_properties_ok(Properties),
			^^assertion(length(Results, Length)),
			^^assertion(length(Diagnostics, Length))
		)).

	test(sarif_dcs_directory_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		dcs_diagnostics(directory(Directory), Diagnostics, [exclude_predicates([logtalk::trace_event/2])]),
		generate(dead_code_scanner, directory(Directory), atom(Atom), [exclude_predicates([logtalk::trace_event/2])]),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
		sarif_dcs_run_results(Run, Results),
		length(Results, ResultsLength),
		length(Diagnostics, DiagnosticsResults),
		^^assertion(ResultsLength == DiagnosticsResults).

	test(sarif_lr_01, deterministic) :-
		prepare_linter_run(false),
		lr_diagnostics(all, Diagnostics),
		generate(linter_reporter, all, atom(Atom), []),
		assertion(ground(Atom)),
		json_parse(atom(Atom), SARIF),
		SARIF = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_lr_run_ok(Run, Rules, Properties, Results),
		assertion(Rules \== []),
		length(Diagnostics, Count),
		assertion(length(Results, Count)),
		sarif_original_uri_base_ids_ok(Run, 'APP_ROOT'),
		sarif_relative_result_locations_ok(Results, 'APP_ROOT'),
		sarif_lr_run_properties_ok(Properties, Count).

	test(sarif_lr_02, deterministic) :-
		prepare_linter_run(true),
		^^file_path('linter_warnings.sarif', ReportFile),
		generate(linter_reporter, all, file(ReportFile), []),
		assertion(os::file_exists(ReportFile)),
		json_parse(file(ReportFile), SARIF),
		SARIF = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_lr_run_ok(Run, _Rules, _Properties, Results),
		assertion(Results \== []).

	test(sarif_lgtdoc_01, deterministic) :-
		object_property(lgtdoc_diagnostics_fixture(_), file(File)),
		lgtdoc_diagnostics(file(File), Diagnostics),
		generate(lgtdoc, file(File), atom(Atom), []),
		json_parse(atom(Atom), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON),
		JSON = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_lgtdoc_run_ok(Run, Properties, Results),
		assertion(Results \== []),
		length(Diagnostics, Count),
		assertion(length(Results, Count)),
		sarif_original_uri_base_ids_ok(Run, 'APP_ROOT'),
		sarif_relative_result_locations_ok(Results, 'APP_ROOT'),
		sarif_lgtdoc_run_properties_ok(Properties, Count).

	test(sarif_lgtunit_01, deterministic) :-
		object_property(lgtunit_diagnostics_fixture, file(File)),
		lgtunit_diagnostics(file(File), Diagnostics),
		generate(lgtunit, file(File), atom(Atom), []),
		json_parse(atom(Atom), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON),
		JSON = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[Run]},
		sarif_lgtunit_run_ok(Run, Properties, Results),
		length(Diagnostics, Count),
		assertion(length(Results, Count)),
		sarif_original_uri_base_ids_ok(Run, 'APP_ROOT'),
		sarif_relative_result_locations_ok(Results, 'APP_ROOT'),
		sarif_lgtunit_run_properties_ok(Properties, Count).

	test(sarif_aggregate_01, deterministic) :-
		prepare_linter_run(false),
		lr_diagnostics(all, LRDiagnostics),
		dcs_diagnostics(entity(category), DCSDiagnostics),
		sarif::generate([
			tool_spec(linter_reporter, all, []),
			tool_spec(dead_code_scanner, entity(category), [])
		], atom(Atom)),
		json_parse(atom(Atom), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON),
		JSON = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[LRRun, DCSRun]},
		sarif_lr_run_ok(LRRun, LRRules, LRProperties, LRResults),
		assertion(LRRules \== []),
		length(LRDiagnostics, LRCount),
		assertion(length(LRResults, LRCount)),
		assertion(subsumes_term({totalWarnings-LRCount, fingerprintAlgorithm-canonical_warning_v1}, LRProperties)),
		sarif_dcs_run_ok(DCSRun, _DCSRules, DCSProperties, _DCSNotifications, DCSResults),
		length(DCSDiagnostics, DCSCount),
		assertion(length(DCSResults, DCSCount)),
		sarif_lr_run_properties_ok(LRProperties, LRCount),
		sarif_dcs_run_properties_ok(DCSProperties).

	test(sarif_aggregate_02, deterministic) :-
		sarif::generate(dead_code_scanner, entity(category), atom(SingleAtom), []),
		json_parse(atom(SingleAtom), Single),
		sarif::generate([tool_spec(dead_code_scanner, entity(category), [])], atom(AggregateAtom)),
		json_parse(atom(AggregateAtom), Aggregate),
		Single = {'$schema'-Schema, version-Version, runs-[SingleRun]},
		Aggregate = {'$schema'-Schema, version-Version, runs-[AggregateRun]},
		sarif_dcs_run_ok(SingleRun, SingleRules, SingleProperties, SingleNotifications, SingleResults),
		sarif_dcs_run_ok(AggregateRun, AggregateRules, AggregateProperties, AggregateNotifications, AggregateResults),
		assertion(SingleRules == AggregateRules),
		assertion(SingleProperties == AggregateProperties),
		assertion(SingleNotifications == AggregateNotifications),
		assertion(SingleResults == AggregateResults).

	test(sarif_aggregate_03, deterministic) :-
		object_property(lgtunit, file(File)),
		dcs_diagnostics(file(File), Diagnostics),
		sarif::generate([
			tool_spec(dead_code_scanner, entity(category), [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
			tool_spec(dead_code_scanner, file(File), [])
		], atom(Atom)),
		json_parse(atom(Atom), JSON),
		JSON = {'$schema'-_, version-'2.1.0', runs-[EntityRun, FileRun]},
		sarif_dcs_run_results(EntityRun, EntityResults),
		^^assertion(subsumes_term([_, _, _], EntityResults)),
		sarif_dcs_run_ok(FileRun, _Rules, Properties, _Notifications, Results),
		sarif_dcs_run_properties_ok(Properties),
		length(Diagnostics, Count),
		assertion(length(Results, Count)).

	test(sarif_aggregate_04, deterministic) :-
		prepare_linter_run(false),
		^^file_path('aggregate_report.sarif', ReportFile),
		sarif::generate([
			tool_spec(linter_reporter, all, []),
			tool_spec(dead_code_scanner, entity(category), [])
		], file(ReportFile)),
		assertion(os::file_exists(ReportFile)),
		json_parse(file(ReportFile), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON),
		JSON = {'$schema'-'https://json.schemastore.org/sarif-2.1.0.json', version-'2.1.0', runs-[LRRun, DCSRun]},
		sarif_lr_run_ok(LRRun, _LRRules, _LRProperties, LRResults),
		assertion(LRResults \== []),
		sarif_dcs_run_ok(DCSRun, _DCSRules, _DCSProperties, _DCSNotifications, DCSResults),
		assertion(DCSResults \== []).

	test(sarif_schema_01, deterministic) :-
		generate(dead_code_scanner, entity(category), atom(Atom), []),
		json_parse(atom(Atom), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON).

	test(sarif_schema_02, deterministic) :-
		prepare_linter_run(false),
		generate(linter_reporter, all, atom(Atom), []),
		json_parse(atom(Atom), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON).

	test(sarif_schema_03, deterministic) :-
		prepare_linter_run(true),
		^^file_path('linter_warnings.sarif', ReportFile),
		generate(linter_reporter, all, file(ReportFile), []),
		assertion(os::file_exists(ReportFile)),
		json_parse(file(ReportFile), JSON),
		sarif_schema(Schema),
		json_schema_validate(Schema, JSON).

	test(sarif_error_01, error(domain_error(diagnostics_tool, sarif))) :-
		generate(sarif, all, atom(_), []).

	test(sarif_error_02, error(type_error(object_identifier, 42))) :-
		sarif::generate([tool_spec(dead_code_scanner, entity(42), [])], atom(_)).

	prepare_linter_run(Explanations) :-
		^^file_path('linter_warnings.sarif', ReportFile),
		^^clean_file(ReportFile),
		reset,
		enable([explanations(Explanations)]),
		logtalk_load([errors(warnings), errors(main_include_compiler_warning)], [reload(always)]),
		disable.

	sarif_schema(Schema) :-
		^^file_path('sarif-schema-2.1.0.json', Path),
		json_schema_parse(file(Path), Schema).

	sarif_dcs_run_properties_ok(Properties) :-
		json_object_member(Properties, fingerprintAlgorithm, canonical_finding_v1).

	sarif_dcs_run_ok(Run, Rules, Properties, Notifications, Results) :-
		dead_code_scanner::diagnostics_tool(_, _, Version, _, _),
		json_object_member(Run, tool, Tool),
		json_object_member(Tool, driver, Driver),
		json_object_member(Driver, name, dead_code_scanner),
		json_object_member(Driver, informationUri, 'https://logtalk.org/'),
		json_object_member(Driver, version, Version),
		json_object_member(Driver, guid, '91f50eb3-a092-43b5-b8e2-3c1f64bb7047'),
		json_object_member(Driver, rules, Rules),
		json_object_member(Run, automationDetails, AutomationDetails),
		json_object_member(AutomationDetails, id, _),
		json_object_member(AutomationDetails, guid, _),
		json_object_member(Run, invocations, [Invocation]),
		sarif_dcs_invocation_ok(Invocation, Notifications),
		( 	json_object_member(Run, versionControlProvenance, [VersionControlDetails| _]) ->
			sarif_dcs_version_control_details_ok(VersionControlDetails)
		; 	true
		),
		json_object_member(Run, properties, Properties),
		json_object_member(Run, results, Results).

	sarif_dcs_rule_ok([Rule| _], RuleId, Rule) :-
		Rule = {id-RuleId, guid-_, name-RuleId, shortDescription-_, fullDescription-_, defaultConfiguration-{level-_}},
		!.
	sarif_dcs_rule_ok([_| Rules], RuleId, Rule) :-
		sarif_dcs_rule_ok(Rules, RuleId, Rule).

	sarif_dcs_result_ok([Result| _], RuleId, RuleIndex, Level, Confidence, Result) :-
		Result = {
			ruleId-RuleId,
			ruleIndex-RuleIndex,
			level-Level,
			message-{text-_},
			locations-_,
			partialFingerprints-{entityPredicateV1-_, locationV1-_},
			fingerprints-{canonicalFindingV1-_},
			properties-{class-RuleId, confidence-Confidence, findingProperties-_, entityKind-_, entity-_, predicate-_}
		},
		!.
	sarif_dcs_result_ok([_| Results], RuleId, RuleIndex, Level, Confidence, Result) :-
		sarif_dcs_result_ok(Results, RuleId, RuleIndex, Level, Confidence, Result).

	sarif_dcs_invocation_ok({executionSuccessful- @true}, []) :-
		!.
	sarif_dcs_invocation_ok({executionSuccessful- @true, toolExecutionNotifications-Notifications}, Notifications).

	sarif_dcs_run_results(Run, Results) :-
		json_object_member(Run, results, Results).

	sarif_dcs_version_control_details_ok({
		repositoryUri-RepositoryURI,
		revisionId-RevisionId,
		branch-_,
		mappedTo-{uri-MappedToURI}
	}) :-
		absolute_uri(RepositoryURI),
		RevisionId \== '',
		absolute_uri(MappedToURI).

	absolute_uri(URI) :-
		valid_url(URI),
		sub_atom(URI, _, 3, _, '://'),
		!.

	sarif_lr_run_ok(Run, Rules, Properties, Results) :-
		linter_reporter::diagnostics_tool(_, _, Version, _, _),
		json_object_member(Run, tool, Tool),
		json_object_member(Tool, driver, Driver),
		json_object_member(Driver, name, linter_reporter),
		json_object_member(Driver, informationUri, 'https://logtalk.org/'),
		json_object_member(Driver, version, Version),
		json_object_member(Driver, guid, _),
		json_object_member(Driver, rules, Rules),
		json_object_member(Run, automationDetails, AutomationDetails),
		json_object_member(AutomationDetails, id, linter_reporter),
		json_object_member(AutomationDetails, guid, _),
		json_object_member(Run, properties, Properties),
		json_object_member(Run, results, Results).

	sarif_lr_run_properties_ok(Properties, Count) :-
		json_object_member(Properties, totalWarnings, Count),
		json_object_member(Properties, fingerprintAlgorithm, canonical_warning_v1),
		\+ json_object_member(Properties, gitBranch, _),
		\+ json_object_member(Properties, gitCommitHash, _).

	sarif_lgtdoc_run_ok(Run, Properties, Results) :-
		lgtdoc::diagnostics_tool(_, _, Version, _, _),
		json_object_member(Run, tool, Tool),
		json_object_member(Tool, driver, Driver),
		json_object_member(Driver, name, lgtdoc),
		json_object_member(Driver, informationUri, 'https://logtalk.org/'),
		json_object_member(Driver, version, Version),
		json_object_member(Driver, guid, _),
		json_object_member(Driver, rules, _),
		json_object_member(Run, automationDetails, AutomationDetails),
		json_object_member(AutomationDetails, id, _),
		json_object_member(AutomationDetails, guid, _),
		json_object_member(Run, invocations, [Invocation]),
		sarif_lgtdoc_invocation_ok(Invocation),
		json_object_member(Run, properties, Properties),
		json_object_member(Run, results, Results).

	sarif_lgtdoc_invocation_ok({executionSuccessful- @true, toolExecutionNotifications-[]}) :-
		!.
	sarif_lgtdoc_invocation_ok({executionSuccessful- @true}).

	sarif_lgtdoc_run_properties_ok(Properties, Count) :-
		json_object_member(Properties, diagnosticsCount, Count).

	sarif_lgtunit_run_ok(Run, Properties, Results) :-
		lgtunit::diagnostics_tool(_, _, Version, _, _),
		json_object_member(Run, tool, Tool),
		json_object_member(Tool, driver, Driver),
		json_object_member(Driver, name, lgtunit),
		json_object_member(Driver, informationUri, 'https://logtalk.org/'),
		json_object_member(Driver, version, Version),
		json_object_member(Driver, guid, _),
		json_object_member(Driver, rules, _),
		json_object_member(Run, automationDetails, AutomationDetails),
		json_object_member(AutomationDetails, id, _),
		json_object_member(AutomationDetails, guid, _),
		json_object_member(Run, invocations, [Invocation]),
		sarif_lgtunit_invocation_ok(Invocation),
		json_object_member(Run, properties, Properties),
		json_object_member(Run, results, Results).

	sarif_lgtunit_invocation_ok({executionSuccessful- @true}).

	sarif_lgtunit_run_properties_ok(Properties, Count) :-
		json_object_member(Properties, totalWarnings, Count),
		json_object_member(Properties, fingerprintAlgorithm, canonical_warning_v1).

	sarif_original_uri_base_ids_ok(Run, BaseId) :-
		json_object_member(Run, originalUriBaseIds, OriginalURIBaseIds),
		json_object_member(OriginalURIBaseIds, BaseId, BaseInfo),
		json_object_member(BaseInfo, uri, URI),
		absolute_uri(URI).

	sarif_relative_result_locations_ok([], _BaseId).
	sarif_relative_result_locations_ok([Result| Results], BaseId) :-
		json_object_member(Result, locations, [Location| _]),
		json_object_member(Location, physicalLocation, PhysicalLocation),
		json_object_member(PhysicalLocation, artifactLocation, ArtifactLocation),
		json_object_member(ArtifactLocation, uriBaseId, BaseId),
		json_object_member(ArtifactLocation, uri, URI),
		relative_uri(URI),
		sarif_relative_result_locations_ok(Results, BaseId).

	sarif_location_fingerprints_ok([], _BaseId, _ApplicationRoot).
	sarif_location_fingerprints_ok([Result| Results], BaseId, ApplicationRoot) :-
		json_object_member(Result, partialFingerprints, PartialFingerprints),
		( 	json_object_member(PartialFingerprints, locationV1, LocationFingerprint) ->
			true
		; 	json_object_member(PartialFingerprints, ruleLocationV1, LocationFingerprint)
		),
		relative_fingerprint_ok(LocationFingerprint, BaseId, ApplicationRoot),
		json_object_member(Result, fingerprints, Fingerprints),
		( 	json_object_member(Fingerprints, canonicalFindingV1, CanonicalFingerprint) ->
			true
		; 	json_object_member(Fingerprints, canonicalWarningV1, CanonicalFingerprint)
		),
		relative_fingerprint_ok(CanonicalFingerprint, BaseId, ApplicationRoot),
		sarif_location_fingerprints_ok(Results, BaseId, ApplicationRoot).

	relative_fingerprint_ok(Fingerprint, BaseId, ApplicationRoot) :-
		sub_atom(Fingerprint, _, _, _, BaseId),
		!,
		\+ sub_atom(Fingerprint, _, _, _, ApplicationRoot).

	relative_uri(URI) :-
		URI \== '',
		\+ absolute_uri(URI),
		\+ sub_atom(URI, 0, 1, _, '/').

	json_object_member(Object, Key, Value) :-
		compound(Object),
		functor(Object, '{}', 1),
		arg(1, Object, Pairs),
		json_pairs_member(Pairs, Key, Value).

	json_pairs_member((Key-Value, _Pairs), Key, Value) :-
		!.
	json_pairs_member((_Pair, Pairs), Key, Value) :-
		json_pairs_member(Pairs, Key, Value).
	json_pairs_member(Key-Value, Key, Value).

	clean_xml_docs :-
		^^file_path('xml_docs', XMLDocsDirectory),
		(  os::directory_exists(XMLDocsDirectory) ->
			os::directory_files(XMLDocsDirectory, XMLFiles, [paths(absolute), extensions(['.xml'])]),
			forall(
				list::member(XMLFile, XMLFiles),
				os::delete_file(XMLFile)
			),
			os::delete_directory(XMLDocsDirectory)
		;  true
		).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, lgtdoc, _Tokens).

:- end_object.
