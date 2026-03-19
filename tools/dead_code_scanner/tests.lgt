%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 2016-2023 Paulo Moura <pmoura@logtalk.org>
%  SPDX-FileCopyrightText: 2016 Barry Evans <barryevans@kyndi.com>
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
		version is 0:8:0,
		author is 'Barry Evans and Paulo Moura',
		date is 2026-03-19,
		comment is 'Unit tests for the "dead_code_scanner" tool.'
	]).

	cover(dead_code_scanner).

	:- uses(dead_code_scanner, [
		findings/2, findings/3, finding/2, finding/3,
		summary/2, summary/3, preflight/2, preflight/3,
		predicates/2, predicates/3, predicate/2, predicate/3,
		all/0,
		rlibrary/1, library/1,
		rdirectory/2, rdirectory/1, directory/1,
		file/1,
		entity/2, entity/1
	]).

	:- uses(list, [
		length/2, member/2, memberchk/2
	]).

	:- uses(json, [
		parse/2 as json_parse/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(url(atom), [
		valid/1 as valid_url/1
	]).

	% category tests

	test(dcs_stand_alone_category_01, deterministic) :-
		predicates(stand_alone_category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_category_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_01, deterministic) :-
		predicates(category, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_category_02, deterministic) :-
		setof(Predicate, predicate(category, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_findings_entity_01, deterministic) :-
		findings(entity(category), Findings),
		setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(category, category, Predicate, Finding)), Predicates),
		setof(Class-Confidence, Predicate^File^Lines^member(dead_predicate(Class, Confidence, _Properties, category, category, Predicate, File, Lines), Findings), Metadata),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]),
		^^assertion(Metadata == [local_dead_code-medium]).

	test(dcs_findings_entity_02, deterministic) :-
		findings(entity(category), Findings, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(category, category, Predicate, Finding)), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_findings_entity_03, deterministic) :-
		findings(entity(category), Findings, [waive_findings([
			dead_predicate(local_dead_code, medium, _, category, category, dead_predicate/0, _, _),
			dead_predicate(local_dead_code, medium, _, category, category, dead_non_terminal//0, _, _)
		])]),
		setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(category, category, Predicate, Finding)), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_finding_entity_01, deterministic) :-
		setof(Finding, finding(entity(category), Finding), Findings),
		setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(category, category, Predicate, Finding)), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_finding_entity_02, deterministic) :-
		setof(Finding, finding(entity(category), Finding, [waive_findings([dead_predicate(local_dead_code, medium, _, category, category, dead_predicate_1/0, _, _)])]), Findings),
		setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(category, category, Predicate, Finding)), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_summary_entity_01, deterministic) :-
		summary(entity(category), Summary),
		^^assertion(Summary == summary(entity(category), 1, 5, finding_breakdown([class_count(local_dead_code, 5)], [confidence_count(medium, 5)]), [entity_summary(category, category, 5, finding_breakdown([class_count(local_dead_code, 5)], [confidence_count(medium, 5)]))])).

	test(dcs_summary_entity_02, deterministic) :-
		summary(entity(category), Summary, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		^^assertion(Summary == summary(entity(category), 1, 3, finding_breakdown([class_count(local_dead_code, 3)], [confidence_count(medium, 3)]), [entity_summary(category, category, 3, finding_breakdown([class_count(local_dead_code, 3)], [confidence_count(medium, 3)]))])).

	test(dcs_summary_entity_03, deterministic) :-
		summary(entity(category), Summary, [waive_findings([dead_predicate(local_dead_code, medium, _, category, category, dead_predicate_1/0, _, _)])]),
		^^assertion(Summary == summary(entity(category), 1, 4, finding_breakdown([class_count(local_dead_code, 4)], [confidence_count(medium, 4)]), [entity_summary(category, category, 4, finding_breakdown([class_count(local_dead_code, 4)], [confidence_count(medium, 4)]))])).

	test(dcs_export_json_entity_01, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom)),
		json_parse(atom(Atom), JSON),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-entity, value-category},
			options-_,
			preflight-{warnings-PreflightWarnings},
			summary-{totalEntities-1, totalFindings-5, breakdown-BreakdownJSON, entities-[EntitySummaryJSON]},
			findings-Findings
		},
		^^assertion(PreflightWarnings = [_]),
		^^assertion(BreakdownJSON == {classes-[{class-local_dead_code, findingsCount-5}], confidences-[{confidence-medium, findingsCount-5}]}),
		^^assertion(EntitySummaryJSON == {kind-category, entity-category, findingsCount-5, breakdown-{classes-[{class-local_dead_code, findingsCount-5}], confidences-[{confidence-medium, findingsCount-5}]}}),
		Findings = [FirstFinding| _],
		^^assertion(subsumes_term({kind-dead_predicate, class-local_dead_code, confidence-medium, properties-_, entityKind-category, entity-category, predicate-_, file-_, lines-{start-_, end-_}}, FirstFinding)),
		^^assertion(subsumes_term([_, _, _, _, _], Findings)).

	test(dcs_export_json_entity_02, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom), [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		json_parse(atom(Atom), JSON),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-entity, value-category},
			options-_,
			preflight-{warnings-PreflightWarnings},
			summary-{totalEntities-1, totalFindings-3, breakdown-BreakdownJSON, entities-[EntitySummaryJSON]},
			findings-Findings
		},
		^^assertion(PreflightWarnings = [_]),
		^^assertion(BreakdownJSON == {classes-[{class-local_dead_code, findingsCount-3}], confidences-[{confidence-medium, findingsCount-3}]}),
		^^assertion(EntitySummaryJSON == {kind-category, entity-category, findingsCount-3, breakdown-{classes-[{class-local_dead_code, findingsCount-3}], confidences-[{confidence-medium, findingsCount-3}]}}),
		^^assertion(subsumes_term([_, _, _], Findings)).

	test(dcs_export_json_01, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom)),
		json_parse(atom(Atom), JSON),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-entity, value-category},
			options-{excludeDirectories-[], excludeFiles-[], excludeEntities-[], excludePredicates-[], excludeLibraries-[startup, scratch_directory], waiveFindings-[], validateExport- @false},
			preflight-{warnings-PreflightWarnings},
			summary-{totalEntities-1, totalFindings-5, breakdown-BreakdownJSON, entities-[EntitySummaryJSON]},
			findings-Findings
		},
		^^assertion(PreflightWarnings = [_]),
		^^assertion(BreakdownJSON == {classes-[{class-local_dead_code, findingsCount-5}], confidences-[{confidence-medium, findingsCount-5}]}),
		^^assertion(EntitySummaryJSON == {kind-category, entity-category, findingsCount-5, breakdown-{classes-[{class-local_dead_code, findingsCount-5}], confidences-[{confidence-medium, findingsCount-5}]}}),
		^^assertion(subsumes_term([_, _, _, _, _], Findings)).

	test(dcs_export_json_02, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom), [waive_findings([dead_predicate(local_dead_code, medium, _, category, category, dead_predicate_1/0, _, _)])]),
		json_parse(atom(Atom), JSON),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-entity, value-category},
			options-{excludeDirectories-[], excludeFiles-[], excludeEntities-[], excludePredicates-[], excludeLibraries-[startup, scratch_directory], waiveFindings-['dead_predicate(local_dead_code,medium,A,category,category,dead_predicate_1/0,B,C)'], validateExport- @false},
			preflight-{warnings-PreflightWarnings},
			summary-{totalEntities-1, totalFindings-4, breakdown-BreakdownJSON, entities-[EntitySummaryJSON]},
			findings-Findings
		},
		^^assertion(PreflightWarnings = [_]),
		^^assertion(BreakdownJSON == {classes-[{class-local_dead_code, findingsCount-4}], confidences-[{confidence-medium, findingsCount-4}]}),
		^^assertion(EntitySummaryJSON = {kind-category, entity-category, findingsCount-4, breakdown-{classes-[{class-local_dead_code, findingsCount-4}], confidences-[{confidence-medium, findingsCount-4}]}}),
		^^assertion(subsumes_term([_, _, _, _], Findings)).

	test(dcs_export_json_03, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom), [validate_export(true)]),
		json_parse(atom(Atom), JSON),
		^^assertion(subsumes_term({formatVersion-_, tool-_, target-_, options-{excludeDirectories-_, excludeFiles-_, excludeEntities-_, excludePredicates-_, excludeLibraries-_, waiveFindings-_, validateExport- @true}, preflight-_, summary-_, findings-_}, JSON)).

	test(dcs_export_json_schema_01, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom)),
		json_parse(atom(Atom), JSON),
		schema_path(Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	test(dcs_export_json_schema_02, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom), [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		json_parse(atom(Atom), JSON),
		schema_path(Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	test(dcs_export_json_schema_03, deterministic) :-
		dead_code_scanner::export(entity(category), json, atom(Atom), [validate_export(true)]),
		json_parse(atom(Atom), JSON),
		schema_path(Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	test(dcs_export_json_file_01, deterministic) :-
		object_property(lgtunit, file(File)),
		findings(file(File), Findings),
		summary(file(File), summary(file(File), TotalEntities, TotalFindings, _, _)),
		dead_code_scanner::export(file(File), json, atom(Atom)),
		json_parse(atom(Atom), JSON),
		^^assertion(ground(JSON)),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-file, value-File},
			options-_,
			preflight-_,
			summary-{totalEntities-TotalEntities, totalFindings-TotalFindings, breakdown-_, entities-EntitiesJSON},
			findings-FindingsJSON
		},
		length(Findings, TotalFindings),
		^^assertion(length(FindingsJSON, TotalFindings)),
		^^assertion(length(EntitiesJSON, TotalEntities)).

	test(dcs_export_json_directory_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		findings(directory(Directory), Findings),
		summary(directory(Directory), summary(directory(Directory), TotalEntities, TotalFindings, _, _)),
		dead_code_scanner::export(directory(Directory), json, atom(Atom), [validate_export(true)]),
		json_parse(atom(Atom), JSON),
		^^assertion(ground(JSON)),
		JSON = {
			formatVersion-'1.0.0',
			tool-dead_code_scanner,
			target-{kind-directory, value-Directory},
			options-{excludeDirectories-_, excludeFiles-_, excludeEntities-_, excludePredicates-_, excludeLibraries-_, waiveFindings-_, validateExport- @true},
			preflight-_,
			summary-{totalEntities-TotalEntities, totalFindings-TotalFindings, breakdown-_, entities-EntitiesJSON},
			findings-FindingsJSON
		},
		^^assertion(ground(JSON)),
		length(Findings, TotalFindings),
		^^assertion(length(FindingsJSON, TotalFindings)),
		^^assertion(length(EntitiesJSON, TotalEntities)).

	test(dcs_export_sarif_01, deterministic) :-
		dead_code_scanner::export(entity(category), sarif, atom(Atom)),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {
			'$schema'-'https://json.schemastore.org/sarif-2.1.0.json',
			version-'2.1.0',
			runs-[Run]
		},
		sarif_run_ok(Run, Rules, Properties, Notifications, Results),
		sarif_rule_ok(Rules, local_dead_code, Rule),
		^^assertion(subsumes_term({
			id-local_dead_code,
			guid-'f6fd0e53-0c2d-45fd-a6dd-7b2f2af3e2a1',
			name-local_dead_code,
			shortDescription-{text-_},
			fullDescription-{text-_},
			defaultConfiguration-{level-warning}
		}, Rule)),
		sarif_run_properties_ok(Properties),
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
		^^assertion(subsumes_term([_, _, _, _, _], Results)).

	test(dcs_export_sarif_03, deterministic) :-
		dead_code_scanner::export(entity(predicate_directives), sarif, atom(Atom)),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
		sarif_run_ok(Run, Rules, Properties, Notifications, Results),
		sarif_run_properties_ok(Properties),
		^^assertion(subsumes_term([_], Notifications)),
		sarif_result_ok(Results, unused_uses_resource, 1, error, high, _),
		( 	current_logtalk_flag(modules, supported) ->
			sarif_rule_ok(Rules, unused_uses_resource, UsesRule),
			sarif_rule_ok(Rules, unused_use_module_resource, UseModuleRule),
			sarif_result_ok(Results, unused_use_module_resource, 2, error, high, _),
			^^assertion(subsumes_term({id-unused_uses_resource, guid-_, name-unused_uses_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UsesRule)),
			^^assertion(subsumes_term({id-unused_use_module_resource, guid-_, name-unused_use_module_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UseModuleRule)),
			^^assertion(length(Results, 5))
		; 	sarif_rule_ok(Rules, unused_uses_resource, UsesRule),
			^^assertion(subsumes_term({id-unused_uses_resource, guid-_, name-unused_uses_resource, shortDescription-_, fullDescription-_, defaultConfiguration-{level-error}}, UsesRule)),
			^^assertion(length(Results, 3))
		).

	test(dcs_export_sarif_02, deterministic) :-
		dead_code_scanner::export(entity(category), sarif, atom(Atom), [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-_, runs-[Run]},
		sarif_run_results(Run, Results),
		^^assertion(subsumes_term([_, _, _], Results)),
		!.

	test(dcs_export_sarif_file_01, deterministic) :-
		object_property(lgtunit, file(File)),
		once((
			findings(file(File), Findings),
			dead_code_scanner::export(file(File), sarif, atom(Atom)),
			json_parse(atom(Atom), SARIF),
			^^assertion(ground(SARIF)),
			SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
			sarif_run_ok(Run, _Rule, Properties, _, Results),
			sarif_run_properties_ok(Properties),
			^^assertion(length(Results, Length)),
			^^assertion(length(Findings, Length))
		)).

	test(dcs_export_sarif_directory_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		findings(directory(Directory), Findings, [exclude_predicates([logtalk::trace_event/2])]),
		dead_code_scanner::export(directory(Directory), sarif, atom(Atom), [exclude_predicates([logtalk::trace_event/2])]),
		json_parse(atom(Atom), SARIF),
		^^assertion(ground(SARIF)),
		SARIF = {'$schema'-_, version-'2.1.0', runs-[Run]},
		sarif_run_results(Run, Results),
		length(Results, ResultsLength),
		length(Findings, FindingsResults),
		^^assertion(ResultsLength == FindingsResults).

	test(dcs_category_03, deterministic) :-
		predicates(category, Predicates, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	test(dcs_category_04, deterministic) :-
		setof(Predicate, predicate(category, Predicate, [exclude_predicates([dead_predicate/0, dead_non_terminal//0])]), Predicates),
		^^assertion(Predicates == [
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0
		]).

	% prototype tests

	test(dcs_stand_alone_prototype_01, deterministic) :-
		predicates(stand_alone_prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_prototype_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_01, deterministic) :-
		predicates(prototype, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_prototype_02, deterministic) :-
		setof(Predicate, predicate(prototype, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% class and instance tests

	test(dcs_stand_alone_class_01, deterministic) :-
		predicates(stand_alone_class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_stand_alone_class_02, deterministic) :-
		setof(Predicate, predicate(stand_alone_class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_01, deterministic) :-
		predicates(class, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_class_02, deterministic) :-
		setof(Predicate, predicate(class, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_01, deterministic) :-
		predicates(subclass_with_metaclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_with_metaclass_02, deterministic) :-
		setof(Predicate, predicate(subclass_with_metaclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_01, deterministic) :-
		predicates(subclass, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_subclass_02, deterministic) :-
		setof(Predicate, predicate(subclass, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_01, deterministic) :-
		predicates(instance, Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	test(dcs_instance_02, deterministic) :-
		setof(Predicate, predicate(instance, Predicate), Predicates),
		^^assertion(Predicates == [
			dead_predicate/0,
			dead_predicate_1/0, dead_predicate_2/0, dead_predicate_3/0,
			dead_non_terminal//0
		]).

	% the following tests ony check (for now) that the called
	% predicates succeed as expected and are deterministic

	test(dcs_predicates_2_01, deterministic) :-
		predicates(lgtunit, _).

	test(dcs_entity_1_01, deterministic) :-
		entity(lgtunit).

	test(dcs_entity_2_01, deterministic) :-
		entity(category, [exclude_predicates([dead_predicate/0])]).

	test(dcs_entity_1_02, deterministic) :-
		entity(lgtunit_messages).

	test(dcs_all_0_01, deterministic) :-
		all.

	test(dcs_library_1_01, deterministic) :-
		library(lgtunit).

	test(dcs_rlibrary_1_01, deterministic) :-
		rlibrary(lgtunit).

	test(dcs_file_1_01, deterministic) :-
		object_property(lgtunit, file(File)),
		file(File).

	test(dcs_directory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		directory(Directory).

	test(dcs_rdirectory_1_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory).

	% tests for non-called predicates listed in uses/2 and use_module/2 directives

	:- if(current_logtalk_flag(modules, supported)).

		test(dcs_uses_directive_01, deterministic) :-
			setof(Predicate, predicate(predicate_directives, Predicate), Predicates),
			^^assertion(Predicates == [some_module:bar/2, some_module:baz/2, list::app/3, list::member/2, logtalk::dbg/1]).

		test(dcs_findings_uses_directive_01, deterministic) :-
			setof(Finding, finding(entity(predicate_directives), Finding), Findings),
			setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(object, predicate_directives, Predicate, Finding)), Predicates),
			setof(Classification, Finding^(member(Finding, Findings), finding_classification(Finding, Classification)), ClassifiedPredicates),
			^^assertion(Predicates == [some_module:bar/2, some_module:baz/2, list::app/3, list::member/2, logtalk::dbg/1]),
			^^assertion(ClassifiedPredicates == [classification(unused_use_module_resource, some_module:bar/2), classification(unused_use_module_resource, some_module:baz/2), classification(unused_uses_resource, list::app/3), classification(unused_uses_resource, list::member/2), classification(unused_uses_resource, logtalk::dbg/1)]).

	:- else.

		test(dcs_uses_directive_01, deterministic) :-
			setof(Predicate, predicate(predicate_directives, Predicate), Predicates),
			^^assertion(Predicates == [list::app/3, list::member/2, logtalk::dbg/1]).

		test(dcs_findings_uses_directive_01, deterministic) :-
			setof(Finding, finding(entity(predicate_directives), Finding), Findings),
			setof(Predicate, Finding^(member(Finding, Findings), finding_predicate(object, predicate_directives, Predicate, Finding)), Predicates),
			setof(Classification, Finding^(member(Finding, Findings), finding_classification(Finding, Classification)), ClassifiedPredicates),
			^^assertion(Predicates == [list::app/3, list::member/2, logtalk::dbg/1]),
			^^assertion(ClassifiedPredicates == [classification(unused_uses_resource, list::app/3), classification(unused_uses_resource, list::member/2), classification(unused_uses_resource, logtalk::dbg/1)]).

	:- endif.

	% tests for option validation

	test(dcs_exclude_directories_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_directories([foo, bar])]).

	test(dcs_exclude_files_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_files([foo, bar])]).

	test(dcs_exclude_libraries_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_libraries([foo, bar])]).

	test(dcs_exclude_entities_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_entities([foo, bar])]).

	test(dcs_exclude_predicates_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [exclude_predicates([foo/1, bar//2])]).

	test(dcs_waive_findings_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [waive_findings([dead_predicate(local_dead_code, medium, _, object, foo, bar/1, _, _)])]).

	test(dcs_validate_export_option_01, deterministic) :-
		logtalk::expand_library_path(lgtunit, Directory),
		rdirectory(Directory, [validate_export(true)]).

	test(dcs_preflight_01, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		preflight(file(test_entities), Warnings),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		^^assertion(Warnings == []).

	test(dcs_preflight_02, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, off),
		set_logtalk_flag(optimize, off),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		preflight(file(test_entities), Warnings),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		^^assertion(subsumes_term([
			missing_analysis_prerequisite(_, optimize),
			missing_analysis_prerequisite(_, source_data)
		], Warnings)).

	test(dcs_preflight_warning_01, deterministic) :-
		retractall(preflight_warning(_)),
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		file(test_entities),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		\+ preflight_warning(_).

	test(dcs_preflight_warning_02, deterministic) :-
		retractall(preflight_warning(_)),
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, off),
		set_logtalk_flag(optimize, off),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		file(test_entities),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		setof(Warning, preflight_warning(Warning), Warnings),
		^^assertion(subsumes_term([
			missing_analysis_prerequisite(_, optimize),
			missing_analysis_prerequisite(_, source_data)
		], Warnings)).

	test(dcs_local_dead_code_confidence_01, deterministic) :-
		current_logtalk_flag(source_data, OldSourceData),
		current_logtalk_flag(optimize, OldOptimize),
		set_logtalk_flag(source_data, on),
		set_logtalk_flag(optimize, on),
		logtalk_load(test_entities, [reload(always), unknown_entities(silent)]),
		findings(file(test_entities), Findings),
		set_logtalk_flag(source_data, OldSourceData),
		set_logtalk_flag(optimize, OldOptimize),
		logtalk_load(test_entities, [reload(always), source_data(on), unknown_entities(silent)]),
		memberchk(dead_predicate(local_dead_code, high, Properties, category, category, dead_predicate/0, _, _), Findings),
		^^assertion(member(optimize(on), Properties)).

	% suppress all messages from the "dead_code_scanner"
	% component to not pollute the unit tests output

	:- private(preflight_warning/1).
	:- dynamic(preflight_warning/1).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _Kind, dead_code_scanner, _Tokens) :-
		Message = missing_analysis_prerequisite(_, _),
		assertz(preflight_warning(Message)).
	logtalk::message_hook(_Message, _Kind, dead_code_scanner, _Tokens).

	schema_path(Path) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'dead_code_scanner.schema.json', Path).

	sarif_run_properties_ok({
		fingerprintAlgorithm-canonical_finding_v1,
		gitBranch-_,
		gitCommitHash-_
	}) :-
		!.
	sarif_run_properties_ok({
		fingerprintAlgorithm-canonical_finding_v1
	}).

	sarif_run_ok({
		tool-{driver-{name-dead_code_scanner, informationUri-'https://logtalk.org/', version-'0.22.0', guid-'91f50eb3-a092-43b5-b8e2-3c1f64bb7047', rules-Rules}},
		automationDetails-{id-_, guid-_},
		invocations-[Invocation],
		versionControlProvenance-[VersionControlDetails| _],
		properties-Properties,
		results-Results
	}, Rules, Properties, Notifications, Results) :-
		sarif_invocation_ok(Invocation, Notifications),
		sarif_version_control_details_ok(VersionControlDetails),
		!.
	sarif_run_ok({
		tool-{driver-{name-dead_code_scanner, informationUri-'https://logtalk.org/', version-'0.22.0', guid-'91f50eb3-a092-43b5-b8e2-3c1f64bb7047', rules-Rules}},
		automationDetails-{id-_, guid-_},
		invocations-[Invocation],
		properties-Properties,
		results-Results
	}, Rules, Properties, Notifications, Results) :-
		sarif_invocation_ok(Invocation, Notifications),
		!.

	sarif_rule_ok([Rule| _], RuleId, Rule) :-
		Rule = {id-RuleId, guid-_, name-RuleId, shortDescription-_, fullDescription-_, defaultConfiguration-{level-_}},
		!.
	sarif_rule_ok([_| Rules], RuleId, Rule) :-
		sarif_rule_ok(Rules, RuleId, Rule).

	sarif_result_ok([Result| _], RuleId, RuleIndex, Level, Confidence, Result) :-
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
	sarif_result_ok([_| Results], RuleId, RuleIndex, Level, Confidence, Result) :-
		sarif_result_ok(Results, RuleId, RuleIndex, Level, Confidence, Result).

	sarif_invocation_ok({executionSuccessful- @true}, []) :-
		!.
	sarif_invocation_ok({executionSuccessful- @true, toolExecutionNotifications-Notifications}, Notifications).

	sarif_run_results({
		tool-_,
		automationDetails-{id-_, guid-_},
		invocations-_,
		versionControlProvenance-_,
		properties-_,
		results-Results
	}, Results) :-
		!.
	sarif_run_results({
		tool-_,
		automationDetails-{id-_, guid-_},
		invocations-_,
		properties-_,
		results-Results
	}, Results).

	sarif_version_control_details_ok({
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

	finding_predicate(EntityKind, Entity, Predicate, dead_predicate(_Class, _Confidence, _Properties, EntityKind, Entity, Predicate, _File, _Lines)).

	finding_classification(dead_predicate(Class, _Confidence, _Properties, _EntityKind, _Entity, Predicate, _File, _Lines), classification(Class, Predicate)).

:- end_object.
