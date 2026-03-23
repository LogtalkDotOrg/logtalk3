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
		date is 2026-03-23,
		comment is 'Unit tests for the "sbom" tool.'
	]).

	cover(sbom).

	:- uses(sbom, [
		document/1, document/2,
		export/1, export/2
	]).

	:- uses(list, [
		memberchk/2
	]).

	:- uses(json, [
		parse/2 as json_parse/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(os, [
		decompose_file_name/3,
		path_concat/3
	]).

	test(sbom_document_01, deterministic) :-
		document(Document),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-DocumentNamespace,
			creationInfo-{created-Created, creators-['Logtalk "sbom" tool']},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(atom(DocumentNamespace)),
		^^assertion(atom(Created)),
		^^assertion(ground(Packages)),
		^^assertion(ground(Relationships)),
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-'loaded-application',
			versionInfo-'0.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			primaryPackagePurpose-'APPLICATION',
			summary-_
		}, Packages),
		memberchk({
			'SPDXID'-'SPDXRef-Logtalk',
			name-logtalk,
			versionInfo-_,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Logtalk runtime'
		}, Packages),
		memberchk({
			'SPDXID'-'SPDXRef-Backend',
			name-_,
			versionInfo-_,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Backend Prolog compiler/runtime'
		}, Packages),
		memberchk({spdxElementId-'SPDXRef-DOCUMENT', relationshipType-'DESCRIBES', relatedSpdxElement-'SPDXRef-Application'}, Relationships),
		memberchk({spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Logtalk'}, Relationships),
		memberchk({spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Backend'}, Relationships).

	test(sbom_document_02, deterministic) :-
		document(Document, [name(sample_app), version('1.2.3'), creator('Tool: Custom exporter'), namespace('https://example.com/spdx')]),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-sample_app,
			documentNamespace-DocumentNamespace,
			creationInfo-{created-_, creators-['Tool: Custom exporter']},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(sub_atom(DocumentNamespace, 0, _, _, 'https://example.com/spdx/')),
		^^assertion(Relationships \== []),
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-sample_app,
			versionInfo-'1.2.3',
			downloadLocation-_,
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			primaryPackagePurpose-'APPLICATION',
			summary-_
		}, Packages).

	test(sbom_export_01, deterministic) :-
		export(atom(Atom)),
		json_parse(atom(Atom), JSON),
		JSON = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-['SPDXRef-Application'],
			packages-_,
			relationships-_
		}.

	test(sbom_export_02, deterministic) :-
		export(atom(Atom), [validate_export(true)]),
		json_parse(atom(Atom), JSON),
		schema_path(Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	schema_path(Path) :-
		object_property(tests, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'spdx-schema.json', Path).

:- end_object.
