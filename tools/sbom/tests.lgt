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

	:- uses(registries, [
		add/3
	]).

	:- uses(packs, [
		directory/2,
		install/4,
		reset/0
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
		change_directory/1,
		decompose_file_name/3,
		make_directory_path/1,
		path_concat/3
	]).

	setup :-
		object_property(tests, file(File)),
		decompose_file_name(File, Directory, _),
		change_directory(Directory),
		^^file_path('test_files/logtalk_packs', LogtalkPacks),
		make_directory_path(LogtalkPacks),
		reset,
		^^file_url('test_files/sbom_fixture_registry', URL),
		add(sbom_fixture_registry, URL, [update(true)]),
		install(sbom_fixture_registry, sbom_fixture_pack, 1:0:0, []),
		install(sbom_fixture_registry, sbom_fixture_no_checksum_pack, 1:0:0, []),
		directory(sbom_fixture_pack, PackDirectory),
		path_concat(PackDirectory, 'loader.lgt', Loader),
		logtalk_load(Loader),
		directory(sbom_fixture_no_checksum_pack, NoChecksumPackDirectory),
		path_concat(NoChecksumPackDirectory, 'loader.lgt', NoChecksumLoader),
		logtalk_load(NoChecksumLoader),
		!.

	cleanup :-
		reset,
		!.

	test(sbom_document_01, deterministic) :-
		document(Document),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-{created-_, creators-['Logtalk "sbom" tool']},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(ground(Document)),
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
			name-'Logtalk',
			versionInfo-_,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'Apache-2.0',
			licenseDeclared-'Apache-2.0',
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Logtalk runtime'
		}, Packages),
		memberchk({
			'SPDXID'-'SPDXRef-Backend',
			name-_,
			versionInfo-_,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-BackendLicense,
			licenseDeclared-BackendLicense,
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Backend Prolog compiler/runtime'
		}, Packages),
		^^assertion(atom(BackendLicense)),
		memberchk({spdxElementId-'SPDXRef-DOCUMENT', relationshipType-'DESCRIBES', relatedSpdxElement-'SPDXRef-Application'}, Relationships),
		memberchk({spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Logtalk'}, Relationships),
		memberchk({spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Backend'}, Relationships).

	test(sbom_document_02, deterministic) :-
		document(Document, [
			name(sample_app),
			version('1.2.3'),
			application_license('MIT'),
			logtalk_license('Apache-2.0'),
			backend_license('BSD-2-Clause'),
			application_built_date('2026-03-23T00:00:00Z'),
			application_release_date('2026-03-23T00:00:00Z'),
			application_valid_until_date('2027-03-23T00:00:00Z'),
			application_supplier('Organization: Example Application'),
			application_originator('Person: Application Maintainer'),
			logtalk_built_date('2026-03-22T00:00:00Z'),
			logtalk_release_date('2026-03-22T00:00:00Z'),
			logtalk_valid_until_date('2027-03-22T00:00:00Z'),
			logtalk_supplier('Organization: Logtalk.org'),
			logtalk_originator('Person: Paulo Moura'),
			backend_built_date('2026-03-21T00:00:00Z'),
			backend_release_date('2026-03-21T00:00:00Z'),
			backend_valid_until_date('2027-03-21T00:00:00Z'),
			backend_supplier('Organization: Backend Vendor'),
			backend_originator('Organization: Backend Vendor'),
			pack_license(dummy_pack, 'Zlib'),
			creator('Tool: Custom exporter'),
			namespace('https://example.com/spdx')
		]),
		^^assertion(ground(Document)),
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
			licenseConcluded-'MIT',
			licenseDeclared-'MIT',
			builtDate-'2026-03-23T00:00:00Z',
			releaseDate-'2026-03-23T00:00:00Z',
			validUntilDate-'2027-03-23T00:00:00Z',
			supplier-'Organization: Example Application',
			originator-'Person: Application Maintainer',
			primaryPackagePurpose-'APPLICATION',
			summary-_
		}, Packages),
		memberchk({
			'SPDXID'-'SPDXRef-Logtalk',
			name-'Logtalk',
			versionInfo-_,
			downloadLocation-_,
			filesAnalyzed- @false,
			licenseConcluded-'Apache-2.0',
			licenseDeclared-'Apache-2.0',
			builtDate-'2026-03-22T00:00:00Z',
			releaseDate-'2026-03-22T00:00:00Z',
			validUntilDate-'2027-03-22T00:00:00Z',
			supplier-'Organization: Logtalk.org',
			originator-'Person: Paulo Moura',
			primaryPackagePurpose-'FRAMEWORK',
			summary-_
		}, Packages),
		memberchk({
			'SPDXID'-'SPDXRef-Backend',
			name-_,
			versionInfo-_,
			downloadLocation-_,
			filesAnalyzed- @false,
			licenseConcluded-'BSD-2-Clause',
			licenseDeclared-'BSD-2-Clause',
			builtDate-'2026-03-21T00:00:00Z',
			releaseDate-'2026-03-21T00:00:00Z',
			validUntilDate-'2027-03-21T00:00:00Z',
			supplier-'Organization: Backend Vendor',
			originator-'Organization: Backend Vendor',
			primaryPackagePurpose-'FRAMEWORK',
			summary-_
		}, Packages).

	test(sbom_document_03, deterministic) :-
		document(Document),
		Document = {
			spdxVersion-_,
			dataLicense-_,
			'SPDXID'-_,
			name-_,
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-_,
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(ground(Document)),
		memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_pack',
			name-sbom_fixture_pack,
			versionInfo-'1.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			checksums-[{algorithm-'SHA256', checksumValue-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'}],
			licenseConcluded-'Apache-2.0',
			licenseDeclared-'Apache-2.0',
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_pack'
		}, Packages),
		memberchk({
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_pack'
		}, Relationships).

	test(sbom_document_04, deterministic) :-
		document(Document, [
			pack_license(sbom_fixture_pack, 'Zlib'),
			pack_built_date(sbom_fixture_pack, '2026-03-20T00:00:00Z'),
			pack_release_date(sbom_fixture_pack, '2026-03-20T00:00:00Z'),
			pack_valid_until_date(sbom_fixture_pack, '2027-03-20T00:00:00Z'),
			pack_supplier(sbom_fixture_pack, 'Organization: Fixture Registry'),
			pack_originator(sbom_fixture_pack, 'Person: Fixture Author')
		]),
		Document = {
			spdxVersion-_,
			dataLicense-_,
			'SPDXID'-_,
			name-_,
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-_,
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(ground(Document)),
		memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_pack',
			name-sbom_fixture_pack,
			versionInfo-'1.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			checksums-[{algorithm-'SHA256', checksumValue-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'}],
			licenseConcluded-'Zlib',
			licenseDeclared-'Zlib',
			builtDate-'2026-03-20T00:00:00Z',
			releaseDate-'2026-03-20T00:00:00Z',
			validUntilDate-'2027-03-20T00:00:00Z',
			supplier-'Organization: Fixture Registry',
			originator-'Person: Fixture Author',
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_pack'
		}, Packages),
		memberchk({
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_pack'
		}, Relationships).

	test(sbom_document_05, deterministic) :-
		document(Document),
		Document = {
			spdxVersion-_,
			dataLicense-_,
			'SPDXID'-_,
			name-_,
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-_,
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(ground(Document)),
		memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_no_checksum_pack',
			name-sbom_fixture_no_checksum_pack,
			versionInfo-'1.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'MIT',
			licenseDeclared-'MIT',
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_no_checksum_pack'
		}, Packages),
		memberchk({
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_no_checksum_pack'
		}, Relationships),
		\+ memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_no_checksum_pack',
			checksums-_
		}, Packages).

	test(sbom_export_01, deterministic) :-
		export(atom(Atom)),
		json_parse(atom(Atom), JSON),
		^^assertion(ground(JSON)),
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

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, packs, _Tokens).

	schema_path(Path) :-
		object_property(tests, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'spdx-schema.json', Path).

:- end_object.
