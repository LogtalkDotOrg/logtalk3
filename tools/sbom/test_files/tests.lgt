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
		date is 2026-03-25,
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
		member/2, memberchk/2
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
		^^file_path('logtalk_packs', LogtalkPacks),
		make_directory_path(LogtalkPacks),
		reset,
		^^file_url('sbom_fixture_registry', URL),
		add(sbom_fixture_registry, URL, [update(true)]),
		install(sbom_fixture_registry, sbom_fixture_pack, 1:0:0, []),
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
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-{created-_, creators-[Creator]},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-Relationships
		},
		^^assertion(sub_atom(Creator, 0, _, _, 'Tool: Logtalk SBOM generator-')),
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
			externalRefs-LogtalkExternalReferences,
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Logtalk runtime'
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'https://logtalk.org/'}, LogtalkExternalReferences),
		memberchk({
			'SPDXID'-'SPDXRef-Backend',
			name-_,
			versionInfo-_,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-BackendLicense,
			licenseDeclared-BackendLicense,
			externalRefs-BackendExternalReferences,
			primaryPackagePurpose-'FRAMEWORK',
			summary-'Backend Prolog compiler/runtime'
		}, Packages),
		^^assertion(atom(BackendLicense)),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-_}, BackendExternalReferences),
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
			creators(['Tool: Custom exporter']),
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
			externalRefs-LogtalkExternalReferences,
			builtDate-'2026-03-22T00:00:00Z',
			releaseDate-'2026-03-22T00:00:00Z',
			validUntilDate-'2027-03-22T00:00:00Z',
			supplier-'Organization: Logtalk.org',
			originator-'Person: Paulo Moura',
			primaryPackagePurpose-'FRAMEWORK',
			summary-_
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'https://logtalk.org/'}, LogtalkExternalReferences),
		memberchk({
			'SPDXID'-'SPDXRef-Backend',
			name-_,
			versionInfo-_,
			downloadLocation-_,
			filesAnalyzed- @false,
			licenseConcluded-'BSD-2-Clause',
			licenseDeclared-'BSD-2-Clause',
			externalRefs-BackendExternalReferences,
			builtDate-'2026-03-21T00:00:00Z',
			releaseDate-'2026-03-21T00:00:00Z',
			validUntilDate-'2027-03-21T00:00:00Z',
			supplier-'Organization: Backend Vendor',
			originator-'Organization: Backend Vendor',
			primaryPackagePurpose-'FRAMEWORK',
			summary-_
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-_}, BackendExternalReferences).

	test(sbom_document_03, deterministic) :-
		document(Document),
		^^assertion(ground(Document)),
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
		memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_pack',
			name-sbom_fixture_pack,
			versionInfo-'1.0.0',
			downloadLocation-'file://sbom_fixture_pack',
			filesAnalyzed- @false,
			checksums-[{algorithm-'SHA256', checksumValue-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'}],
			licenseConcluded-'Apache-2.0',
			licenseDeclared-'Apache-2.0',
			homepage-'file://sbom_fixture_pack',
			externalRefs-PackExternalReferences,
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_pack'
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-distribution, referenceLocator-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_pack'
		}, Relationships),
		memberchk({
			spdxElementId-'SPDXRef-Pack-sbom_fixture_pack',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_no_checksum_pack'
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
		^^assertion(ground(Document)),
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
		memberchk({
			'SPDXID'-'SPDXRef-Pack-sbom_fixture_pack',
			name-sbom_fixture_pack,
			versionInfo-'1.0.0',
			downloadLocation-'file://sbom_fixture_pack',
			filesAnalyzed- @false,
			checksums-[{algorithm-'SHA256', checksumValue-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'}],
			licenseConcluded-'Zlib',
			licenseDeclared-'Zlib',
			homepage-'file://sbom_fixture_pack',
			externalRefs-PackExternalReferences,
			builtDate-'2026-03-20T00:00:00Z',
			releaseDate-'2026-03-20T00:00:00Z',
			validUntilDate-'2027-03-20T00:00:00Z',
			supplier-'Organization: Fixture Registry',
			originator-'Person: Fixture Author',
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_pack'
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-distribution, referenceLocator-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_pack'
		}, Relationships),
		memberchk({
			spdxElementId-'SPDXRef-Pack-sbom_fixture_pack',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_no_checksum_pack'
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
			downloadLocation-'file://sbom_fixture_no_checksum_pack',
			filesAnalyzed- @false,
			licenseConcluded-'MIT',
			licenseDeclared-'MIT',
			homepage-'file://sbom_fixture_no_checksum_pack',
			externalRefs-PackExternalReferences,
			primaryPackagePurpose-'LIBRARY',
			summary-'Loaded Logtalk pack sbom_fixture_no_checksum_pack'
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'file://sbom_fixture_no_checksum_pack'}, PackExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-distribution, referenceLocator-'file://sbom_fixture_no_checksum_pack'}, PackExternalReferences),
		memberchk({
			spdxElementId-'SPDXRef-Pack-sbom_fixture_pack',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-'SPDXRef-Pack-sbom_fixture_no_checksum_pack'
		}, Relationships),
		\+ memberchk({
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
		^^file_path('spdx-schema.json', Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	test(sbom_document_06, deterministic) :-
		document(Document, [format(cdx)]),
		^^assertion(ground(Document)),
		current_logtalk_flag(prolog_dialect, Backend),
		sbom<<backend(Backend, _, BackendLicense, BackendWebsite),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-SerialNumber,
			version-1,
			metadata-Metadata,
			externalReferences-BomExternalReferences,
			components-Components,
			dependencies-Dependencies
		},
		^^assertion(sub_atom(SerialNumber, 0, _, _, 'urn:uuid:')),
		memberchk({type-vcs, url-'https://github.com/LogtalkDotOrg/logtalk3'}, BomExternalReferences),
		memberchk({type-website, url-'https://logtalk.org/'}, BomExternalReferences),
		Metadata = {
			timestamp-_,
			authors-[{name-'Logtalk SBOM generator'}],
			tools-{components-[ToolComponent]},
			licenses-[{license-{id-'CC0-1.0'}}],
			component-{
				type-application,
				'bom-ref'-'SPDXRef-Application',
				name-'loaded-application',
				version-'0.0.0',
				scope-required,
				description-_
			}
		},
		ToolComponent = {
			type-application,
			'bom-ref'-'logtalk:tool:sbom',
			name-sbom,
			version-_,
			scope-excluded,
			description-'Logtalk SBOM generator',
			externalReferences-ToolExternalReferences
		},
		memberchk({type-website, url-'https://logtalk.org/'}, ToolExternalReferences),
		memberchk({
			type-framework,
			'bom-ref'-'SPDXRef-Logtalk',
			name-'Logtalk',
			version-_,
			scope-required,
			description-'Logtalk runtime',
			licenses-[{license-{id-'Apache-2.0'}}],
			externalReferences-LogtalkExternalReferences
		}, Components),
		memberchk({type-website, url-'https://logtalk.org/'}, LogtalkExternalReferences),
		(   BackendLicense == 'NOASSERTION' ->
			memberchk({
				type-framework,
				'bom-ref'-'SPDXRef-Backend',
				name-_,
				version-_,
				scope-required,
				description-'Backend Prolog compiler/runtime',
				externalReferences-BackendExternalReferences
			}, Components)
		;   memberchk({
				type-framework,
				'bom-ref'-'SPDXRef-Backend',
				name-_,
				version-_,
				scope-required,
				description-'Backend Prolog compiler/runtime',
				licenses-[{license-{id-BackendLicense}}],
				externalReferences-BackendExternalReferences
			}, Components)
		),
		memberchk({type-website, url-BackendWebsite}, BackendExternalReferences),
		memberchk({
			type-library,
			'bom-ref'-'SPDXRef-Pack-sbom_fixture_pack',
			name-sbom_fixture_pack,
			version-'1.0.0',
			scope-required,
			description-'Loaded Logtalk pack sbom_fixture_pack',
			hashes-[{alg-'SHA-256', content-'0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef'}],
			licenses-[{license-{id-'Apache-2.0'}}],
			externalReferences-PackExternalReferences
		}, Components),
		memberchk({type-distribution, url-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({type-website, url-'file://sbom_fixture_pack'}, PackExternalReferences),
		memberchk({
			type-library,
			'bom-ref'-'SPDXRef-Pack-sbom_fixture_no_checksum_pack',
			name-sbom_fixture_no_checksum_pack,
			version-'1.0.0',
			scope-required,
			description-'Loaded Logtalk pack sbom_fixture_no_checksum_pack',
			licenses-[{license-{id-'MIT'}}],
			externalReferences-NoChecksumPackExternalReferences
		}, Components),
		memberchk({type-distribution, url-'file://sbom_fixture_no_checksum_pack'}, NoChecksumPackExternalReferences),
		memberchk({type-website, url-'file://sbom_fixture_no_checksum_pack'}, NoChecksumPackExternalReferences),
		memberchk({
			ref-'SPDXRef-Application',
			dependsOn-ApplicationDependsOn
		}, Dependencies),
		memberchk('SPDXRef-Logtalk', ApplicationDependsOn),
		memberchk('SPDXRef-Backend', ApplicationDependsOn),
		memberchk('SPDXRef-Pack-sbom_fixture_pack', ApplicationDependsOn),
		\+ memberchk('SPDXRef-Pack-sbom_fixture_no_checksum_pack', ApplicationDependsOn),
		memberchk({
			ref-'SPDXRef-Logtalk',
			dependsOn-['SPDXRef-Backend']
		}, Dependencies),
		memberchk({
			ref-'SPDXRef-Pack-sbom_fixture_pack',
			dependsOn-['SPDXRef-Pack-sbom_fixture_no_checksum_pack']
		}, Dependencies),
		memberchk({ref-'SPDXRef-Pack-sbom_fixture_no_checksum_pack'}, Dependencies),
		memberchk({ref-'SPDXRef-Backend'}, Dependencies).

	test(sbom_export_03, deterministic) :-
		export(atom(Atom), [format(cdx), validate_export(true)]),
		json_parse(atom(Atom), JSON),
		^^assertion(ground(JSON)),
		JSON = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-_,
			externalReferences-_,
			components-_,
			dependencies-_
		},
		^^file_path('cyclonedx-1.6.schema.json', Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, JSON).

	test(sbom_document_07, deterministic) :-
		document(Document, [
			format(cdx),
			application_license('Acme Software License')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-Metadata,
			externalReferences-_,
			components-_,
			dependencies-_
		},
		Metadata = {
			timestamp-_,
			authors-_,
			tools-_,
			licenses-[{license-{id-'CC0-1.0'}}],
			component-{
				type-application,
				'bom-ref'-'SPDXRef-Application',
				name-'loaded-application',
				version-'0.0.0',
				scope-required,
				description-_,
				licenses-[{license-{name-'Acme Software License'}}]
			}
		}.

	test(sbom_document_08, deterministic) :-
		document(Document, [
			format(cdx),
			application_license('Apache-2.0 AND MIT')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-Metadata,
			externalReferences-_,
			components-_,
			dependencies-_
		},
		Metadata = {
			timestamp-_,
			authors-_,
			tools-_,
			licenses-[{license-{id-'CC0-1.0'}}],
			component-{
				type-application,
				'bom-ref'-'SPDXRef-Application',
				name-'loaded-application',
				version-'0.0.0',
				scope-required,
				description-_,
				licenses-[{expression-'Apache-2.0 AND MIT'}]
			}
		}.

	test(sbom_document_09, deterministic) :-
		document(Document, [
			format(cdx),
			application_external_reference(website, 'https://example.com/app'),
			application_external_reference(vcs, 'https://example.com/example-app.git')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-Metadata,
			externalReferences-_,
			components-_,
			dependencies-_
		},
		Metadata = {
			timestamp-_,
			authors-_,
			tools-_,
			licenses-[{license-{id-'CC0-1.0'}}],
			component-{
				type-application,
				'bom-ref'-'SPDXRef-Application',
				name-'loaded-application',
				version-'0.0.0',
				scope-required,
				description-_,
				externalReferences-ApplicationExternalReferences
			}
		},
		memberchk({type-vcs, url-'https://example.com/example-app.git'}, ApplicationExternalReferences),
		memberchk({type-website, url-'https://example.com/app'}, ApplicationExternalReferences).

	test(sbom_document_10, deterministic) :-
		document(Document, [
			format(cdx),
			bom_external_reference(documentation, 'https://example.com/sbom-docs'),
			bom_external_reference(website, 'https://example.com/sbom-home')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-_,
			externalReferences-BomExternalReferences,
			components-_,
			dependencies-_
		},
		memberchk({type-vcs, url-'https://github.com/LogtalkDotOrg/logtalk3'}, BomExternalReferences),
		memberchk({type-website, url-'https://logtalk.org/'}, BomExternalReferences),
		memberchk({type-documentation, url-'https://example.com/sbom-docs'}, BomExternalReferences),
		memberchk({type-website, url-'https://example.com/sbom-home'}, BomExternalReferences).

	test(sbom_document_11, deterministic) :-
		document(Document, [
			application_external_reference(website, 'https://example.com/app'),
			application_external_reference(vcs, 'https://example.com/example-app.git')
		]),
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-_
		},
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-'loaded-application',
			versionInfo-'0.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			externalRefs-ApplicationExternalReferences,
			primaryPackagePurpose-'APPLICATION',
			summary-_
		}, Packages),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'https://example.com/app'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-vcs, referenceLocator-'https://example.com/example-app.git'}, ApplicationExternalReferences).

	test(sbom_document_12, deterministic) :-
		document(Document, [
			creators(['Tool: Build pipeline', 'Person: Alice Example', 'Organization: Example, Inc.'])
		]),
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-{created-_, creators-['Tool: Build pipeline', 'Person: Alice Example', 'Organization: Example, Inc.']},
			documentDescribes-['SPDXRef-Application'],
			packages-_,
			relationships-_
		}.

	test(sbom_document_13, deterministic) :-
		document(Document, [
			format(cdx),
			creators(['Tool: Build pipeline', 'Person: Alice Example', 'Organization: Example, Inc.'])
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-{
				timestamp-_,
				authors-[
					{name-'Tool: Build pipeline'},
					{name-'Person: Alice Example'},
					{name-'Organization: Example, Inc.'}
				],
				tools-_,
				licenses-_,
				component-_
			},
			externalReferences-_,
			components-_,
			dependencies-_
		}.

	test(sbom_document_14, deterministic, [cleanup((current_object(sbom_application_object_14) -> abolish_object(sbom_application_object_14) ; true))]) :-
		create_object(sbom_application_object_14, [implements(application_protocol), imports(application_common)], [], [
			name(sample_application),
			version('3.2.1'),
			description('Sample application from metadata object'),
			license('BSD-3-Clause'),
			homepage('https://example.com/sample'),
			distribution('https://example.com/sample/download'),
			package('pkg:generic/sample_application@3.2.1'),
			creators(['Person: Application Author']),
			supplier('Organization: Example Supplier'),
			originator('Person: Example Originator'),
			built_date('2026-03-24T00:00:00Z'),
			release_date('2026-03-25T00:00:00Z'),
			valid_until_date('2027-03-25T00:00:00Z'),
			repository('https://example.com/sample.git'),
			git_object_identifier('gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'),
			software_heritage_identifier('swh:1:rev:0123456789abcdef0123456789abcdef01234567')
		]),
		document(Document),
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-sample_application,
			documentNamespace-_,
			creationInfo-{created-_, creators-['Person: Application Author']},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-_
		},
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-sample_application,
			versionInfo-'3.2.1',
			downloadLocation-'https://example.com/sample/download',
			filesAnalyzed- @false,
			licenseConcluded-'BSD-3-Clause',
			licenseDeclared-'BSD-3-Clause',
			homepage-'https://example.com/sample',
			externalRefs-ApplicationExternalReferences,
			builtDate-'2026-03-24T00:00:00Z',
			releaseDate-'2026-03-25T00:00:00Z',
			validUntilDate-'2027-03-25T00:00:00Z',
			supplier-'Organization: Example Supplier',
			originator-'Person: Example Originator',
			primaryPackagePurpose-'APPLICATION',
			summary-'Sample application from metadata object'
		}, Packages),
		memberchk({referenceCategory-'PACKAGE-MANAGER', referenceType-purl, referenceLocator-'pkg:generic/sample_application@3.2.1'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'PERSISTENT-ID', referenceType-gitoid, referenceLocator-'gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'PERSISTENT-ID', referenceType-swh, referenceLocator-'swh:1:rev:0123456789abcdef0123456789abcdef01234567'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-distribution, referenceLocator-'https://example.com/sample/download'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-vcs, referenceLocator-'https://example.com/sample.git'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'OTHER', referenceType-website, referenceLocator-'https://example.com/sample'}, ApplicationExternalReferences).

	test(sbom_document_15, deterministic, [cleanup((current_object(sbom_application_object_15) -> abolish_object(sbom_application_object_15) ; true))]) :-
		create_object(sbom_application_object_15, [implements(application_protocol), imports(application_common)], [], [
			name(sample_application),
			version('3.2.1'),
			description('Sample application from metadata object'),
			license('BSD-3-Clause'),
			homepage('https://example.com/sample'),
			distribution('https://example.com/sample/download'),
			package('pkg:generic/sample_application@3.2.1'),
			creators(['Person: Application Author']),
			supplier('Organization: Example Supplier'),
			originator('Person: Example Originator'),
			built_date('2026-03-24T00:00:00Z'),
			release_date('2026-03-25T00:00:00Z'),
			valid_until_date('2027-03-25T00:00:00Z'),
			repository('https://example.com/sample.git'),
			git_object_identifier('gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'),
			software_heritage_identifier('swh:1:rev:0123456789abcdef0123456789abcdef01234567')
		]),
		document(Document, [
			format(cdx),
			name(overridden_application),
			version('9.9.9'),
			application_license('MIT'),
			application_originator('Organization: Override Originator'),
			creators(['Tool: Override Creator']),
			application_external_reference(documentation, 'https://example.com/sample/docs')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-{
				timestamp-_,
				authors-[{name-'Tool: Override Creator'}],
				tools-_,
				licenses-_,
				component-{
					type-application,
					'bom-ref'-'SPDXRef-Application',
					name-overridden_application,
					version-'9.9.9',
					scope-required,
					description-'Sample application from metadata object',
					licenses-[{license-{id-'MIT'}}],
					supplier-{name-'Example Supplier'},
					manufacturer-{name-'Override Originator'},
					properties-ApplicationProperties,
					purl-'pkg:generic/sample_application@3.2.1',
					omniborId-['gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'],
					swhid-['swh:1:rev:0123456789abcdef0123456789abcdef01234567'],
					externalReferences-ApplicationExternalReferences
				}
			},
			externalReferences-_,
			components-_,
			dependencies-_
		},
		memberchk({type-documentation, url-'https://example.com/sample/docs'}, ApplicationExternalReferences),
		memberchk({type-distribution, url-'https://example.com/sample/download'}, ApplicationExternalReferences),
		memberchk({type-vcs, url-'https://example.com/sample.git'}, ApplicationExternalReferences),
		memberchk({type-website, url-'https://example.com/sample'}, ApplicationExternalReferences),
		\+ member({type-purl, url-_}, ApplicationExternalReferences),
		\+ member({type-gitoid, url-_}, ApplicationExternalReferences),
		\+ member({type-swh, url-_}, ApplicationExternalReferences),
		memberchk({name-'logtalk:sbom:built_date', value-'2026-03-24T00:00:00Z'}, ApplicationProperties),
		memberchk({name-'logtalk:sbom:release_date', value-'2026-03-25T00:00:00Z'}, ApplicationProperties),
		memberchk({name-'logtalk:sbom:valid_until_date', value-'2027-03-25T00:00:00Z'}, ApplicationProperties),
		memberchk({name-'logtalk:sbom:supplier', value-'Organization: Example Supplier'}, ApplicationProperties),
		memberchk({name-'logtalk:sbom:originator', value-'Organization: Override Originator'}, ApplicationProperties).

	test(sbom_document_16, deterministic, [cleanup((
			(current_object(sbom_application_object_16_1) -> abolish_object(sbom_application_object_16_1) ; true),
			(current_object(sbom_application_object_16_2) -> abolish_object(sbom_application_object_16_2) ; true)
		))]) :-
		create_object(sbom_application_object_16_1, [implements(application_protocol), imports(application_common)], [], [name(first_application), version('1.0.0')]),
		create_object(sbom_application_object_16_2, [implements(application_protocol), imports(application_common)], [], [name(second_application), version('2.0.0')]),
		document(Document),
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-{created-_, creators-[Creator]},
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-_
		},
		^^assertion(sub_atom(Creator, 0, _, _, 'Tool: Logtalk SBOM generator-')),
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-'loaded-application',
			versionInfo-'0.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			primaryPackagePurpose-'APPLICATION',
			summary-'Logtalk application currently loaded in this session'
		}, Packages),
		\+ memberchk({
			'SPDXID'-'SPDXRef-Application',
			externalRefs-_
		}, Packages).

	test(sbom_document_17, deterministic) :-
		document(Document, [
			application_external_reference(purl, 'pkg:generic/option_application@1.2.3'),
			application_external_reference(gitoid, 'gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'),
			application_external_reference(swh, 'swh:1:rev:0123456789abcdef0123456789abcdef01234567')
		]),
		^^assertion(ground(Document)),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-'loaded-application',
			documentNamespace-_,
			creationInfo-_,
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-_
		},
		memberchk({
			'SPDXID'-'SPDXRef-Application',
			name-'loaded-application',
			versionInfo-'0.0.0',
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-'NOASSERTION',
			licenseDeclared-'NOASSERTION',
			externalRefs-ApplicationExternalReferences,
			primaryPackagePurpose-'APPLICATION',
			summary-_
		}, Packages),
		memberchk({referenceCategory-'PACKAGE-MANAGER', referenceType-purl, referenceLocator-'pkg:generic/option_application@1.2.3'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'PERSISTENT-ID', referenceType-gitoid, referenceLocator-'gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'}, ApplicationExternalReferences),
		memberchk({referenceCategory-'PERSISTENT-ID', referenceType-swh, referenceLocator-'swh:1:rev:0123456789abcdef0123456789abcdef01234567'}, ApplicationExternalReferences).

	test(sbom_document_18, deterministic) :-
		document(Document, [
			format(cdx),
			application_external_reference(purl, 'pkg:generic/option_application@1.2.3'),
			application_external_reference(gitoid, 'gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'),
			application_external_reference(swh, 'swh:1:rev:0123456789abcdef0123456789abcdef01234567')
		]),
		^^assertion(ground(Document)),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-_,
			version-1,
			metadata-{
				timestamp-_,
				authors-_,
				tools-_,
				licenses-_,
				component-{
					type-application,
					'bom-ref'-'SPDXRef-Application',
					name-'loaded-application',
					version-'0.0.0',
					scope-required,
					description-_,
					purl-'pkg:generic/option_application@1.2.3',
					omniborId-['gitoid:commit:sha1:0123456789abcdef0123456789abcdef01234567'],
					swhid-['swh:1:rev:0123456789abcdef0123456789abcdef01234567']
				}
			},
			externalReferences-_,
			components-_,
			dependencies-_
		}.

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_Message, _Kind, packs, _Tokens).

:- end_object.
