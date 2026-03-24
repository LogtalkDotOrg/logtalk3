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


:- object(sbom,
	imports(options)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-03-24,
		comment is 'This tool generates a Software Bill of Materials (SBOM) for an application.'
	]).

	:- public(document/2).
	:- mode(document(-compound, +list(compound)), one).
	:- info(document/2, [
		comment is 'Returns an SPDX 2.3 or a CycloneDX 1.6 JSON term describing the currently loaded application using the given options. The JSON term represents objects using curly terms, pairs using a dash, and strings using atoms.',
		argnames is ['Document', 'Options']
	]).

	:- public(document/1).
	:- mode(document(-compound), one).
	:- info(document/1, [
		comment is 'Returns an SPDX 2.3 JSON term describing the currently loaded application using default options. The JSON term represents objects using curly terms, pairs using a dash, and strings using atoms.',
		argnames is ['Document']
	]).

	:- public(export/2).
	:- mode(export(++compound, +list(compound)), one).
	:- info(export/2, [
		comment is 'Exports an SPDX 2.3 or a CycloneDX 1.6 JSON document describing the currently loaded application to the specified sink using the given options. Valid sinks are ``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, and ``atom(Atom)``.',
		argnames is ['Sink', 'Options']
	]).

	:- public(export/1).
	:- mode(export(++compound), one).
	:- info(export/1, [
		comment is 'Exports an SPDX 2.3 JSON document describing the currently loaded application to the specified sink using default options. Valid sinks are ``codes(List)``, ``stream(Stream)``, ``file(Path)``, ``chars(List)``, and ``atom(Atom)``.',
		argnames is ['Sink']
	]).

	:- private(spdx_license_schema_/1).
	:- dynamic(spdx_license_schema_/1).
	:- mode(spdx_license_schema_(-term), zero_or_one).
	:- info(spdx_license_schema_/1, [
		comment is 'Caches the parsed schema used to validate SPDX license identifiers for CycloneDX exports.',
		argnames is ['Schema']
	]).

	:- uses(list, [
		append/3,
		member/2
	]).

	:- uses(json, [
		generate/2 as json_generate/2
	]).

	:- uses(json_schema, [
		parse/2 as json_schema_parse/2,
		validate/2 as json_schema_validate/2
	]).

	:- uses(term_io, [
		format_to_atom/3,
		write_term_to_atom/3
	]).

	:- uses(user, [
		atomic_list_concat/2,
		atomic_list_concat/3
	]).

	:- uses(os, [
		date_time/7,
		decompose_file_name/3,
		path_concat/3,
		pid/1,
		wall_time/1
	]).

	:- uses(packs, [
		loaded_pack/3,
		pack_metadata/4
	]).

	:- uses(uuid, [
		uuid_v4/1
	]).

	:- uses(url(atom), [
		valid/1 as valid_url/1
	]).

	document(Document, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		sbom_document(Document, Options),
		!.

	document(Document) :-
		document(Document, []).

	export(Sink, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		sbom_document(Document, Options),
		(	^^option(validate_export(true), Options) ->
			validate_document(Document, Options)
		;	true
		),
		json_generate(Sink, Document),
		!.

	export(Sink) :-
		export(Sink, []).

	sbom_document(Document, Options) :-
		^^option(format(Format), Options),
		sbom_data(Data, Options),
		(	Format == spdx ->
			spdx_document(Document, Data, Options)
		;	Format == cdx ->
			cyclonedx_document(Document, Data, Options)
		;	fail
		).

	sbom_data(data(Name, Created, Creators, DocumentNamespace, ApplicationPackage, LogtalkPackage, BackendPackage, PackPackages), Options) :-
		^^option(name(Name), Options),
		^^option(version(Version), Options),
		^^option(application_license(ApplicationLicense), Options),
		^^option(logtalk_license(LogtalkLicense), Options),
		^^option(backend_license(BackendLicense), Options),
		creation_timestamp(Created),
		creators(Creators, Options),
		document_namespace(DocumentNamespace, Options),
		application_metadata_options(Options, ApplicationMetadata),
		logtalk_metadata_options(Options, LogtalkMetadata),
		backend_metadata_options(Options, BackendMetadata),
		application_package(Name, Version, ApplicationLicense, ApplicationMetadata, ApplicationPackage),
		logtalk_package(LogtalkLicense, LogtalkMetadata, LogtalkPackage),
		backend_package(BackendLicense, BackendMetadata, BackendPackage),
		loaded_pack_packages(Options, PackPackages).

	spdx_document(Document, data(Name, Created, _, DocumentNamespace, ApplicationPackage, LogtalkPackage, BackendPackage, PackPackages), Options) :-
		spdx_creation_info(CreationInfo, Created, Options),
		packages_json([ApplicationPackage, LogtalkPackage, BackendPackage| PackPackages], Options, Packages),
		relationships_json(PackPackages, Relationships),
		Document = {
			spdxVersion-'SPDX-2.3',
			dataLicense-'CC0-1.0',
			'SPDXID'-'SPDXRef-DOCUMENT',
			name-Name,
			documentNamespace-DocumentNamespace,
			creationInfo-CreationInfo,
			documentDescribes-['SPDXRef-Application'],
			packages-Packages,
			relationships-Relationships
		}.

	cyclonedx_document(Document, data(_, Created, Creators, _, ApplicationPackage, LogtalkPackage, BackendPackage, PackPackages), Options) :-
		cyclonedx_serial_number(SerialNumber),
		cyclonedx_metadata(Created, Creators, ApplicationPackage, Options, Metadata),
		cyclonedx_bom_external_references(Options, BomExternalReferences),
		cyclonedx_components_json([LogtalkPackage, BackendPackage| PackPackages], Components),
		cyclonedx_dependencies_json([ApplicationPackage, LogtalkPackage, BackendPackage| PackPackages], Dependencies),
		Document = {
			bomFormat-'CycloneDX',
			specVersion-'1.6',
			serialNumber-SerialNumber,
			version-1,
			metadata-Metadata,
			externalReferences-BomExternalReferences,
			components-Components,
			dependencies-Dependencies
		}.

	creation_info(CreationInfo, Options) :-
		creation_timestamp(Created),
		creators(Creators, Options),
		CreationInfo = {
			created-Created,
			creators-Creators
		}.

	spdx_creation_info(CreationInfo, Created, Options) :-
		spdx_creators(Creators, Options),
		CreationInfo = {
			created-Created,
			creators-Creators
		}.

	spdx_creators([Creator], Options) :-
		(   ^^option(creator(Creator), Options) ->
			true
		;   spdx_default_creator(Creator)
		).

	creators([Creator], Options) :-
		(   ^^option(creator(Creator), Options) ->
			true
		;   cyclonedx_default_creator(Creator)
		).

	spdx_default_creator(Creator) :-
		sbom_tool_version(Version),
		atomic_list_concat(['Tool: Logtalk SBOM generator', Version], '-', Creator).

	cyclonedx_default_creator('Logtalk SBOM generator').

	cyclonedx_metadata(Created, Creators, ApplicationPackage, Options, Metadata) :-
		cyclonedx_authors(Creators, Authors),
		cyclonedx_tool_component(ToolComponent),
		cyclonedx_bom_license(BomLicense),
		cyclonedx_application_component_json(ApplicationPackage, Options, ApplicationComponent),
		Metadata = {
			timestamp-Created,
			authors-Authors,
			tools-{components-[ToolComponent]},
			licenses-[BomLicense],
			component-ApplicationComponent
		}.

	cyclonedx_authors([], []).
	cyclonedx_authors([Creator| Creators], [{name-Creator}| Authors]) :-
		cyclonedx_authors(Creators, Authors).

	cyclonedx_serial_number(SerialNumber) :-
		uuid_v4(UUID),
		atom_concat('urn:uuid:', UUID, SerialNumber).

	cyclonedx_tool_component(ToolComponent) :-
		sbom_tool_version(Version),
		cyclonedx_component_pairs(
			metadata(absent, absent, absent, absent, absent),
			none,
			'logtalk:tool:sbom',
			sbom,
			Version,
			'NOASSERTION',
			'APPLICATION',
			excluded,
			'Logtalk SBOM generator',
			Pairs
		),
		json_object(Pairs, ToolComponent).

	cyclonedx_bom_license(BomLicense) :-
		cyclonedx_license_json('CC0-1.0', BomLicense).

	cyclonedx_bom_external_references(Options, References) :-
		DefaultReferences = [
			{type-vcs, url-'https://github.com/LogtalkDotOrg/logtalk3'},
			{type-website, url-'https://logtalk.org/'}
		],
		findall(
			{type-Type, url-URL},
			member(bom_external_reference(Type, URL), Options),
			CustomReferences
		),
		append(DefaultReferences, CustomReferences, References0),
		sort(References0, References).

	cyclonedx_application_component_json(package(Reference, Name, Version, License, Checksum, Metadata, Purpose, Description), Options, JSON) :-
		cyclonedx_component_pairs(Metadata, Checksum, Reference, Name, Version, License, Purpose, required, Description, Pairs0),
		application_cyclonedx_external_reference_pairs(Options, ExternalReferencePairs),
		append(Pairs0, ExternalReferencePairs, Pairs),
		json_object(Pairs, JSON).

	application_cyclonedx_external_reference_pairs(Options, [externalReferences-References]) :-
		findall(
			{type-Type, url-URL},
			member(application_external_reference(Type, URL), Options),
			References0
		),
		sort(References0, References),
		References \== [],
		!.
	application_cyclonedx_external_reference_pairs(_, []).

	sbom_tool_version(Version) :-
		this(This),
		object_property(This, info(Info)),
		member(version(Major:Minor:Patch), Info),
		!,
		atomic_list_concat([Major, Minor, Patch], '.', Version).
	sbom_tool_version('1.0.0').

	creation_timestamp(Created) :-
		date_time(Year, Month, Day, Hours, Minutes, Seconds, _),
		format_to_atom('~|~`0t~d~4+-~|~`0t~d~2+-~|~`0t~d~2+T~|~`0t~d~2+:~|~`0t~d~2+:~|~`0t~d~2+Z', [Year, Month, Day, Hours, Minutes, Seconds], Created).

	document_namespace(DocumentNamespace, Options) :-
		^^option(namespace(BaseNamespace), Options),
		pid(PID),
		wall_time(Time),
		TimeInteger is truncate(Time * 1000),
		atomic_list_concat([PID, TimeInteger], '-', Suffix),
		atomic_list_concat([BaseNamespace, Suffix], '/', DocumentNamespace).

	application_metadata_options(Options, metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator)) :-
		optional_metadata_option(application_built_date, Options, BuiltDate),
		optional_metadata_option(application_release_date, Options, ReleaseDate),
		optional_metadata_option(application_valid_until_date, Options, ValidUntilDate),
		optional_metadata_option(application_supplier, Options, Supplier),
		optional_metadata_option(application_originator, Options, Originator).

	logtalk_metadata_options(Options, metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator)) :-
		optional_metadata_option(logtalk_built_date, Options, BuiltDate),
		optional_metadata_option(logtalk_release_date, Options, ReleaseDate),
		optional_metadata_option(logtalk_valid_until_date, Options, ValidUntilDate),
		optional_metadata_option(logtalk_supplier, Options, Supplier),
		optional_metadata_option(logtalk_originator, Options, Originator).

	backend_metadata_options(Options, metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator)) :-
		optional_metadata_option(backend_built_date, Options, BuiltDate),
		optional_metadata_option(backend_release_date, Options, ReleaseDate),
		optional_metadata_option(backend_valid_until_date, Options, ValidUntilDate),
		optional_metadata_option(backend_supplier, Options, Supplier),
		optional_metadata_option(backend_originator, Options, Originator).

	pack_metadata_options(Pack, Options, metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator)) :-
		optional_pack_metadata_option(pack_built_date, Pack, Options, BuiltDate),
		optional_pack_metadata_option(pack_release_date, Pack, Options, ReleaseDate),
		optional_pack_metadata_option(pack_valid_until_date, Pack, Options, ValidUntilDate),
		optional_pack_metadata_option(pack_supplier, Pack, Options, Supplier),
		optional_pack_metadata_option(pack_originator, Pack, Options, Originator).

	optional_metadata_option(Functor, Options, Value) :-
		Template =.. [Functor, Value],
		(   ^^option(Template, Options) ->
			true
		;   Value = absent
		).

	optional_pack_metadata_option(Functor, Pack, Options, Value) :-
		Template =.. [Functor, Pack, Value],
		(   ^^option(Template, Options) ->
			true
		;   Value = absent
		).

	application_package(Name, Version, License, Metadata, package('SPDXRef-Application', Name, Version, License, none, Metadata, 'APPLICATION', 'Logtalk application currently loaded in this session')).

	logtalk_package(License, Metadata, package('SPDXRef-Logtalk', 'Logtalk', Version, License, none, Metadata, 'FRAMEWORK', 'Logtalk runtime')) :-
		current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status)),
		version_atom(logtalk(Major, Minor, Patch, Status), Version).

	backend_package(LicenseOption, Metadata, package('SPDXRef-Backend', BackendName, Version, License, none, Metadata, 'FRAMEWORK', 'Backend Prolog compiler/runtime')) :-
		current_logtalk_flag(prolog_dialect, Backend),
		backend(Backend, BackendName, DefaultLicense, _Website),
		(   LicenseOption == default ->
			License = DefaultLicense
		;	License = LicenseOption
		),
		current_logtalk_flag(prolog_version, BackendVersion),
		version_atom(BackendVersion, Version).

	backend(b,       'B-Prolog',       'NOASSERTION',      'http://www.picat-lang.org/bprolog/').
	backend(ciao,    'Ciao Prolog',    'LGPL-2.1',         'http://ciao-lang.org/').
	backend(cx,      'CxProlog',       'GPL-2.0-or-later', 'http://ctp.di.fct.unl.pt/~amd/cxprolog/').
	backend(eclipse, 'ECLiPSe',        'MPL-1.1',          'https://eclipseclp.org/').
	backend(gnu,     'GNU Prolog',     'LGPL-3.0-or-later','https://www.gprolog.org/').
	backend(ji,      'JIProlog',       'NOASSERTION',      'http://www.jiprolog.com/').
	backend(quintus, 'Quintus Prolog', 'NOASSERTION',      'https://quintus.sics.se/').
	backend(sicstus, 'SICStus Prolog', 'NOASSERTION',      'https://sicstus.sics.se/').
	backend(swi,     'SWI-Prolog',     'BSD-2-Clause',     'https://www.swi-prolog.org/').
	backend(tau,     'Tau Prolog',     'BSD-3-Clause',     'http://tau-prolog.org/').
	backend(trealla, 'Trealla Prolog', 'MIT',              'https://github.com/trealla-prolog/trealla').
	backend(xsb,     'XSB',            'LGPL-2.0',         'https://xsb.sourceforge.net/').
	backend(xvm,     'XVM',            'NOASSERTION',      'https://permion.ai/').
	backend(yap,     'YAP',            'Artistic-2.0',     'https://github.com/vscosta').

	loaded_pack_packages(Options, PackPackages) :-
		findall(
			package(SPDXID, Pack, VersionAtom, License, Checksum, Metadata, 'LIBRARY', Description),
			loaded_pack_package(SPDXID, Pack, VersionAtom, License, Checksum, Metadata, Description, Options),
			PackPackages0
		),
		sort(PackPackages0, PackPackages).

	loaded_pack_package(SPDXID, Pack, VersionAtom, License, Checksum, Metadata, Description, Options) :-
		loaded_pack(Registry, Pack, Version),
		version_atom(Version, VersionAtom),
		pack_metadata_options(Pack, Options, Metadata),
		resolved_pack_metadata(Registry, Pack, Version, DefaultLicense, Checksum, _Home, _SourceURL),
		pack_license(Pack, DefaultLicense, Options, License),
		atomic_list_concat(['SPDXRef-Pack-', Pack], SPDXID),
		atomic_list_concat(['Loaded Logtalk pack ', Pack], Description).

	pack_license(Pack, DefaultLicense, Options, License) :-
		^^option(pack_license(Pack, License), Options, pack_license(Pack, DefaultLicense)).

	resolved_pack_metadata(Registry, Pack, Version, License, Checksum, Home, SourceURL) :-
		pack_metadata(Registry, Pack, Version, metadata(_, _, License0, Home0, SourceURL0, PackChecksum, _, _, _, _, _, _)),
		normalize_pack_license(License0, License),
		normalize_pack_checksum(PackChecksum, Checksum),
		Home = Home0,
		SourceURL = SourceURL0.

	normalize_pack_license(none, 'NOASSERTION').
	normalize_pack_license(License, License).

	normalize_pack_checksum(none, none).
	normalize_pack_checksum(sha256-Digest, checksum('SHA256', Digest)).

	packages_json([], _, []).
	packages_json([Package| Packages], Options, [JSON| JSONs]) :-
		package_json(Package, Options, JSON),
		packages_json(Packages, Options, JSONs).

	package_json(package(SPDXID, Name, Version, License, Checksum, Metadata, Purpose, Description), Options, JSON) :-
		package_json_pairs(Metadata, Checksum, SPDXID, Name, Version, License, Purpose, Description, Options, Pairs),
		json_object(Pairs, JSON).

	cyclonedx_components_json([], []).
	cyclonedx_components_json([Package| Packages], [JSON| JSONs]) :-
		cyclonedx_component_json(Package, JSON),
		cyclonedx_components_json(Packages, JSONs).

	cyclonedx_component_json(package(Reference, Name, Version, License, Checksum, Metadata, Purpose, Description), JSON) :-
		cyclonedx_component_pairs(Metadata, Checksum, Reference, Name, Version, License, Purpose, required, Description, Pairs),
		json_object(Pairs, JSON).

	cyclonedx_component_pairs(Metadata, Checksum, Reference, Name, Version, License, Purpose, Scope, Description, Pairs) :-
		cyclonedx_component_type(Purpose, Type),
		BasePairs = [
			type-Type,
			'bom-ref'-Reference,
			name-Name,
			version-Version,
			scope-Scope,
			description-Description
		],
		cyclonedx_hash_pairs(Checksum, HashPairs),
		cyclonedx_license_pairs(License, LicensePairs),
		cyclonedx_external_reference_pairs(Reference, Name, Version, ExternalReferencePairs),
		cyclonedx_entity_pairs(Metadata, EntityPairs),
		cyclonedx_property_pairs(Metadata, PropertyPairs),
		append(BasePairs, HashPairs, Pairs0),
		append(Pairs0, LicensePairs, Pairs1),
		append(Pairs1, ExternalReferencePairs, Pairs2),
		append(Pairs2, EntityPairs, Pairs3),
		append(Pairs3, PropertyPairs, Pairs).

	cyclonedx_component_type('APPLICATION', application).
	cyclonedx_component_type('FRAMEWORK', framework).
	cyclonedx_component_type('LIBRARY', library).

	cyclonedx_hash_pairs(none, []).
	cyclonedx_hash_pairs(checksum('SHA256', Value), [hashes-[{alg-'SHA-256', content-Value}]]).

	cyclonedx_external_reference_pairs(Reference, Name, Version, [externalReferences-References]) :-
		findall(ExternalReference, cyclonedx_external_reference(Reference, Name, Version, ExternalReference), References0),
		sort(References0, References),
		References \== [],
		!.
	cyclonedx_external_reference_pairs(_, _, _, []).

	cyclonedx_external_reference('SPDXRef-Logtalk', _, _, {type-website, url-'https://logtalk.org/'}).
	cyclonedx_external_reference('SPDXRef-Backend', Name, _, {type-website, url-URL}) :-
		backend(_, Name, _, URL).
	cyclonedx_external_reference('logtalk:tool:sbom', _, _, {type-website, url-'https://logtalk.org/'}).
	cyclonedx_external_reference(Reference, Name, Version, ExternalReference) :-
		sub_atom(Reference, 0, _, _, 'SPDXRef-Pack-'),
		cyclonedx_pack_external_reference(Name, Version, ExternalReference).

	cyclonedx_pack_external_reference(Name, _Version, {type-website, url-Home}) :-
		loaded_pack(Registry, Name, VersionTerm),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, Home, _SourceURL),
		Home \== none.
	cyclonedx_pack_external_reference(Name, Version, {type-distribution, url-URL}) :-
		loaded_pack(Registry, Name, VersionTerm),
		version_atom(VersionTerm, Version),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, _Home, URL),
		URL \== none.

	cyclonedx_license_pairs('NOASSERTION', []).
	cyclonedx_license_pairs(License, [licenses-[LicenseJSON]]) :-
		cyclonedx_license_json(License, LicenseJSON).

	cyclonedx_license_json(License, {license-{id-License}}) :-
		spdx_license_identifier(License),
		!.
	cyclonedx_license_json(License, {expression-License}) :-
		spdx_license_expression(License),
		!.
	cyclonedx_license_json(License, {license-{name-License}}).

	spdx_license_identifier(License) :-
		spdx_license_schema(Schema),
		json_schema_validate(Schema, License).

	spdx_license_expression(License) :-
		atom(License),
		atom_codes(License, Codes),
		phrase(spdx_expression, Codes).

	spdx_expression -->
		spdx_or_expression,
		spdx_blanks,
		eos.

	spdx_or_expression -->
		spdx_and_expression,
		spdx_or_expression_rest.

	spdx_or_expression_rest -->
		spdx_blanks,
		[0'O,0'R],
		spdx_required_blanks,
		spdx_and_expression,
		spdx_or_expression_rest.
	spdx_or_expression_rest --> [].

	spdx_and_expression -->
		spdx_with_expression,
		spdx_and_expression_rest.

	spdx_and_expression_rest -->
		spdx_blanks,
		[0'A,0'N,0'D],
		spdx_required_blanks,
		spdx_with_expression,
		spdx_and_expression_rest.
	spdx_and_expression_rest --> [].

	spdx_with_expression -->
		spdx_primary_expression,
		spdx_with_expression_rest.

	spdx_with_expression_rest -->
		spdx_required_blanks,
		[0'W,0'I,0'T,0'H],
		spdx_required_blanks,
		spdx_license_token,
		spdx_blanks.
	spdx_with_expression_rest --> [].

	spdx_primary_expression -->
		spdx_blanks,
		[0'(],
		spdx_blanks,
		spdx_or_expression,
		spdx_blanks,
		[0')],
		spdx_blanks.
	spdx_primary_expression -->
		spdx_blanks,
		spdx_license_token,
		spdx_blanks.

	spdx_license_token -->
		spdx_token_codes(Codes),
		{ Codes \== [], atom_codes(Token, Codes), spdx_identifier_reference(Token) }.

	spdx_identifier_reference(Token) :-
		spdx_license_identifier(Token),
		!.
	spdx_identifier_reference(Token) :-
		custom_spdx_reference(Token).

	custom_spdx_reference(Token) :-
		sub_atom(Token, 0, _, _, 'LicenseRef-'),
		!.
	custom_spdx_reference(Token) :-
		sub_atom(Token, 0, _, _, 'DocumentRef-'),
		sub_atom(Token, _, _, _, ':LicenseRef-').

	spdx_token_codes([Code| Codes]) -->
		[Code],
		{ Code =\= 0'(, Code =\= 0'), \+ is_space(Code) },
		spdx_token_codes_rest(Codes).

	spdx_token_codes_rest([Code| Codes]) -->
		[Code],
		{ Code =\= 0'(, Code =\= 0'), \+ is_space(Code) },
		!,
		spdx_token_codes_rest(Codes).
	spdx_token_codes_rest([]) --> [].

	spdx_required_blanks -->
		[Code],
		{ is_space(Code) },
		spdx_blanks.

	spdx_blanks -->
		[Code],
		{ is_space(Code) },
		!,
		spdx_blanks.
	spdx_blanks --> [].

	is_space(32).
	is_space(0'\t).
	is_space(0'\n).
	is_space(0'\r).

	spdx_license_schema(Schema) :-
		spdx_license_schema_(Schema),
		!.
	spdx_license_schema(Schema) :-
		schema_path(cdx_spdx_license, Path),
		json_schema_parse(file(Path), Schema),
		assertz(spdx_license_schema_(Schema)).

	cyclonedx_entity_pairs(metadata(_, _, _, Supplier, Originator), Pairs) :-
		cyclonedx_supplier_pairs(Supplier, SupplierPairs),
		cyclonedx_originator_pairs(Originator, OriginatorPairs),
		append(SupplierPairs, OriginatorPairs, Pairs).

	cyclonedx_supplier_pairs(absent, []) :-
		!.
	cyclonedx_supplier_pairs(Supplier, [supplier-{name-Name}]) :-
		cyclonedx_organization_name(Supplier, Name),
		!.
	cyclonedx_supplier_pairs(_, []).

	cyclonedx_originator_pairs(absent, []) :-
		!.
	cyclonedx_originator_pairs(Originator, [manufacturer-{name-Name}]) :-
		cyclonedx_organization_name(Originator, Name),
		!.
	cyclonedx_originator_pairs(Originator, [authors-[{name-Name}]]) :-
		cyclonedx_person_name(Originator, Name),
		!.
	cyclonedx_originator_pairs(Originator, [authors-[{name-Originator}]]) :-
		Originator \== absent.

	cyclonedx_organization_name(Value, Name) :-
		atom_concat('Organization: ', Name, Value).

	cyclonedx_person_name(Value, Name) :-
		atom_concat('Person: ', Name, Value).

	cyclonedx_property_pairs(metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator), [properties-Properties]) :-
		findall(Property, cyclonedx_property(metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator), Property), Properties),
		Properties \== [],
		!.
	cyclonedx_property_pairs(_, []).

	cyclonedx_property(metadata(Value, _, _, _, _), {name-'logtalk:sbom:built_date', value-Value}) :-
		Value \== absent.
	cyclonedx_property(metadata(_, Value, _, _, _), {name-'logtalk:sbom:release_date', value-Value}) :-
		Value \== absent.
	cyclonedx_property(metadata(_, _, Value, _, _), {name-'logtalk:sbom:valid_until_date', value-Value}) :-
		Value \== absent.
	cyclonedx_property(metadata(_, _, _, Value, _), {name-'logtalk:sbom:supplier', value-Value}) :-
		Value \== absent.
	cyclonedx_property(metadata(_, _, _, _, Value), {name-'logtalk:sbom:originator', value-Value}) :-
		Value \== absent.

	cyclonedx_dependencies_json(Packages, Dependencies) :-
		pack_references(Packages, PackReferences),
		cyclonedx_dependency_entries(Packages, PackReferences, Dependencies).

	pack_references([], []).
	pack_references([package(Reference, _, _, _, _, _, 'LIBRARY', _)| Packages], [Reference| References]) :-
		!,
		pack_references(Packages, References).
	pack_references([_| Packages], References) :-
		pack_references(Packages, References).

	cyclonedx_dependency_entries([ApplicationPackage, LogtalkPackage, BackendPackage| PackPackages], PackReferences, [ApplicationDependency, LogtalkDependency, BackendDependency| PackDependencies]) :-
		ApplicationPackage = package(ApplicationReference, _, _, _, _, _, _, _),
		LogtalkPackage = package(LogtalkReference, _, _, _, _, _, _, _),
		BackendPackage = package(BackendReference, _, _, _, _, _, _, _),
		cyclonedx_dependency_json(ApplicationReference, [LogtalkReference, BackendReference| PackReferences], ApplicationDependency),
		cyclonedx_dependency_json(LogtalkReference, [BackendReference], LogtalkDependency),
		cyclonedx_dependency_json(BackendReference, [], BackendDependency),
		cyclonedx_pack_dependency_entries(PackPackages, PackDependencies).

	cyclonedx_pack_dependency_entries([], []).
	cyclonedx_pack_dependency_entries([package(Reference, _, _, _, _, _, _, _)| Packages], [Dependency| Dependencies]) :-
		cyclonedx_dependency_json(Reference, [], Dependency),
		cyclonedx_pack_dependency_entries(Packages, Dependencies).

	cyclonedx_dependency_json(Reference, [], JSON) :-
		!,
		JSON = {ref-Reference}.
	cyclonedx_dependency_json(Reference, DependsOn, JSON) :-
		JSON = {
			ref-Reference,
			dependsOn-DependsOn
		}.

	package_json_pairs(Metadata, Checksum, SPDXID, Name, Version, License, Purpose, Description, Options, Pairs) :-
		spdx_download_location_pairs(SPDXID, Name, Version, DownloadLocationPairs),
		BasePairs = [
			'SPDXID'-SPDXID,
			name-Name,
			versionInfo-Version,
			DownloadLocation,
			filesAnalyzed- @false
		],
		LicensePairs = [
			licenseConcluded-License,
			licenseDeclared-License
		],
		TrailingPairs = [
			primaryPackagePurpose-Purpose,
			summary-Description
		],
		DownloadLocationPairs = [DownloadLocation],
		spdx_homepage_pairs(SPDXID, Name, HomepagePairs),
		spdx_external_reference_pairs(SPDXID, Name, Version, Options, ExternalReferencePairs),
		checksum_pairs(Checksum, ChecksumPairs),
		metadata_pairs(Metadata, MetadataPairs),
		append(BasePairs, ChecksumPairs, Pairs0),
		append(Pairs0, LicensePairs, Pairs1),
		append(Pairs1, HomepagePairs, Pairs2),
		append(Pairs2, ExternalReferencePairs, Pairs3),
		append(Pairs3, MetadataPairs, Pairs4),
		append(Pairs4, TrailingPairs, Pairs).

	spdx_download_location_pairs(SPDXID, Name, Version, [downloadLocation-URL]) :-
		(   spdx_pack_download_location(SPDXID, Name, Version, URL) ->
			true
		;   URL = 'http://spdx.org/rdf/terms#noassertion'
		).

	spdx_homepage_pairs(SPDXID, Name, [homepage-URL]) :-
		spdx_pack_homepage(SPDXID, Name, URL),
		!.
	spdx_homepage_pairs(_, _, []).

	spdx_external_reference_pairs(SPDXID, Name, Version, Options, [externalRefs-References]) :-
		findall(Reference, spdx_package_external_reference(SPDXID, Name, Version, Options, Reference), References0),
		sort(References0, References),
		References \== [],
		!.
	spdx_external_reference_pairs(_, _, _, _, []).

	spdx_package_external_reference('SPDXRef-Application', _, _, Options, Reference) :-
		member(application_external_reference(Type, URL), Options),
		spdx_external_reference_json(Type, URL, Reference).
	spdx_package_external_reference('SPDXRef-Logtalk', _, _, _, Reference) :-
		spdx_external_reference_json(website, 'https://logtalk.org/', Reference).
	spdx_package_external_reference('SPDXRef-Backend', Name, _, _, Reference) :-
		backend(_, Name, _, URL),
		spdx_external_reference_json(website, URL, Reference).
	spdx_package_external_reference(SPDXID, Name, Version, _, Reference) :-
		sub_atom(SPDXID, 0, _, _, 'SPDXRef-Pack-'),
		spdx_pack_external_reference(Name, Version, Type, URL),
		spdx_external_reference_json(Type, URL, Reference).

	spdx_pack_external_reference(Name, _Version, website, Home) :-
		loaded_pack(Registry, Name, VersionTerm),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, Home, _SourceURL),
		Home \== none.
	spdx_pack_external_reference(Name, Version, distribution, URL) :-
		loaded_pack(Registry, Name, VersionTerm),
		version_atom(VersionTerm, Version),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, _Home, URL),
		URL \== none.

	spdx_external_reference_json(Type, URL, {
		referenceCategory-'OTHER',
		referenceType-Type,
		referenceLocator-URL
	}).

	spdx_pack_download_location(SPDXID, Name, Version, URL) :-
		sub_atom(SPDXID, 0, _, _, 'SPDXRef-Pack-'),
		loaded_pack(Registry, Name, VersionTerm),
		version_atom(VersionTerm, Version),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, _Home, URL),
		URL \== none.

	spdx_pack_homepage(SPDXID, Name, URL) :-
		sub_atom(SPDXID, 0, _, _, 'SPDXRef-Pack-'),
		loaded_pack(Registry, Name, VersionTerm),
		resolved_pack_metadata(Registry, Name, VersionTerm, _License, _Checksum, URL, _SourceURL),
		URL \== none.

	checksum_pairs(none, []).
	checksum_pairs(checksum(Algorithm, Value), [checksums-[{algorithm-Algorithm, checksumValue-Value}]]).

	metadata_pairs(metadata(BuiltDate, ReleaseDate, ValidUntilDate, Supplier, Originator), Pairs) :-
		optional_pair(builtDate, BuiltDate, BuiltDatePairs),
		optional_pair(releaseDate, ReleaseDate, ReleaseDatePairs),
		optional_pair(validUntilDate, ValidUntilDate, ValidUntilDatePairs),
		optional_pair(supplier, Supplier, SupplierPairs),
		optional_pair(originator, Originator, OriginatorPairs),
		append(BuiltDatePairs, ReleaseDatePairs, Pairs0),
		append(Pairs0, ValidUntilDatePairs, Pairs1),
		append(Pairs1, SupplierPairs, Pairs2),
		append(Pairs2, OriginatorPairs, Pairs).

	optional_pair(_, absent, []) :-
		!.
	optional_pair(Key, Value, [Key-Value]).

	json_object([Pair| Pairs], JSON) :-
		list_to_curly_pairs([Pair| Pairs], Term),
		JSON = {Term}.

	list_to_curly_pairs([Pair], Pair) :-
		!.
	list_to_curly_pairs([Pair| Pairs], (Pair, Rest)) :-
		list_to_curly_pairs(Pairs, Rest).

	relationships_json(PackPackages, Relationships) :-
		base_relationships(BaseRelationships),
		pack_relationships(PackPackages, PackRelationships),
		append(BaseRelationships, PackRelationships, Relationships).

	base_relationships([
		{spdxElementId-'SPDXRef-DOCUMENT', relationshipType-'DESCRIBES', relatedSpdxElement-'SPDXRef-Application'},
		{spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Logtalk'},
		{spdxElementId-'SPDXRef-Application', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Backend'},
		{spdxElementId-'SPDXRef-Logtalk', relationshipType-'DEPENDS_ON', relatedSpdxElement-'SPDXRef-Backend'}
	]).

	pack_relationships([], []).
	pack_relationships([package(SPDXID, _, _, _, _, _, _, _)| PackPackages], [Relationship| Relationships]) :-
		Relationship = {
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-SPDXID
		},
		pack_relationships(PackPackages, Relationships).

	validate_document(Document, Options) :-
		^^option(format(Format), Options),
		schema_path(Format, Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, Document).

	schema_path(spdx, Path) :-
		object_property(sbom, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'test_files/spdx-schema.json', Path).
	schema_path(cdx, Path) :-
		object_property(sbom, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'test_files/cyclonedx-1.6.schema.json', Path).
	schema_path(cdx_spdx_license, Path) :-
		object_property(sbom, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'test_files/spdx.schema.json.cdx', Path).

	version_atom(v(Major, Minor, Patch), Version) :-
		!,
		atomic_list_concat([Major, Minor, Patch], '.', Version).
	version_atom(logtalk(Major, Minor, Patch, Status), Version) :-
		!,
		atomic_list_concat([Major, Minor, Patch], '.', BaseVersion),
		atomic_list_concat([BaseVersion, Status], '-', Version).
	version_atom(Major:Minor:Patch:Status, Version) :-
		!,
		atomic_list_concat([Major, Minor, Patch], '.', BaseVersion),
		atomic_list_concat([BaseVersion, Status], '-', Version).
	version_atom(Major:Minor:Patch, Version) :-
		!,
		atomic_list_concat([Major, Minor, Patch], '.', Version).
	version_atom(Major:Minor, Version) :-
		!,
		atomic_list_concat([Major, Minor], '.', Version).
	version_atom(Version, VersionAtom) :-
		atom(Version),
		!,
		VersionAtom = Version.
	version_atom(Version, VersionAtom) :-
		number(Version),
		!,
		write_term_to_atom(Version, VersionAtom, []).
	version_atom(Version, VersionAtom) :-
		write_term_to_atom(Version, VersionAtom, [quoted(true)]).

	default_option(name('loaded-application')).
	default_option(format(spdx)).
	default_option(version('0.0.0')).
	default_option(application_license('NOASSERTION')).
	default_option(logtalk_license('Apache-2.0')).
	default_option(backend_license(default)).
	default_option(namespace('https://logtalk.org/spdxdocs/logtalk-sbom')).
	default_option(validate_export(false)).

	valid_option(name(Name)) :-
		atom(Name).
	valid_option(format(spdx)).
	valid_option(format(cdx)).
	valid_option(version(Version)) :-
		atom(Version).
	valid_option(application_license(License)) :-
		atom(License).
	valid_option(logtalk_license(License)) :-
		atom(License).
	valid_option(application_built_date(BuiltDate)) :-
		atom(BuiltDate),
		BuiltDate \== none.
	valid_option(application_release_date(ReleaseDate)) :-
		atom(ReleaseDate),
		ReleaseDate \== none.
	valid_option(application_valid_until_date(ValidUntilDate)) :-
		atom(ValidUntilDate),
		ValidUntilDate \== none.
	valid_option(application_supplier(Supplier)) :-
		atom(Supplier),
		Supplier \== none.
	valid_option(application_originator(Originator)) :-
		atom(Originator),
		Originator \== none.
	valid_option(application_external_reference(Type, URL)) :-
		atom(Type),
		Type \== none,
		atom(URL),
		URL \== none,
		valid_url(URL).
	valid_option(bom_external_reference(Type, URL)) :-
		atom(Type),
		Type \== none,
		atom(URL),
		URL \== none,
		valid_url(URL).
	valid_option(logtalk_built_date(BuiltDate)) :-
		atom(BuiltDate),
		BuiltDate \== none.
	valid_option(logtalk_release_date(ReleaseDate)) :-
		atom(ReleaseDate),
		ReleaseDate \== none.
	valid_option(logtalk_valid_until_date(ValidUntilDate)) :-
		atom(ValidUntilDate),
		ValidUntilDate \== none.
	valid_option(logtalk_supplier(Supplier)) :-
		atom(Supplier),
		Supplier \== none.
	valid_option(logtalk_originator(Originator)) :-
		atom(Originator),
		Originator \== none.
	valid_option(pack_license(Pack, License)) :-
		atom(Pack),
		atom(License).
	valid_option(pack_built_date(Pack, BuiltDate)) :-
		atom(Pack),
		atom(BuiltDate),
		BuiltDate \== none.
	valid_option(pack_release_date(Pack, ReleaseDate)) :-
		atom(Pack),
		atom(ReleaseDate),
		ReleaseDate \== none.
	valid_option(pack_valid_until_date(Pack, ValidUntilDate)) :-
		atom(Pack),
		atom(ValidUntilDate),
		ValidUntilDate \== none.
	valid_option(pack_supplier(Pack, Supplier)) :-
		atom(Pack),
		atom(Supplier),
		Supplier \== none.
	valid_option(pack_originator(Pack, Originator)) :-
		atom(Pack),
		atom(Originator),
		Originator \== none.
	valid_option(backend_license(default)).
	valid_option(backend_license(License)) :-
		atom(License).
	valid_option(backend_built_date(BuiltDate)) :-
		atom(BuiltDate),
		BuiltDate \== none.
	valid_option(backend_release_date(ReleaseDate)) :-
		atom(ReleaseDate),
		ReleaseDate \== none.
	valid_option(backend_valid_until_date(ValidUntilDate)) :-
		atom(ValidUntilDate),
		ValidUntilDate \== none.
	valid_option(backend_supplier(Supplier)) :-
		atom(Supplier),
		Supplier \== none.
	valid_option(backend_originator(Originator)) :-
		atom(Originator),
		Originator \== none.
	valid_option(namespace(Namespace)) :-
		atom(Namespace).
	valid_option(creator(Creator)) :-
		atom(Creator).
	valid_option(validate_export(true)).
	valid_option(validate_export(false)).

:- end_object.
