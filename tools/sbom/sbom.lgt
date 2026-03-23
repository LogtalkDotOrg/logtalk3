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
		date is 2026-03-23,
		comment is 'This tool generates a Software Bill of Materials (SBOM) for an application.'
	]).

	:- public(document/2).
	:- mode(document(-compound, +list(compound)), one).
	:- info(document/2, [
		comment is 'Returns an SPDX 2.3 JSON term describing the currently loaded application using the given options.',
		argnames is ['Document', 'Options']
	]).

	:- public(document/1).
	:- mode(document(-compound), one).
	:- info(document/1, [
		comment is 'Returns an SPDX 2.3 JSON term describing the currently loaded application using default options.',
		argnames is ['Document']
	]).

	:- public(export/2).
	:- mode(export(++compound, +list(compound)), one).
	:- info(export/2, [
		comment is 'Exports an SPDX 2.3 JSON document describing the currently loaded application to the specified sink using the given options.',
		argnames is ['Sink', 'Options']
	]).

	:- public(export/1).
	:- mode(export(++compound), one).
	:- info(export/1, [
		comment is 'Exports an SPDX 2.3 JSON document describing the currently loaded application to the specified sink using default options.',
		argnames is ['Sink']
	]).

	:- uses(list, [
		append/3
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
		installed/4,
		directory/2
	]).

	document(Document, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		spdx_document(Document, Options),
		!.

	document(Document) :-
		document(Document, []).

	export(Sink, UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		spdx_document(Document, Options),
		(^^option(validate_export(true), Options) ->
			validate_document(Document)
		;	true
		),
		json_generate(Sink, Document),
		!.

	export(Sink) :-
		export(Sink, []).

	spdx_document(Document, Options) :-
		^^option(name(Name), Options),
		^^option(version(Version), Options),
		^^option(application_license(ApplicationLicense), Options),
		^^option(logtalk_license(LogtalkLicense), Options),
		^^option(backend_license(BackendLicense), Options),
		creation_info(CreationInfo, Options),
		document_namespace(DocumentNamespace, Options),
		application_package(Name, Version, ApplicationLicense, ApplicationPackage),
		logtalk_package(LogtalkLicense, LogtalkPackage),
		backend_package(BackendLicense, BackendPackage),
		loaded_pack_packages(Options, PackPackages),
		packages_json([ApplicationPackage, LogtalkPackage, BackendPackage| PackPackages], Packages),
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

	creation_info(CreationInfo, Options) :-
		creation_timestamp(Created),
		creators(Creators, Options),
		CreationInfo = {
			created-Created,
			creators-Creators
		}.

	creators([Creator], Options) :-
		^^option(creator(Creator), Options).

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

	application_package(Name, Version, License, package('SPDXRef-Application', Name, Version, License, none, 'APPLICATION', 'Logtalk application currently loaded in this session')).

	logtalk_package(License, package('SPDXRef-Logtalk', 'Logtalk', Version, License, none, 'FRAMEWORK', 'Logtalk runtime')) :-
		current_logtalk_flag(version_data, logtalk(Major, Minor, Patch, Status)),
		version_atom(logtalk(Major, Minor, Patch, Status), Version).

	backend_package(LicenseOption, package('SPDXRef-Backend', BackendName, Version, License, none, 'FRAMEWORK', 'Backend Prolog compiler/runtime')) :-
		current_logtalk_flag(prolog_dialect, Backend),
		backend(Backend, BackendName, DefaultLicense),
		(   LicenseOption == default ->
			License = DefaultLicense
		;	License = LicenseOption
		),
		current_logtalk_flag(prolog_version, BackendVersion),
		version_atom(BackendVersion, Version).

	backend(b,       'B-Prolog',       'NOASSERTION').
	backend(ciao,    'Ciao Prolog',    'LGPL-2.1').
	backend(cx,      'CxProlog',       'GPL-2.0-or-later').
	backend(eclipse, 'ECLiPSe',        'MPL-1.1').
	backend(gnu,     'GNU Prolog',     'LGPL-3.0-or-later').
	backend(ji,      'JIProlog',       'NOASSERTION').
	backend(quintus, 'Quintus Prolog', 'NOASSERTION').
	backend(sicstus, 'SICStus Prolog', 'NOASSERTION').
	backend(swi,     'SWI-Prolog',     'BSD-2-Clause').
	backend(tau,     'Tau Prolog',     'BSD-3-Clause').
	backend(trealla, 'Trealla Prolog', 'MIT').
	backend(xsb,     'XSB',            'LGPL-2.0').
	backend(xvm,     'XVM',            'NOASSERTION').
	backend(yap,     'YAP',            'Artistic-2.0').

	loaded_pack_packages(Options, PackPackages) :-
		findall(
			package(SPDXID, Pack, VersionAtom, License, Checksum, 'LIBRARY', Description),
			loaded_pack_package(SPDXID, Pack, VersionAtom, License, Checksum, Description, Options),
			PackPackages0
		),
		sort(PackPackages0, PackPackages).

	loaded_pack_package(SPDXID, Pack, VersionAtom, License, Checksum, Description, Options) :-
		installed(Registry, Pack, Version, _Pinned),
		directory(Pack, PackDirectory),
		loaded_from_pack_directory(PackDirectory),
		version_atom(Version, VersionAtom),
		pack_metadata(Registry, Pack, Version, DefaultLicense, Checksum),
		pack_license(Pack, DefaultLicense, Options, License),
		atomic_list_concat(['SPDXRef-Pack-', Pack], SPDXID),
		atomic_list_concat(['Loaded Logtalk pack ', Pack], Description).

	pack_license(Pack, DefaultLicense, Options, License) :-
		^^option(pack_license(Pack, License), Options, pack_license(Pack, DefaultLicense)).

	pack_metadata(Registry, Pack, Version, License, Checksum) :-
		(   registry_pack_object(Registry, Pack, PackObject) ->
			(   PackObject::license(PackLicense) ->
				License = PackLicense
			;   License = 'NOASSERTION'
			),
			(   PackObject::version(Version, _, _, PackChecksum, _, _) ->
				normalize_pack_checksum(PackChecksum, Checksum)
			;   Checksum = none
			)
		;   License = 'NOASSERTION',
			Checksum = none
		).

	registry_pack_object(Registry, Pack, PackObject) :-
		implements_protocol(RegistryObject, registry_protocol),
		RegistryObject::name(Registry),
		object_property(RegistryObject, file(_, Directory)),
		once((
			implements_protocol(PackObject, pack_protocol),
			PackObject::name(Pack),
			object_property(PackObject, file(_, Directory))
		)).

	normalize_pack_checksum(none, none).
	normalize_pack_checksum(sha256-Digest, checksum('SHA256', Digest)).

	loaded_from_pack_directory(PackDirectory) :-
		atom_concat(PackDirectory, '/', Prefix),
		logtalk::loaded_file_property(_, directory(LoadedDirectory)),
		(LoadedDirectory == PackDirectory ; sub_atom(LoadedDirectory, 0, _, _, Prefix)),
		!.

	packages_json([], []).
	packages_json([Package| Packages], [JSON| JSONs]) :-
		package_json(Package, JSON),
		packages_json(Packages, JSONs).

	package_json(package(SPDXID, Name, Version, License, none, Purpose, Description), JSON) :-
		JSON = {
			'SPDXID'-SPDXID,
			name-Name,
			versionInfo-Version,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			licenseConcluded-License,
			licenseDeclared-License,
			primaryPackagePurpose-Purpose,
			summary-Description
		}.
	package_json(package(SPDXID, Name, Version, License, Checksum, Purpose, Description), JSON) :-
		checksum_json(Checksum, ChecksumJSON),
		JSON = {
			'SPDXID'-SPDXID,
			name-Name,
			versionInfo-Version,
			downloadLocation-'http://spdx.org/rdf/terms#noassertion',
			filesAnalyzed- @false,
			checksums-[ChecksumJSON],
			licenseConcluded-License,
			licenseDeclared-License,
			primaryPackagePurpose-Purpose,
			summary-Description
		}.

	checksum_json(checksum(Algorithm, Value), {algorithm-Algorithm, checksumValue-Value}).

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
	pack_relationships([package(SPDXID, _, _, _, _, _, _)| PackPackages], [Relationship| Relationships]) :-
		Relationship = {
			spdxElementId-'SPDXRef-Application',
			relationshipType-'DEPENDS_ON',
			relatedSpdxElement-SPDXID
		},
		pack_relationships(PackPackages, Relationships).

	validate_document(Document) :-
		schema_path(Path),
		json_schema_parse(file(Path), Schema),
		json_schema_validate(Schema, Document).

	schema_path(Path) :-
		object_property(sbom, file(File)),
		decompose_file_name(File, Directory, _),
		path_concat(Directory, 'spdx-schema.json', Path).

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
	default_option(version('0.0.0')).
	default_option(application_license('NOASSERTION')).
	default_option(logtalk_license('Apache-2.0')).
	default_option(backend_license(default)).
	default_option(namespace('https://logtalk.org/spdxdocs/logtalk-sbom')).
	default_option(creator('Logtalk "sbom" tool')).
	default_option(validate_export(false)).

	valid_option(name(Name)) :-
		atom(Name).
	valid_option(version(Version)) :-
		atom(Version).
	valid_option(application_license(License)) :-
		atom(License).
	valid_option(logtalk_license(License)) :-
		atom(License).
	valid_option(pack_license(Pack, License)) :-
		atom(Pack),
		atom(License).
	valid_option(backend_license(default)).
	valid_option(backend_license(License)) :-
		atom(License).
	valid_option(namespace(Namespace)) :-
		atom(Namespace).
	valid_option(creator(Creator)) :-
		atom(Creator).
	valid_option(validate_export(true)).
	valid_option(validate_export(false)).

:- end_object.
