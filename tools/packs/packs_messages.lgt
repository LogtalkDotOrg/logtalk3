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


:- category(packs_messages).

	:- info([
		version is 0:41:0,
		author is 'Paulo Moura',
		date is 2025-05-23,
		comment is 'Packs default message translations.'
	]).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).

	logtalk::message_prefix_stream(Kind, packs, Prefix, Stream) :-
		message_prefix_stream(Kind, Prefix, Stream).

	message_prefix_stream(information, '% ',     user_output).
	message_prefix_stream(warning,     '*     ', user_output).
	message_prefix_stream(error,       '!     ', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(Message, packs) -->
		message_tokens(Message).

	% common messages

	message_tokens(help) -->
		[	'Common queries using the packs tool main predicates:'-[], nl, nl,
			'  List registries:                 registries::list.'-[], nl,
			'  Update registries:               registries::update.'-[], nl,
			'  Add a new registry:              registries::add(Registry, URL).'-[], nl,
			'  Delete a registry:               registries::delete(Registry).'-[], nl, nl,
			'  Print registry data:             registries::describe(Registry).'-[], nl,
			'  Registry readme file:            registries::readme(Registry).'-[], nl,
			'  Registry installation directory: packs::directory(Pack).'-[], nl, nl,
			'  List available packs:            packs::available.'-[], nl,
			'  List installed packs:            packs::installed.'-[], nl, nl,
			'  Print pack data:                 packs::describe(Pack).'-[], nl,
			'                                   packs::describe(Registry, Pack).'-[], nl,
			'  Pack readme file:                packs::readme(Pack).'-[], nl,
			'  Pack installation directory:     packs::directory(Pack).'-[], nl, nl,
			'  Install a pack:                  packs::install(Pack).'-[], nl,
			'                                   packs::install(Registry, Pack).'-[], nl,
			'                                   packs::install(Registry, Pack, Version).'-[], nl, nl,
			'  Update a pack:                   packs::update(Pack).'-[], nl,
			'  Uninstall a pack:                packs::uninstall(Pack).'-[], nl,
			'  Clean a pack archive:            packs::clean(Pack).'-[], nl, nl,
			'Consult the Handbook and the tool API documentation'-[], nl,
			'for details and other available predicates.'-[], nl
		].

	message_tokens(missing_command(Command)) -->
		['Missing required shell command: ~q'-[Command], nl].

	message_tokens(readme_file(ReadMeFile)) -->
		['Readme file: ~w'-[ReadMeFile], nl].

	message_tokens(note(Note)) -->
		['Note: ~w'-[Note], nl].

	message_tokens(logtalk_packs(LogtalkPacks)) -->
		['Logtalk packs storage directory: ~w'-[LogtalkPacks], nl].

	message_tokens(reset_failed(LogtalkPacks)) -->
		['Reset of registries and packs directory structure failed: ~q'-[LogtalkPacks], nl].

	% registry add messages

	message_tokens(adding_registry(Registry)) -->
		['Adding registry: ~q'-[Registry], nl].

	message_tokens(registry_added(Registry)) -->
		['Registry added: ~q'-[Registry], nl].

	message_tokens(registry_name_must_be(Name)) -->
		['Registry name must be:  ~q'-[Name], nl].

	% registry update messages

	message_tokens(updating_registry(Registry, URL)) -->
		['Updating registry: ~q (~w)'-[Registry, URL], nl].

	message_tokens(registry_updated(Registry, URL)) -->
		['Registry updated:  ~q (~w)'-[Registry, URL], nl].

	message_tokens(pinned_registry(Registry, URL)) -->
		['Registry is pinned: ~q (~w)'-[Registry, URL], nl].

	message_tokens(up_to_date_registry(Registry, URL)) -->
		['Registry is up-to-date: ~q (~w)'-[Registry, URL], nl].

	% registry delete messages

	message_tokens(deleting_registry(Registry)) -->
		['Deleting registry: ~q'-[Registry], nl].

	message_tokens(registry_deleted(Registry)) -->
		['Registry deleted: ~q'-[Registry], nl].

	% registry error messages

	message_tokens(registry_already_defined(Registry)) -->
		['Registry is already defined: ~q'-[Registry], nl].

	message_tokens(unsupported_archive_format(Archive)) -->
		['Unsupported archive format: ~q'-[Archive], nl].

	message_tokens(registry_loader_file_missing(Registry)) -->
		['Registry loader file missing: ~q'-[Registry], nl].

	message_tokens(registry_loading_failed(Registry)) -->
		['Registry loading failed: ~q'-[Registry], nl].

	message_tokens(unknown_registry(Registry)) -->
		['Unknown registry: ~q'-[Registry], nl].

	message_tokens(registry_deletion_failed(Registry, Directory)) -->
		['Registry deletion failed: ~q (~q)'-[Registry, Directory], nl].

	message_tokens(registry_clone_pull_failed(Registry, URL)) -->
		['Registry clone pull failed: ~q (~q)'-[Registry, URL], nl].

	message_tokens(registry_cloning_failed(Registry, URL)) -->
		['Registry cloning failed: ~q (~q)'-[Registry, URL], nl].

	message_tokens(registry_directory_copy_failed(Registry, URL)) -->
		['Registry directory copy failed: ~q (~q)'-[Registry, URL], nl].

	message_tokens(registry_download_failed(Registry, Command)) -->
		['Registry archive download failed: ~q (~q)'-[Registry, Command], nl].

	message_tokens(registry_archive_uncompress_failed(Registry, Path)) -->
		['Registry archive uncompress failed: ~q (~q)'-[Registry, Path], nl].

	message_tokens(cannot_update_pinned_registry(Registry)) -->
		['Cannot update pinned registry: ~q'-[Registry], nl].

	message_tokens(cannot_delete_pinned_registry(Registry)) -->
		['Cannot delete pinned registry: ~q'-[Registry], nl].

	message_tokens(cannot_delete_registry_with_installed_packs(Registry)) -->
		['Cannot delete registry with installed packs: ~q'-[Registry], nl].

	% registry describe messages

	message_tokens(registry_info(Registry,Description,Home,Clone,Archive,Notes,Pinned)) -->
		[	nl,
			'Registry:    ~q'-[Registry]
		],
		pinned(Pinned),
		[	'Description: ~w'-[Description], nl,
			'Home:        ~w'-[Home], nl,
			'Cloning URL: ~w'-[Clone], nl,
			'Archive URL: ~w'-[Archive], nl
		],
		notes(Notes).

	message_tokens(defined_registries(DefinedRegistries)) -->
		defined_registries(DefinedRegistries).

	% registry lint messages

	message_tokens(linting_registry(Registry)) -->
		['Lint checking registry: ~q'-[Registry], nl].

	message_tokens(linted_registry(Registry)) -->
		['Lint checks completed for registry: ~q'-[Registry], nl].

	% pack install messages

	message_tokens(incompatible_pack_status(Registry, Pack, Version, Status)) -->
		['Pack with status ~w cannot be installed with given options: ~q::~q@~q'-[Status, Registry, Pack, Version], nl].

	message_tokens(unknown_pack_version(Registry, Pack, Version)) -->
		['Unknown pack version: ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(pack_already_installed_from_different_registry(Registry, Pack, Version)) -->
		['Pack is already installed but from a different registry: ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(pack_already_installed(Pack)) -->
		['Pack is already installed: ~q'-[Pack], nl].

	message_tokens(installing_pack(Registry, Pack, LatestVersion)) -->
		['Installing pack: ~q::~q@~q'-[Registry, Pack, LatestVersion], nl].

	message_tokens(pack_installed(Registry, Pack, Version)) -->
		['Pack installed:  ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(pack_cloning_failed(Pack, URL)) -->
		['Pack cloning failed: ~q (~q)'-[Pack, URL], nl].

	message_tokens(pack_directory_copy_failed(Pack, URL)) -->
		['Pack directory copy failed: ~q (~q)'-[Pack, URL], nl].

	message_tokens(os_dependency_not_available(Dependency)) -->
		['Operating-system dependency not available: ~q'-[Dependency], nl].

	message_tokens(logtalk_dependency_not_available(Dependency)) -->
		['Logtalk dependency not available: ~q'-[Dependency], nl].

	message_tokens(backend_dependency_not_available(Dependency)) -->
		['Backend dependency not available: ~q'-[Dependency], nl].

	message_tokens(pack_dependency_not_available(Dependency, Operator1, Lower, Operator2, Upper)) -->
		['Pack dependency not available: ~q ~q ~q and ~q ~q'-[Dependency, Operator1, Lower, Operator2, Upper], nl].

	message_tokens(pack_dependency_not_available(Registry, Pack)) -->
		['Pack dependency not available: ~q::~q'-[Registry, Pack], nl].

	message_tokens(pack_dependency_not_available(Dependency)) -->
		['Pack dependency not available: ~q'-[Dependency], nl].

	message_tokens(no_pack_version_compatible_with_os_version(Name, Machine, Version)) -->
		['No pack version compatible with the current operating-system version is available: ~w ~w ~w'-[Name, Machine, Version], nl].

	message_tokens(no_pack_version_compatible_with_logtalk_version(Version)) -->
		['No pack version compatible with the current Logtalk version is available: ~w'-[Version], nl].

	message_tokens(no_pack_version_compatible_with_backend_version(Name, Version)) -->
		['No pack version compatible with the current backend version is available: ~w ~w'-[Name, Version], nl].

	message_tokens(updating_pack_would_break_installed_pack(Pack, Version, Dependent)) -->
		['Updating ~q to ~q would break installed pack ~q'-[Pack, Version, Dependent], nl].

	message_tokens(updating_pack_breaks_installed_pack(Pack, Version, Dependent)) -->
		['Updating ~q to ~q breaks installed pack ~q'-[Pack, Version, Dependent], nl].

	% pack uninstall messages

	message_tokens(uninstalling_pack(Registry, Pack)) -->
		['Uninstalling pack: ~q::~q'-[Registry, Pack], nl].

	message_tokens(pack_uninstalled(Registry, Pack)) -->
		['Pack uninstalled:  ~q::~q'-[Registry, Pack], nl].

	message_tokens(pack_uninstall_failed(Pack, Directory)) -->
		['Pack uninstall failed: ~q (~q)'-[Pack, Directory], nl].

	message_tokens(pack_directory_clean_failed(Pack, Directory)) -->
		['Pack directory clean failed: ~q (~q)'-[Pack, Directory], nl].

	% pack update messages

	message_tokens(updating_pack(Registry, Pack, Version, LatestVersion)) -->
		['Updating pack: ~q::~q (~q to ~q)'-[Registry, Pack, Version, LatestVersion], nl].

	message_tokens(pack_updated(Registry, Pack, Version)) -->
		['Pack updated:  ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(up_to_date_pack(Registry, Pack, Version)) -->
		['Pack is up-to-date: ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(pinned_pack(Registry, Pack, Version)) -->
		['Pack is pinned:     ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(orphaned_pack(Registry, Pack, Version)) -->
		['Pack is orphaned:   ~q::~q@~q'-[Registry, Pack, Version], nl].

	message_tokens(pack_update_failed(Registry, Pack, Version)) -->
		['Pack update failed: ~q::~q@~q'-[Registry, Pack, Version], nl].

	% pack installed messages

	message_tokens(installed_pack(Registry, Pack, Version, true)) -->
		['  ~q::~q@~q (pinned)'-[Registry, Pack, Version], nl].
	message_tokens(installed_pack(Registry, Pack, Version, false)) -->
		['  ~q::~q@~q'-[Registry, Pack, Version], nl].

	% pack outdated messages

	message_tokens(outdated_pack(Registry, Pack, Version, LatestVersion)) -->
		['  ~q::~q@~q - ~q available'-[Registry, Pack, Version, LatestVersion], nl].

	% pack listing messages

	message_tokens(pack(Registry, Pack)) -->
		['  ~q::~q'-[Registry, Pack], nl].

	% pack clean messages

	message_tokens(cleaning_pack_archives(Registry, Pack)) -->
		['Cleaning archives for pack: ~q::~q'-[Registry, Pack], nl].

	message_tokens(cleaned_pack_archives(Registry, Pack)) -->
		['Cleaned archives for pack:  ~q::~q'-[Registry, Pack], nl].

	% pack lint messages

	message_tokens(linting_pack(Registry, Pack)) -->
		['Lint checking pack: ~q::~q'-[Registry, Pack], nl].

	message_tokens(linted_pack(Registry, Pack)) -->
		['Lint checks completed for pack: ~q::~q'-[Registry, Pack], nl].

	% pack error messages

	message_tokens(cannot_uninstall_pinned_pack(Pack)) -->
		['Cannot uninstall pinned pack: ~q'-[Pack], nl].

	message_tokens(cannot_uninstall_pack_with_dependents(Pack, Dependents)) -->
		[	'Cannot uninstall pack with dependents: ~q'-[Pack], nl,
			'  The following packs would break: ~q'-[Dependents], nl
		].

	message_tokens(cannot_update_pinned_pack(Pack)) -->
		['Cannot update pinned pack: ~q'-[Pack], nl].

	message_tokens(orphaned_pack(Registry, Pack)) -->
		['Orphaned pack: ~q::~q'-[Registry, Pack], nl].

	message_tokens(unknown_pack(Registry, Pack)) -->
		['Unknown pack: ~q::~q'-[Registry, Pack], nl].

	message_tokens(unknown_pack(Pack)) -->
		['Unknown pack: ~q'-[Pack], nl].

	message_tokens(pack_not_installed(Pack)) -->
		['Pack is not installed: ~q'-[Pack], nl].

	message_tokens(pack_directory_not_found(Pack, Directory)) -->
		['Pack directory not found: ~q (~q)'-[Pack, Directory], nl].

	message_tokens(pack_archive_copy_failed(Pack, Command)) -->
		['Pack archive copy failed: ~q (~q)'-[Pack, Command], nl].

	message_tokens(pack_archive_download_failed(Pack, Command)) -->
		['Pack archive download failed: ~q (~q)'-[Pack, Command], nl].

	message_tokens(pack_archive_checksum_failed(Pack, Archive)) -->
		['Pack archive checksum check failed: ~q (~q)'-[Pack, Archive], nl].

	message_tokens(pack_archive_discarded(Pack)) -->
		['Discarding archive and attempting to download again: ~q'-[Pack], nl].

	message_tokens(pack_signature_download_failed(Pack, Archive)) -->
		['Pack archive signature download failed: ~q (~q)'-[Pack, Archive], nl].

	message_tokens(pack_archive_checksig_failed(Pack, Archive)) -->
		['Pack archive signature check failed: ~q (~q)'-[Pack, Archive], nl].

	message_tokens(pack_archive_decrypt_failed(Pack, Archive)) -->
		['Pack archive decrypt failed: ~q (~q)'-[Pack, Archive], nl].

	message_tokens(pack_archive_uncompress_failed(Pack, Archive)) -->
		['Pack archive uncompress failed: ~q (~q)'-[Pack, Archive], nl].

	% pack describe messages

	message_tokens(pack_info(Registry,Pack,Description,License,Home,Versions,Notes)) -->
		[	nl,
			'Registry:    ~q'-[Registry], nl,
			'Pack:        ~q'-[Pack], nl,
			'Description: ~w'-[Description], nl,
			'License:     ~w'-[License], nl,
			'Home:        ~w'-[Home], nl,
			'Versions:'-[], nl
		],
		pack_info(Versions),
		notes(Notes).

	message_tokens(installation_directory(Directory)) -->
		['Installation directory: ~q'-[Directory], nl].

	% pack search hits

	message_tokens(search_hits(Packs)) -->
		search_hits(Packs).

	search_hits([]) -->
		[].
	search_hits([Pack-Status| Packs]) -->
		['  - ~q ~w'-[Pack, Status], nl],
		search_hits(Packs).

	% auxiliary non-terminals

	pack_info([]) -->
		[nl].
	pack_info([Version| Versions]) -->
		pack_info_version(Version),
		pack_info(Versions).

	pack_info_version(version(Version, Status, URL, Cached, _Checksum, Dependencies, Portability)) -->
		['  ~w (~w)'-[Version, Status], nl],
		['    URL:          ~w'-[URL], nl],
		(	{Cached = cached(Archive)} ->
			['    Archive:      cached (~w)'-[Archive], nl]
		;	['    Archive:      uncached'-[], nl]
		),
		['    Dependencies: ~q'-[Dependencies], nl],
		['    Portability:  ~q'-[Portability], nl].

	defined_registries([]) -->
		[].
	defined_registries([defined(Registry,_,HowDefined,Pinned)| DefinedRegistries]) -->
		['  ~q'-[Registry]], registry_data(HowDefined, Pinned),
		defined_registries(DefinedRegistries).

	registry_data(git, true) -->
		[' (git; pinned)'-[], nl].
	registry_data(git, false) -->
		[' (git)'-[], nl].
	registry_data(archive, true) -->
		[' (archive; pinned)'-[], nl].
	registry_data(archive, false) -->
		[' (archive)'-[], nl].
	registry_data(directory, true) -->
		[' (directory; pinned)'-[], nl].
	registry_data(directory, false) -->
		[' (directory)'-[], nl].

	pinned(true) -->
		[' (pinned)'-[], nl].
	pinned(false) -->
		[nl].

	notes([]) -->
		[nl].
	notes([Note| Notes]) -->
		['Notes:'-[], nl],
		notes(Notes, Note).

	notes([], Note) -->
		note(Note), [nl].
	notes([Next| Notes], Note) -->
		note(Note),
		notes(Notes, Next).

	note(note(Action, Note)) -->
		{var(Action) -> ActionGrounded = '(all)'; ActionGrounded = Action},
		['  ~w - ~w'-[ActionGrounded, Note], nl].
	note(note(Action, Version, Note)) -->
		{var(Action) -> ActionGrounded = '(all)'; ActionGrounded = Action},
		{var(Version) -> VersionGrounded = '(all)'; VersionGrounded = Version},
		['  ~w - ~w - ~w'-[ActionGrounded, VersionGrounded, Note], nl].

:- end_category.
