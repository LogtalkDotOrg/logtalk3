%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(packs,
	imports((packs_common, options))).

	:- info([
		version is 0:13:0,
		author is 'Paulo Moura',
		date is 2021-02-17,
		comment is 'Pack handling predicates.'
	]).

	:- public(available/1).
	:- mode(available(+atom), one).
	:- info(available/1, [
		comment is 'Lists all the packs that are available for installation from the given registry.',
		argnames is ['Registry']
	]).

	:- public(available/0).
	:- mode(available, one).
	:- info(available/0, [
		comment is 'Lists all the packs that are available for installation from all defined registries.'
	]).

	:- public(installed/4).
	:- mode(installed(?atom, ?atom, ?compound, ? boolean), zero_or_more).
	:- info(installed/4, [
		comment is 'Enumerates by backtracking all installed packs.',
		argnames is ['Registry', 'Pack', 'Version', 'Pinned']
	]).

	:- public(installed/3).
	:- mode(installed(?atom, ?atom, ?compound), zero_or_more).
	:- info(installed/3, [
		comment is 'Enumerates by backtracking all installed packs.',
		argnames is ['Registry', 'Pack', 'Version']
	]).

	:- public(installed/0).
	:- mode(installed, one).
	:- info(installed/0, [
		comment is 'Lists all the packs that are installed.'
	]).

	:- public(outdated/4).
	:- mode(outdated(?atom, ?atom, ?compound, ?compound), zero_or_more).
	:- info(outdated/4, [
		comment is 'Enumerates by backtracking all installed but outdated packs (together with the current version installed and the latest version available).',
		argnames is ['Registry', 'Pack', 'Version', 'LatestVersion']
	]).

	:- public(outdated/0).
	:- mode(outdated, one).
	:- info(outdated/0, [
		comment is 'Lists all the packs that are installed but outdated.'
	]).

	:- public(orphaned/2).
	:- mode(orphaned(?atom, ?atom), zero_or_more).
	:- info(orphaned/2, [
		comment is 'Lists all the packs that are installed but whose registry is no longer defined.',
		argnames is ['Registry', 'Pack']
	]).

	:- public(orphaned/0).
	:- mode(orphaned, one).
	:- info(orphaned/0, [
		comment is 'Lists all the packs that are installed but whose registry is no longer defined.'
	]).

	:- public(describe/2).
	:- mode(describe(+atom, +atom), zero_or_one).
	:- info(describe/2, [
		comment is 'Describes a registered pack, including installed version if applicable.',
		argnames is ['Registry', 'Pack']
	]).

	:- public(describe/1).
	:- mode(describe(+atom), zero_or_one).
	:- info(describe/1, [
		comment is 'Describes a registered pack, including installed version if applicable.',
		argnames is ['Pack']
	]).

	:- public(search/1).
	:- mode(search(+atom), one).
	:- info(search/1, [
		comment is 'Searches packs whose name or description includes the search term (case sensitive).',
		argnames is ['Term']
	]).

	:- public(install/4).
	:- mode(install(+atom, +atom, +compound, ++list(compound)), zero_or_one).
	:- info(install/4, [
		comment is 'Installs a new pack using the specified options. Fails if the pack is unknown or already installed but not using a ``force(true)`` option. Fails also if the pack version is unknown.',
		argnames is ['Registry', 'Pack', 'Version', 'Options'],
		remarks is [
			'``force(Boolean)`` option' - 'Force re-installation if the pack is already installed. Default is ``false``.',
			'``clean(Boolean)`` option' - 'Clean pack archive after installation. Default is ``false``.',
			'``verbose(Boolean)`` option' - 'Verbose installing steps. Default is ``false``.',
			'``checksum(Boolean)`` option' - 'Verify pack archive checksum. Default is ``true``.',
			'``checksig(Boolean)`` option' - 'Verify pack archive signature. Default is ``false``.'
		]
	]).

	:- public(install/3).
	:- mode(install(+atom, +atom, ?compound), zero_or_one).
	:- info(install/3, [
		comment is 'Installs the specified version of a pack from the given registry using default options. Fails if the pack is already installed or unknown. Fails also if the pack version is unknown.',
		argnames is ['Registry', 'Pack', 'Version']
	]).

	:- public(install/2).
	:- mode(install(+atom, +atom), zero_or_one).
	:- info(install/2, [
		comment is 'Installs the latest version of a pack from the given registry using default options. Fails if the pack is already installed or unknown.',
		argnames is ['Registry', 'Pack']
	]).

	:- public(install/1).
	:- mode(install(+atom), zero_or_one).
	:- info(install/1, [
		comment is 'Installs a pack (if its name is unique among all registries) using default options. Fails if the pack is already installed or unknown. Fails also if the pack is available from multiple registries.',
		argnames is ['Pack']
	]).

	:- public(update/2).
	:- mode(update(+atom, +atom), zero_or_one).
	:- info(update/2, [
		comment is 'Updates an outdated pack using the specified options. Fails if the pack is unknown or not installed. Fails also if the pack is pinned and not using a ``force(true)`` option.',
		argnames is ['Pack', 'Options'],
		remarks is [
			'``force(Boolean)`` option' - 'Force update if the pack is pinned. Default is ``false``.',
			'``clean(Boolean)`` option' - 'Clean pack archive after updating. Default is ``false``.',
			'``verbose(Boolean)`` option' - 'Verbose updating steps. Default is ``false``.',
			'``checksum(Boolean)`` option' - 'Verify pack archive checksum. Default is ``true``.',
			'``checksig(Boolean)`` option' - 'Verify pack archive signature. Default is ``false``.'
		]
	]).

	:- public(update/1).
	:- mode(update(+atom), zero_or_one).
	:- info(update/1, [
		comment is 'Updates an outdated pack using default options. Fails if the pack is pinned, not installed, or unknown.',
		argnames is ['Pack']
	]).

	:- public(update/0).
	:- mode(update, zero_or_one).
	:- info(update/0, [
		comment is 'Updades all outdated packs (that are not pinned) using default options.'
	]).

	:- public(uninstall/2).
	:- mode(uninstall(+atom, ++list(compound)), zero_or_one).
	:- info(uninstall/2, [
		comment is 'Uninstalls a pack using the specified options. Fails if the pack is unknown or not installed. Fails also if the pack is pinned and not using a ``force(true)`` option.',
		argnames is ['Pack', 'Options'],
		remarks is [
			'``force(Boolean)`` option' - 'Force deletion if the pack is pinned. Default is ``false``.',
			'``clean(Boolean)`` option' - 'Clean pack archive after deleting. Default is ``false``.',
			'``verbose(Boolean)`` option' - 'Verbose uninstalling steps. Default is ``false``.'
		]
	]).

	:- public(uninstall/1).
	:- mode(uninstall(+atom), zero_or_one).
	:- info(uninstall/1, [
		comment is 'Uninstalls a pack using default options. Fails if the pack is pinned, not installed, or unknown.',
		argnames is ['Pack']
	]).

	:- public(clean/2).
	:- mode(clean(+atom, +atom), zero_or_one).
	:- info(clean/2, [
		comment is 'Cleans all pack archives. Fails if the the pack is unknown.',
		argnames is ['Registry', 'Pack']
	]).

	:- public(clean/1).
	:- mode(clean(+atom), zero_or_one).
	:- info(clean/1, [
		comment is 'Cleans all pack archives. Fails if the pack is not unknown.',
		argnames is ['Pack']
	]).

	:- public(clean/0).
	:- mode(clean, one).
	:- info(clean/0, [
		comment is 'Cleans all archives for all packs.'
	]).

	:- public(dependents/3).
	:- mode(dependents(+atom, +atom, -list(atom)), zero_or_one).
	:- info(dependents/3, [
		comment is 'Returns a list of all packs that depend on the given pack from the given registry. Fails if the pack is unknown.',
		argnames is ['Registry', 'Pack', 'Dependents']
	]).

	:- public(dependents/2).
	:- mode(dependents(+atom, +atom), zero_or_one).
	:- info(dependents/2, [
		comment is 'Prints a list of all packs that depend on the given pack from the given registry. Fails if the pack is unknown.',
		argnames is ['Registry', 'Pack']
	]).

	:- public(dependents/1).
	:- mode(dependents(+atom), zero_or_one).
	:- info(dependents/1, [
		comment is 'Prints a list of all packs that depend on the given pack if unique from all defined registries. Fails if the pack is unknown or available from multiple registries.',
		argnames is ['Pack']
	]).

	:- public(directory/2).
	:- mode(directory(?atom, ?atom), zero_or_more).
	:- info(directory/2, [
		comment is 'Enumerates by backtracking all packs and respective installation directories.',
		argnames is ['Pack', 'Directory']
	]).

	:- public(directory/1).
	:- mode(directory(?atom), zero_or_one).
	:- info(directory/1, [
		comment is 'Returns the directory where the packs are installed.',
		argnames is ['Directory']
	]).

	:- uses(list, [
		member/2, sort/4
	]).

	:- uses(logtalk, [
		expand_library_path/2, print_message/3
	]).

	:- uses(os, [
		ensure_file/1, delete_file/1, directory_exists/1, directory_files/3,
		decompose_file_name/3, decompose_file_name/4, file_exists/1,
		internal_os_path/2, make_directory_path/1, operating_system_type/1,
		path_concat/3
	]).

	:- uses(type, [
		check/2, valid/2
	]).

	:- initialization(startup).

	startup :-
		print_message(comment, packs, @'Initializing/checking packs directory structure...'),
		^^setup,
		print_message(comment, packs, @'Verifying availability of the required shell utilities...'),
		^^verify_commands_availability,
		print_message(comment, packs, @'Loading existing registry and pack specifications...'),
		expand_library_path(logtalk_packs(registries), RegistriesDirectory),
		directory_files(RegistriesDirectory, Registries, [type(directory), dot_files(false), paths(absolute)]),
		forall(
			member(Registry, Registries),
			^^load_registry(Registry)
		),
		print_message(banner, packs, @'Packs manager ready. For basic help, type: packs::help.').

	% packs availability predicates

	available(Registry) :-
		check(atom, Registry),
		(	\+ (
				implements_protocol(RegistryObject, registry_protocol),
				RegistryObject::name(Registry)
			) ->
			print_message(error, packs, unknown_registry(Registry)),
			fail
		;	print_message(information, packs, 'Available packs from registry: ~q'+[Registry]),
			(	setof(Pack, PackObject^registry_pack(Registry, Pack, PackObject), Packs) ->
				print_message(information, packs, Packs)
			;	print_message(information, packs, @'  (none)')
			)
		).

	available :-
		print_message(information, packs, @'Available packs:'),
		setof(Pack, PackObject^registry_pack(Registry, Pack, PackObject), Packs),
		print_message(information, packs, Registry::Packs),
		fail.
	available :-
		\+ registry_pack(_, _, _),
		print_message(information, packs, @'  (none)'),
		fail.
	available.

	% packs directory predicates

	directory(Pack, Directory) :-
		(	var(Pack) ->
			implements_protocol(PackObject, pack_protocol),
			PackObject::name(Pack)
		;	check(atom, Pack),
			implements_protocol(PackObject, pack_protocol),
			PackObject::name(Pack),
			!
		),
		expand_library_path(logtalk_packs(packs), Packs),
		path_concat(Packs, Pack, Directory),
		directory_exists(Directory).

	directory(Directory) :-
		expand_library_path(logtalk_packs(packs), Directory).

	% installed pack predicates

	installed(Registry, Pack, Version, Pinned) :-
		check(var_or(atom), Registry),
		check(var_or(atom), Pack),
		check(var_or(compound), Version),
		installed_pack(Registry, Pack, Version, Pinned).

	installed(Registry, Pack, Version) :-
		installed(Registry, Pack, Version, _).

	installed :-
		print_message(information, packs, @'Installed packs:'),
		installed_pack(Registry, Pack, Version, Pinned),
		print_message(information, packs, instaled_pack(Registry, Pack, Version, Pinned)),
		fail.
	installed :-
		\+ installed_pack(_, _, _, _),
		print_message(information, packs, @'  (none)'),
		fail.
	installed.

	% outdated pack predicates

	outdated(Registry, Pack, Version, LatestVersion) :-
		check(var_or(atom), Registry),
		check(var_or(atom), Pack),
		check(var_or(compound), Version),
		outdated_pack(Registry, Pack, Version, LatestVersion).

	outdated :-
		print_message(information, packs, @'Outdated packs:'),
		outdated(Registry, Pack, Version, LatestVersion),
		print_message(information, packs, outdated_pack(Registry, Pack, Version, LatestVersion)),
		fail.
	outdated :-
		\+ outdated_pack(_, _, _, _),
		print_message(information, packs, @'  (none)'),
		fail.
	outdated.

	% pack search predicates

	search(Term) :-
		check(atom, Term),
		print_message(information, packs, 'Packs whose name or description contain the term: ~q'+[Term]),
		findall(
			(Registry::Pack)-Status,
			search_hit(Term, Registry, Pack, Status),
			Packs
		),
		(	Packs == [] ->
			print_message(information, packs, @'  (none)')
		;	print_message(information, packs, search_hits(Packs))
		).

	search_hit(Term, Registry, Pack, Status) :-
		registry_pack(Registry, Pack, PackObject),
		(	sub_atom(Pack, _, _, _, Term) ->
			true
		;	PackObject::description(Description),
			sub_atom(Description, _, _, _, Term)
		),
		(	installed(Registry, Pack, _) ->
			(	pinned(Pack) ->
				Status = '(installed; pinned)'
			;	Status = '(installed)'
			)
		;	Status = ''
		).

	% install pack predicates

	install(Registry, Pack, Version, UserOptions) :-
		check(atom, Registry),
		check(atom, Pack),
		check(callable, Version),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	installed_pack(_, Pack, _, _),
			member(force(false), Options) ->
			print_message(error, packs, pack_already_installed(Pack)),
			fail
		;	registry_pack(Registry, Pack, PackObject) ->
			(	PackObject::version(Version, _, URL, CheckSum, _, _) ->
				print_message(comment, packs, installing_pack(Registry, Pack, Version)),
				install_pack(Registry, Pack, Version, URL, CheckSum, Options),
				print_message(comment, packs, pack_installed(Registry, Pack, Version))
			;	print_message(error, packs, unknown_pack_version(Registry, Pack, Version)),
				fail
			)
		;	print_message(error, packs, unknown_pack(Registry, Pack)),
			fail
		).

	install(Registry, Pack, Version) :-
		install(Registry, Pack, Version, []).

	install(Registry, Pack) :-
		check(atom, Registry),
		check(atom, Pack),
		(	installed_pack(_, Pack, _, _) ->
			print_message(error, packs, pack_already_installed(Pack)),
			fail
		;	latest_pack_version(Registry, Pack, LatestVersion, URL, CheckSum) ->
			print_message(comment, packs, installing_pack(Registry, Pack, LatestVersion)),
			install_pack(Registry, Pack, LatestVersion, URL, CheckSum, []),
			print_message(comment, packs, pack_installed(Registry, Pack, LatestVersion))
		;	print_message(error, packs, unknown_pack(Registry, Pack)),
			fail
		).

	install(Pack) :-
		check(atom, Pack),
		findall(
			Registry-Pack,
			registry_pack(Registry, Pack, _),
			RegistryPacks
		),
		(	RegistryPacks = [] ->
			print_message(error, packs, unknown_pack(Pack)),
			fail
		;	RegistryPacks = [Registry-Pack] ->
			install(Registry, Pack)
		;	print_message(error, packs, 'Pack available from multiple registries:'::RegistryPacks),
			fail
		).

	install_pack(Registry, Pack, Version, URL, CheckSum, Options) :-
		decompose_file_name(URL, _, Name, Extension),
		(	^^supported_archive(Extension) ->
			download(Registry, Pack, URL, Archive, Options),
			(	member(checksum(true), Options) ->
				verify_checksum(Pack, Archive, CheckSum, Options)
			;	true
			),
			(	member(checksig(true), Options) ->
				atom_concat(URL, '.asc', URLSig),
				download(Registry, Pack, URLSig, ArchiveSig, Options),
				verify_checksig(Pack, Archive, ArchiveSig, Options)
			;	true
			),
			uncompress(Pack, Archive, Path, Options),
			save_version(Path, Version),
			save_registry(Path, Registry),
			^^print_readme_file_path(Path)
		;	atom_concat(Name, Extension, Archive),
			print_message(error, packs, unsupported_archive_format(Archive)),
			fail
		),
		(	member(clean(true), Options) ->
			delete_archives(Registry, Pack)
		;	true
		).

	% uninstall pack predicates

	uninstall(Pack, UserOptions) :-
		check(atom, Pack),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	installed_pack(Registry, Pack, _, Pinned) ->
			(	Pinned == true,
				member(force(false), Options) ->
				print_message(error, packs, cannot_uninstall_pinned_pack(Pack)),
				fail
			;	print_message(comment, packs, uninstalling_pack(Registry, Pack)),
				uninstall_pack(Registry, Pack, Options),
				print_message(comment, packs, pack_uninstalled(Registry, Pack))
			)
		;	registry_pack(_, Pack, _) ->
			print_message(error, packs, pack_not_installed(Pack)),
			fail
		;	print_message(error, packs, unknown_pack(Pack)),
			fail
		).

	uninstall(Pack) :-
		uninstall(Pack, []).

	% describe pack predicates

	describe(Registry, Pack) :-
		check(atom, Registry),
		check(atom, Pack),
		(	registry_pack(Registry, Pack, PackObject) ->
			describe_pack(Registry, Pack, PackObject)
		;	print_message(error, packs, unknown_pack(Registry, Pack)),
			fail
		).

	describe(Pack) :-
		check(atom, Pack),
		(	\+ registry_pack(_, Pack, _) ->
			print_message(error, packs, unknown_pack(Pack)),
			fail
		;	forall(
				registry_pack(Registry, Pack, PackObject),
				describe_pack(Registry, Pack, PackObject)
			)
		).

	describe_pack(Registry, Pack, PackObject) :-
		PackObject::description(Description),
		PackObject::license(License),
		PackObject::home(Home),
		findall(
			version(Version, Status, URL, Checksum, Dependencies, Portability),
			(	PackObject::version(Version, Status0, URL, Checksum, Dependencies, Portability),
				(	installed(Registry, Pack, Version) ->
					atom_concat(Status0, '; installed', Status1),
					(	pinned(Pack) ->
						atom_concat(Status1, '; pinned', Status)
					;	Status = Status1
					)
				;	Status = Status0
				)
			),
			Versions
		),
		sort(1, (@>), Versions, Sorted),
		print_message(information, packs, pack_info(Registry,Pack,Description,License,Home,Sorted)).

	% pack readme predicates

	readme(Pack, ReadMeFile) :-
		directory(Pack, Directory),
		^^readme_file_path(Directory, ReadMeFile).

	% pinned pack handling

	pin(Pack) :-
		(	pin_file(Pack, File) ->
			ensure_file(File)
		;	print_message(error, packs, pack_not_installed(Pack)),
			fail
		).

	unpin(Pack) :-
		(	pin_file(Pack, File) ->
			(	file_exists(File) ->
				delete_file(File)
			;	true
			)
		;	print_message(error, packs, pack_not_installed(Pack)),
			fail
		).

	pinned(Pack) :-
		(	pin_file(Pack, File) ->
			file_exists(File)
		;	print_message(error, packs, pack_not_installed(Pack)),
			fail
		).

	pin_file(Pack, File) :-
		check(atom, Pack),
		directory(Pack, Directory),
		path_concat(Directory, 'PINNED.packs', File).

	% update pack predicates

	update(Pack, UserOptions) :-
		check(atom, Pack),
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		(	installed_pack(Registry, Pack, Version, Pinned) ->
			(	Pinned == true ->
				print_message(error, packs, cannot_update_pinned_pack(Pack)),
				fail
			;	update_pack(Registry, Pack, Version, Options)
			)
		;	registry_pack(_, Pack, _) ->
			print_message(error, packs, pack_not_installed(Pack)),
			fail
		;	print_message(error, packs, unknown_pack(Pack)),
			fail
		).

	update(Pack) :-
		update(Pack, []).

	update :-
		installed_pack(Registry, Pack, Version, false),
		update_pack(Registry, Pack, Version, []),
		fail.
	update.

	update_pack(Registry, Pack, Version, Options) :-
		(	latest_pack_version(Registry, Pack, LatestVersion, URL, CheckSum),
			Version @< LatestVersion ->
			print_message(comment, packs, updating_pack(Registry, Pack, Version, LatestVersion)),
			uninstall_pack(Registry, Pack, Options),
			install_pack(Registry, Pack, LatestVersion, URL, CheckSum, Options),
			print_message(comment, packs, pack_updated(Registry, Pack, LatestVersion))
		;	print_message(comment, packs, up_to_date_pack(Registry, Pack, Version))
		),
		(	member(clean(true), Options) ->
			delete_archives(Registry, Pack)
		;	true
		).

	% clean predicates

	clean(Registry, Pack) :-
		check(atom, Registry),
		check(atom, Pack),
		findall(
			Registry-Pack,
			registry_pack(Registry, Pack, _),
			RegistryPacks
		),
		(	RegistryPacks = [] ->
			print_message(error, packs, unknown_pack(Registry, Pack)),
			fail
		;	RegistryPacks = [Registry-Pack] ->
			delete_archives(Registry, Pack)
		;	print_message(error, packs, 'Pack available from multiple registries:'::RegistryPacks),
			fail
		).

	clean(Pack) :-
		check(atom, Pack),
		(	registry_pack(Registry, Pack, _) ->
			delete_archives(Registry, Pack)
		;	print_message(error, packs, unknown_pack(Pack)),
			fail
		).

	clean :-
		expand_library_path(logtalk_packs(packs), Directory),
		directory_files(Directory, Packs, [type(directory), dot_files(false)]),
		member(Pack, Packs),
		path_concat(Directory, Pack, Path),
		read_registry(Path, Registry),
		delete_archives(Registry, Pack),
		fail.
	clean.

	delete_archives(Registry, Pack) :-
		expand_library_path(logtalk_packs(archives), Archives),
		path_concat(Archives, packs, ArchivesPacks),
		path_concat(ArchivesPacks, Registry, ArchivesPacksRegistry),
		path_concat(ArchivesPacksRegistry, Pack, ArchivesPacksRegistryPack),
		(	directory_exists(ArchivesPacksRegistryPack) ->
			directory_files(ArchivesPacksRegistryPack, Files, [type(regular), dot_files(false), paths(absolute)]),
			forall(member(File, Files), delete_file(File))
		;	true
		).

	% orphaned pack predicates

	orphaned :-
		print_message(information, packs, @'Orphaned packs:'),
		orphaned_pack(Registry, Pack),
		print_message(information, packs, orphaned_pack(Registry, Pack)),
		fail.
	orphaned :-
		\+ orphaned_pack(_, _),
		print_message(information, packs, @'  (none)'),
		fail.
	orphaned.

	orphaned(Registry, Pack) :-
		check(var_or(atom), Registry),
		check(var_or(atom), Pack),
		orphaned_pack(Registry, Pack).

	orphaned_pack(Registry, Pack) :-
		installed_pack(Registry, Pack, _, _),
		expand_library_path(logtalk_packs(registries), Registries),
		path_concat(Registries, Registry, Directory),
		\+ directory_exists(Directory).

	% pack dependents predicates

	dependents(Registry, Pack, Dependents) :-
		check(atom, Registry),
		check(atom, Pack),
		(	registry_pack(Registry, Pack, _) ->
			pack_dependents(Registry, Pack, Dependents)
		;	print_message(error, packs, unknown_pack(Registry, Pack)),
			fail
		).

	dependents(Registry, Pack) :-
		dependents(Registry, Pack, Dependents),
		print_dependents(Registry, Pack, Dependents).

	dependents(Pack) :-
		check(atom, Pack),
		findall(
			Registry-Pack,
			registry_pack(Registry, Pack, _),
			RegistryPacks
		),
		(	RegistryPacks = [] ->
			print_message(error, packs, unknown_pack(Pack)),
			fail
		;	RegistryPacks = [Registry-Pack] ->
			pack_dependents(Registry, Pack, Dependents),
			print_dependents(Registry, Pack, Dependents)
		;	print_message(error, packs, 'Pack available from multiple registries:'::RegistryPacks),
			fail
		).

	print_dependents(Registry, Pack, Dependents) :-
		print_message(information, packs, 'Packs depending on pack: ~q::~q'+[Registry, Pack]),
		(	Dependents == [] ->
			print_message(information, packs, @'  (none)')
		;	print_message(information, packs, Dependents)
		).

	pack_dependents(Registry, Pack, Dependents) :-
		findall(
			Dependent,
			pack_dependent(Registry, Pack, Dependent),
			Dependents
		).

	pack_dependent(Registry, Pack, Dependent) :-
		expand_library_path(logtalk_packs(packs), Directory),
		directory_files(Directory, Dependents, [type(directory), dot_files(false), paths(relative)]),
		member(Dependent, Dependents),
		implements_protocol(DependentObject, pack_protocol),
		DependentObject::name(Dependent),
		DependentObject::version(_, _, _, _, Dependencies, _),
		(	member(Dependency, Dependencies),
			dependency(Dependency, Registry::Pack) ->
			true
		;	fail
		).

	dependency(Dependency >= _, Dependency).
	dependency(Dependency =< _, Dependency).
	dependency(Dependency > _, Dependency).
	dependency(Dependency < _, Dependency).
	dependency(Dependency = _, Dependency).

	% auxiliary predicates

	registry_pack(Registry, Pack, PackObject) :-
		implements_protocol(RegistryObject, registry_protocol),
		RegistryObject::name(Registry),
		object_property(RegistryObject, file(_, Directory)),
		implements_protocol(PackObject, pack_protocol),
		PackObject::name(Pack),
		object_property(PackObject, file(_, Directory)).

	pack_object(Pack, PackObject) :-
		(	var(Pack) ->
			implements_protocol(PackObject, pack_protocol),
			PackObject::name(Pack)
		;	implements_protocol(PackObject, pack_protocol),
			PackObject::name(Pack) ->
			!
		;	print_message(error, packs, unknown_pack(Pack)),
			fail
		).

	installed_pack(Registry, Pack, Version, Pinned) :-
		expand_library_path(logtalk_packs(packs), Directory),
		directory_files(Directory, Packs, [type(directory), dot_files(false), paths(relative)]),
		member(Pack, Packs),
		path_concat(Directory, Pack, Path),
		read_version(Path, Version),
		read_registry(Path, Registry),
		(	pinned(Pack) ->
			Pinned = true
		;	Pinned = false
		).

	outdated_pack(Registry, Pack, Version, LatestVersion) :-
		installed_pack(Registry, Pack, Version, _),
		latest_pack_version(Registry, Pack, LatestVersion, _, _),
		Version @< LatestVersion.

	latest_pack_version(Registry, Pack, LatestVersion, URL, CheckSum) :-
		registry_pack(Registry, Pack, PackObject),
		findall(
			Version-URL-CheckSum,
			PackObject::version(Version, _, URL, CheckSum, _, _),
			Versions
		),
		sort(1, (@>), Versions, [LatestVersion-URL-CheckSum| _]).

	uninstall_pack(Registry, Pack, Options) :-
		directory(Pack, Directory0),
		internal_os_path(Directory0, Directory),
		(	operating_system_type(windows) ->
			(	member(verbose(true), Options) ->
				atom_concat('del /s /q "', Directory, Command0),
				atom_concat(Command0, '" && rmdir /s /q "', Command1),
				atom_concat(Command1, Directory, Command2),
				atom_concat(Command2, '"', Command)
			;	atom_concat('del /s /q "', Directory, Command0),
				atom_concat(Command0, '" > nul && rmdir /s /q "', Command1),
				atom_concat(Command1, Directory, Command2),
				atom_concat(Command2, '" > nul', Command)
			)
		;	% assume unix
			(	member(verbose(true), Options) ->
				atom_concat('rm -rvf "', Directory, Command0)
			;	atom_concat('rm -rf "', Directory, Command0)
			),
			atom_concat(Command0, '"', Command)
		),
		^^command(Command, pack_uninstall_failed(Pack, Directory)),
		(	member(clean(true), Options) ->
			delete_archives(Registry, Pack)
		;	true
		).

	download(Registry, Pack, URL, Archive, Options) :-
		expand_library_path(logtalk_packs(archives), Archives),
		path_concat(Archives, packs, ArchivesPacks),
		path_concat(ArchivesPacks, Registry, ArchivesPacksRegistry),
		path_concat(ArchivesPacksRegistry, Pack, ArchivesPacksRegistryPack),
		decompose_file_name(URL, _, Basename),
		path_concat(ArchivesPacksRegistryPack, Basename, Archive0),
		internal_os_path(Archive0, Archive),
		make_directory_path(ArchivesPacksRegistryPack),
		(	file_exists(Archive) ->
			true
		;	(	member(verbose(true), Options) ->
				atom_concat('curl -v -L -o "', Archive, Command0)
			;	atom_concat('curl -s -S -L -o "', Archive, Command0)
			),
			atom_concat(Command0, '" ', Command1),
			atom_concat(Command1, URL, Command),
			^^command(Command, pack_archive_download_failed(Pack, Archive))
		).

	verify_checksum(Pack, Archive, CheckSum, Options) :-
		operating_system_type(OS),
		verify_checksum(OS, Pack, Archive, CheckSum, Options).

	verify_checksum(unix, Pack, Archive, sha256-CheckSum, Options) :-
		atom_concat('echo "', CheckSum, Command1),
		atom_concat(Command1, ' \"', Command2),
		atom_concat(Command2, Archive, Command3),
		(	member(verbose(true), Options) ->
			atom_concat(Command3, '\"" | sha256sum --check --quiet', Command)
		;	atom_concat(Command3, '\"" | sha256sum --check --status', Command)
		),
		^^command(Command, pack_archive_checksum_failed(Pack, Archive)).
	verify_checksum(windows, Pack, Archive, sha256-CheckSum, Options) :-
		(	member(verbose(true), Options) ->
			atom_concat('certutil -v -hashfile "', Archive, Command1)
		;	atom_concat('certutil -hashfile "', Archive, Command1)
		),
		atom_concat(Command1, '" SHA256 | find /v "hash" | find "', Command2),
		atom_concat(Command2, CheckSum, Command3),
		atom_concat(Command3, '" > nul', Command),
		^^command(Command, pack_archive_checksum_failed(Pack, Archive)).
	verify_checksum(unknown, Pack, Archive, sha256-CheckSum, Options) :-
		verify_checksum(unix, Pack, Archive, sha256-CheckSum, Options).

	verify_checksig(Pack, Archive, ArchiveSig, Options) :-
		(	member(verbose(true), Options) ->
			atom_concat('gpg -v --verify "', ArchiveSig, Command0),
			atom_concat(Command0, '" "', Command1),
			atom_concat(Command1, Archive, Command2),
			atom_concat(Command2, '"', Command)
		;	atom_concat('gpg --verify "', ArchiveSig, Command0),
			atom_concat(Command0, '" "', Command1),
			atom_concat(Command1, Archive, Command2),
			(	operating_system_type(windows) ->
				atom_concat(Command2, '" > nul 2>&1', Command)
			;	atom_concat(Command2, '" 2>/dev/null', Command)
			)
		),
		^^command(Command, pack_archive_checksig_failed(Pack, Archive)).

	uncompress(Pack, Archive, Path, Options) :-
		expand_library_path(logtalk_packs(packs), Packs),
		path_concat(Packs, Pack, Path),
		internal_os_path(Path, OSPath),
		make_directory_path(Path),
		^^tar_command(Tar),
		(	member(verbose(true), Options) ->
			atom_concat(Tar, ' -xvf "', Command0)
		;	atom_concat(Tar, ' -xf "', Command0)
		),
		atom_concat(Command0, Archive, Command1),
		atom_concat(Command1, '" --strip 1 --directory "', Command2),
		atom_concat(Command2, OSPath, Command3),
		atom_concat(Command3,  '"', Command),
		^^command(Command, pack_archive_uncompress_failed(Pack, Archive)).

	% installed pack version data handling

	save_version(Path, Version) :-
		path_concat(Path, 'VERSION.packs', File),
		open(File, write, Stream),
		writeq(Stream, Version), write(Stream, '.\n'),
		close(Stream).

	read_version(Path, Version) :-
		path_concat(Path, 'VERSION.packs', File),
		open(File, read, Stream),
		read(Stream, Version),
		close(Stream).

	version_file(Pack, File) :-
		check(atom, Pack),
		directory(Pack, Directory),
		path_concat(Directory, 'VERSION.packs', File).

	% installed pack registry data handling

	save_registry(Path, Registry) :-
		path_concat(Path, 'REGISTRY.packs', File),
		open(File, write, Stream),
		writeq(Stream, Registry), write(Stream, '.\n'),
		close(Stream).

	read_registry(Path, Registry) :-
		path_concat(Path, 'REGISTRY.packs', File),
		open(File, read, Stream),
		read(Stream, Registry),
		close(Stream).

	registry_file(Pack, File) :-
		check(atom, Pack),
		directory(Pack, Directory),
		path_concat(Directory, 'REGISTRY.packs', File).

	% options handling

	default_option(verbose(false)).
	default_option(clean(false)).
	default_option(force(false)).
	default_option(checksum(true)).
	default_option(checksig(false)).

	valid_option(verbose(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(clean(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(force(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(checksum(Boolean)) :-
		valid(boolean, Boolean).
	valid_option(checksig(Boolean)) :-
		valid(boolean, Boolean).

:- end_object.
