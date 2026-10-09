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
		version is 0:40:0,
		author is 'Paulo Moura',
		date is 2026-10-09,
		comment is 'Unit tests for the "packs" tool.'
	]).

	:- uses(list, [
		append/3, member/2, msort/2
	]).

	:- private(lock_install_event/3).
	:- dynamic(lock_install_event/3).

	:- private(capture_lock_installs/0).
	:- dynamic(capture_lock_installs/0).

	:- private(lock_message_event/1).
	:- dynamic(lock_message_event/1).

	:- private(capture_lock_messages/0).
	:- dynamic(capture_lock_messages/0).

	:- private(lock_version_fault/0).
	:- dynamic(lock_version_fault/0).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(packs_common).
	cover(registries).
	cover(packs).
	cover(registry_loader_hook).
	cover(packs_specs_hook).

	setup :-
		% the sample packs are defined using relative paths, which require
		% setting the working directory; but this hack to allow testing
		% pack installation may not work with all backend Prolog systems
		object_property(packs, file(_, Directory)),
		os::change_directory(Directory),
		% create the required packs directory structure
		packs::setup,
		% create a temporary key to test checking of pack signatures
		os::make_directory_path('.ring'),
		(	os::operating_system_type(windows) ->
			atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --quick-gen-key --batch --passphrase "" test_packs@logtalk.org > nul 2>&1'], Command1)
		;	atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --quick-gen-key --batch --passphrase "" test_packs@logtalk.org > /dev/null 2>&1'], Command1)
		),
		os::shell(Command1),
		(	os::operating_system_type(windows) ->
			atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --armor --detach-sign --local-user test_packs@logtalk.org "', Directory, '/test_files/asc/v1.0.0.tar.gz" > nul 2>&1'], Command2),
			atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --detach-sign --local-user test_packs@logtalk.org "', Directory, '/test_files/sig/v1.0.0.tar.gz" > nul 2>&1'], Command3)
		;	atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --armor --detach-sign --local-user test_packs@logtalk.org "', Directory, '/test_files/asc/v1.0.0.tar.gz" > /dev/null 2>&1'], Command2),
			atomic_list_concat(['gpg -q --homedir "', Directory, '.ring" --detach-sign --local-user test_packs@logtalk.org "', Directory, '/test_files/sig/v1.0.0.tar.gz" > /dev/null 2>&1'], Command3)
		),
		os::shell(Command2),
		os::shell(Command3).

	cleanup :-
		packs::reset,
		^^clean_file('.gpg'),
		^^clean_file('test_files/setup.txt'),
		^^clean_file('test_files/setup_lock.txt'),
		^^clean_file('test_files/setup_repo_lock.txt'),
		^^clean_file('test_files/asc/v1.0.0.tar.gz.asc'),
		^^clean_file('test_files/sig/v1.0.0.tar.gz.sig'),
		^^clean_directory('.ring'),
		^^clean_directory('test_files/repo'),
		^^clean_directory('test_files/logtalk_packs').

	% we start with no defined registries or installed packs

	test(packs_registries_logtalk_packs_1_01, deterministic(LogtalkPacks == Storage)) :-
		^^file_path('test_files/logtalk_packs/', Storage),
		registries::logtalk_packs(LogtalkPacks).

	test(packs_packs_logtalk_packs_1_01, deterministic(LogtalkPacks == Storage)) :-
		^^file_path('test_files/logtalk_packs/', Storage),
		packs::logtalk_packs(LogtalkPacks).

	test(packs_registries_logtalk_packs_0_01, deterministic) :-
		registries::logtalk_packs.

	test(packs_packs_logtalk_packs_0_02, deterministic) :-
		packs::logtalk_packs.

	test(packs_registries_prefix_1_01, deterministic(atom(Directory))) :-
		registries::prefix(Directory).

	test(packs_registries_prefix_0_01, deterministic) :-
		registries::prefix.

	test(packs_packs_prefix_1_01, deterministic(atom(Directory))) :-
		packs::prefix(Directory).

	test(packs_packs_prefix_0_01, deterministic) :-
		packs::prefix.

	test(packs_registries_list_0_01, deterministic) :-
		registries::list.

	test(packs_registries_defined_4_01, false) :-
		registries::defined(_, _, _, _).

	test(packs_packs_available_0_01, deterministic) :-
		packs::available.

	test(packs_packs_installed_0_01, deterministic) :-
		packs::installed.

	test(packs_packs_installed_4_01, false) :-
		packs::installed(_, _, _, _).

	test(packs_packs_installed_3_01, false) :-
		packs::installed(_, _, _).

	test(packs_packs_outdated_0_01, deterministic) :-
		packs::outdated.

	test(packs_packs_outdated_4_01, false) :-
		packs::outdated(_, _, _, _).

	test(packs_packs_orphaned_0_01, deterministic) :-
		packs::orphaned.

	test(packs_packs_orphaned_2_01, false) :-
		packs::orphaned(_, _).

	test(packs_registries_clean_0_01, deterministic) :-
		registries::clean.

	test(packs_packs_clean_0_01, deterministic) :-
		packs::clean.

	test(packs_registries_lint_0_01, deterministic) :-
		registries::lint.

	test(packs_packs_lint_0_01, deterministic) :-
		packs::lint.

	test(packs_registries_update_0_01, deterministic) :-
		registries::update.

	test(packs_packs_update_0_01, deterministic) :-
		packs::update.

	test(packs_registries_help_0_01, deterministic) :-
		registries::help.

	test(packs_packs_help_0_01, deterministic) :-
		packs::help.

	test(packs_packs_verify_commands_availability_0_01, deterministic) :-
		packs::verify_commands_availability.

	% commit option validation

	test(packs_registries_add_3_02, error(domain_error(option, commit('')))) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [commit('')]).

	test(packs_registries_add_3_03, error(domain_error(option, commit('abc123')))) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [commit('abc123')]).

	test(packs_registries_add_3_04, error(domain_error(option, commit('gggggggggggggggggggggggggggggggggggggggg')))) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [commit('gggggggggggggggggggggggggggggggggggggggg')]).

	test(packs_registries_add_3_05, false) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [commit('0123456789abcdef0123456789abcdef01234567')]).

	test(packs_registries_add_3_06, false) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [commit('0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef')]).

	% now we add a local registry

	test(packs_registries_add_1_01, deterministic) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(URL).

	test(packs_registries_add_3_01, deterministic) :-
		^^file_url('test_files/local_1_d', URL),
		registries::add(local_1_d, URL, [update(true)]).

	test(packs_registries_defined_4_02, deterministic(Registries == [local_1_d])) :-
		findall(Registry, registries::defined(Registry, _, _, _), Registries).

	test(packs_registries_lint_1_01, deterministic) :-
		registries::lint(local_1_d).

	% registry describe predicate

	test(packs_registries_describe_1_01, deterministic) :-
		registries::describe(local_1_d).

	% registry directory

	test(packs_registries_directory_2_01, deterministic(atom(Directory))) :-
		registries::directory(local_1_d, Directory).

	test(packs_registries_directory_1_01, deterministic) :-
		registries::directory(local_1_d).

	% registry readme

	test(packs_registries_readme_2_01, deterministic((Readme == FileUpperCase; Readme == FileLowerCase))) :-
		^^file_path('test_files/logtalk_packs/registries/local_1_d/README.md', FileUpperCase),
		% some backends convert paths to lower case on Windows
		^^file_path('test_files/logtalk_packs/registries/local_1_d/readme.md', FileLowerCase),
		registries::readme(local_1_d, Readme).

	test(packs_registries_readme_1_01, deterministic) :-
		registries::readme(local_1_d).

	test(packs_registries_provides_2_01, deterministic(Pairs == [local_1_d-alt, local_1_d-asc, local_1_d-badcheck, local_1_d-badsig, local_1_d-bar, local_1_d-deprecated, local_1_d-foo, local_1_d-gpg, local_1_d-sig])) :-
		setof(Registry-Pack, registries::provides(Registry, Pack), Pairs).

	test(packs_registries_update_1_01, deterministic) :-
		registries::update(local_1_d).

	test(packs_registries_clean_1_01, deterministic) :-
		registries::clean(local_1_d).

	test(packs_registries_pin_1_01, deterministic) :-
		registries::pin(local_1_d).

	test(packs_registries_pin_1_02, deterministic) :-
		registries::pin(local_1_d),
		registries::pin(local_1_d).

	test(packs_registries_unpin_1_01, deterministic) :-
		registries::unpin(local_1_d).

	test(packs_registries_unpin_1_02, deterministic) :-
		registries::unpin(local_1_d),
		registries::unpin(local_1_d).

	test(packs_registries_pinned_1_01, deterministic) :-
		registries::pin(local_1_d),
		registries::pinned(local_1_d).

	test(packs_registries_pinned_1_02, false) :-
		registries::unpin(local_1_d),
		registries::pinned(local_1_d).

	test(packs_packs_lint_2_01, deterministic) :-
		packs::lint(local_1_d, foo).

	test(packs_packs_lint_1_01, deterministic) :-
		packs::lint(foo).

	test(packs_packs_versions_3_01, deterministic(Versions == [3:0:0,2:0:0,1:0:0])) :-
		packs::versions(local_1_d, foo, Versions).

	test(packs_packs_available_2_01, deterministic(Packs == [local_1_d-alt, local_1_d-asc, local_1_d-badcheck, local_1_d-badsig, local_1_d-bar, local_1_d-deprecated, local_1_d-foo, local_1_d-gpg, local_1_d-sig])) :-
		findall(Registry-Pack, packs::available(Registry, Pack), Packs0),
		msort(Packs0, Packs).

	test(packs_packs_available_1_01, deterministic) :-
		packs::available(local_1_d).

	test(packs_packs_dependents_3_01, deterministic(Dependents == [])) :-
		packs::dependents(local_1_d, foo, Dependents).

	test(packs_packs_dependents_2_01, deterministic) :-
		packs::dependents(local_1_d, foo).

	test(packs_packs_dependents_1_01, deterministic) :-
		packs::dependents(foo).

	test(packs_packs_install_1_01, deterministic) :-
		packs::install(bar).

	test(packs_packs_install_1_02, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(local_1_d, bar, Version, Pinned).

	test(packs_packs_uninstall_1_01, deterministic) :-
		packs::uninstall(bar).

	test(packs_packs_uninstall_1_02, false) :-
		packs::installed(local_1_d, bar, _, _).

	test(packs_packs_outdated_1_01, deterministic) :-
		packs::outdated(local_1_d).

	% add a second local registry

	test(packs_registries_add_2_01, deterministic) :-
		^^file_url('test_files/local_2_d.zip', URL),
		registries::add(local_2_d, URL).

	test(packs_registries_defined_4_03, deterministic(Registries == [local_1_d, local_2_d])) :-
		findall(Registry, registries::defined(Registry, _, _, _), Registries0),
		list::msort(Registries0, Registries).

	test(packs_registries_unpin_0_01, deterministic) :-
		registries::unpin.

	test(packs_registries_unpin_0_02, false) :-
		registries::defined(_, _, _, true).

	test(packs_registries_pin_0_01, deterministic) :-
		registries::pin.

	test(packs_registries_pin_0_02, deterministic(Registries == [local_1_d, local_2_d])) :-
		findall(Registry, registries::defined(Registry, _, _, true), Registries0),
		list::msort(Registries0, Registries).

	test(packs_registries_provides_2_02, deterministic(Pairs == [local_1_d-alt, local_1_d-asc, local_1_d-badcheck, local_1_d-badsig, local_1_d-bar, local_1_d-deprecated, local_1_d-foo, local_1_d-gpg, local_1_d-sig, local_2_d-baz])) :-
		setof(Registry-Pack, registries::provides(Registry, Pack), Pairs).

	test(packs_packs_available_2_02, deterministic(Packs == [local_1_d-alt, local_1_d-asc, local_1_d-badcheck, local_1_d-badsig, local_1_d-bar,local_1_d-deprecated, local_1_d-foo, local_1_d-gpg, local_1_d-sig, local_2_d-baz])) :-
		findall(Registry-Pack, packs::available(Registry, Pack), Packs0),
		msort(Packs0, Packs).

	% install packs with dependencies

	test(packs_packs_install_4_01, deterministic) :-
		packs::install(local_1_d, foo, 1:0:0, [compatible(false)]).

	test(packs_packs_install_4_02, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(local_1_d, foo, Version, Pinned).

	test(packs_packs_install_4_03, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(local_2_d, baz, Version, Pinned).

	test(packs_packs_install_4_04, deterministic(Version-Pinned == (2:0:0)-false)) :-
		packs::install(local_1_d, foo, 2:0:0, [update(true), compatible(false)]),
		packs::installed(local_1_d, foo, Version, Pinned).

	test(packs_packs_install_4_05, deterministic) :-
		packs::install(local_1_d, alt, 1:0:0, [compatible(false)]).

	test(packs_packs_install_4_06, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(local_1_d, alt, Version, Pinned).

	test(packs_packs_install_4_07, false) :-
		packs::install(local_1_d, gpg, 1:0:0, [gpg('--no-sig-cache --batch --passphrase wrong456')]).

	test(packs_packs_install_4_08, deterministic) :-
		packs::install(local_1_d, gpg, 1:0:0, [gpg('--no-sig-cache --batch --passphrase test123')]).

	test(packs_packs_install_4_09, false) :-
		object_property(packs, file(_, Directory)),
		atomic_list_concat(['--homedir "', Directory, '.ring"'], Homedir),
		packs::install(local_1_d, badsig, 1:0:0, [checksig(true), gpg(Homedir)]).

	test(packs_packs_install_4_10, deterministic) :-
		object_property(packs, file(_, Directory)),
		atomic_list_concat(['--homedir "', Directory, '.ring"'], Homedir),
		packs::install(local_1_d, asc, 1:0:0, [checksig(true), gpg(Homedir)]).

	test(packs_packs_install_4_11, deterministic) :-
		object_property(packs, file(_, Directory)),
		atomic_list_concat(['--homedir "', Directory, '.ring"'], Homedir),
		packs::install(local_1_d, sig, 1:0:0, [checksig(true), gpg(Homedir)]).

	test(packs_packs_install_4_12, false) :-
		packs::install(badcheck).

	test(packs_packs_dependents_3_02, deterministic(Dependents == [foo])) :-
		packs::dependents(local_2_d, baz, Dependents).

	test(packs_packs_install_2_01, false) :-
		packs::install(local_1_d, deprecated).

	test(packs_packs_install_3_01, deterministic) :-
		packs::install(local_1_d, deprecated, 1:0:0).

	% print installed packs

	test(packs_packs_installed_1_01, deterministic) :-
		packs::installed(local_1_d).

	% update all installed packs

	test(packs_packs_update_2_01, deterministic(OldVersion == NewVersion)) :-
		packs::installed(local_1_d, deprecated, OldVersion, _),
		packs::update(deprecated, []),
		packs::installed(local_1_d, deprecated, NewVersion, _).

	test(packs_packs_update_2_02, deterministic(OldVersion \== NewVersion)) :-
		packs::installed(local_1_d, deprecated, OldVersion, _),
		packs::update(deprecated, [status(all)]),
		packs::installed(local_1_d, deprecated, NewVersion, _).

	test(packs_packs_update_2_03, deterministic(Version-Pinned == (3:0:0)-false)) :-
		packs::uninstall(foo),
		packs::install(local_1_d, foo, 1:0:0, [compatible(false)]),
		packs::update(foo, [compatible(false)]),
		packs::installed(local_1_d, foo, Version, Pinned).

	% update packs

	test(packs_packs_update_1_01, deterministic) :-
		packs::update(baz).

	test(packs_packs_update_2_04, deterministic) :-
		packs::update(baz, [force(true)]).

	test(packs_packs_update_3_01, deterministic) :-
		packs::uninstall(foo),
		packs::install(local_1_d, foo, 1:0:0, [compatible(false)]),
		packs::update(foo, 2:0:0, [clean(true), compatible(false)]).

	test(packs_packs_update_2_05, deterministic(OldVersion == NewVersion)) :-
		packs::installed(local_1_d, foo, OldVersion, _),
		packs::update(foo, [status(stable)]),
		packs::installed(local_1_d, foo, NewVersion, _).

	% clean pack archives

	test(packs_packs_clean_2_01, deterministic) :-
		packs::clean(local_2_d, baz).

	test(packs_packs_clean_1_01, deterministic) :-
		packs::clean(foo).

	% pack directory

	test(packs_packs_directory_2_01, deterministic(atom(Directory))) :-
		packs::directory(foo, Directory).

	test(packs_packs_directory_1_01, deterministic) :-
		packs::directory(foo).

	% pack readme file

	test(packs_packs_readme_1_01, deterministic) :-
		packs::readme(foo).

	test(packs_packs_readme_2_01, deterministic(os::file_exists(ReadMeFile))) :-
		packs::readme(foo, ReadMeFile).

	% pack describe predicates

	test(packs_packs_describe_2_01, deterministic) :-
		packs::describe(local_1_d, foo).

	test(packs_packs_describe_1_01, deterministic) :-
		packs::describe(baz).

	% pack query predicates

	test(packs_packs_pack_object_3_01, deterministic(PackObject == foo_pack)) :-
		packs::pack_object(local_1_d, foo, PackObject).

	test(packs_packs_pack_metadata_4_01, deterministic(atom(Directory))) :-
		fixture_loaded_state(foo, ExpectedLoaded),
		packs::installed(local_1_d, foo, Version, Pinned),
		packs::pack_metadata(local_1_d, foo, Version, metadata(Name, Description, License, Home, SourceURL, Checksum, Dependencies, Portability, Directory, Pinned, Installed, Loaded)),
		^^assertion(Name == foo),
		^^assertion(Description == 'A local pack for testing'),
		^^assertion(License == 'Apache-2.0'),
		^^assertion(Home == 'file://test_files/foo'),
		^^assertion(SourceURL == 'file://test_files/foo'),
		^^assertion(Checksum == none),
		^^assertion(Dependencies == [logtalk @>= 3:42:0, local_2_d::baz @>= 1:0:0, local_2_d::baz @< 2:0:0]),
		^^assertion(Portability == [eclipse, gnu, swi, sicstus, yap, trealla, xsb]),
		^^assertion(Installed == true),
		^^assertion(Loaded == ExpectedLoaded).

	test(packs_packs_pack_property_4_01, deterministic) :-
		packs::installed(local_1_d, foo, Version, _),
		packs::pack_property(local_1_d, foo, Version, license('Apache-2.0')).

	test(packs_packs_pack_property_4_02, deterministic) :-
		fixture_loaded_state(foo, Loaded),
		packs::installed(local_1_d, foo, Version, _),
		packs::pack_property(local_1_d, foo, Version, loaded(Loaded)).

	test(packs_packs_loaded_pack_3_01, deterministic) :-
		packs::directory(foo, Directory),
		os::path_concat(Directory, 'loader.lgt', Loader),
		logtalk_load(Loader),
		packs::installed(local_1_d, foo, Version, _),
		packs::loaded_pack(local_1_d, foo, Version).

	test(packs_packs_loaded_pack_file_4_01, true(atom(File))) :-
		packs::installed(local_1_d, foo, Version, _),
		packs::loaded_pack_file(local_1_d, foo, Version, File),
		packs::directory(foo, Directory),
		^^assertion(sub_atom(File, 0, _, _, Directory)).

	test(packs_packs_pack_property_4_03, deterministic) :-
		packs::installed(local_1_d, foo, Version, _),
		packs::pack_property(local_1_d, foo, Version, loaded(true)).

	test(packs_packs_pack_dependency_6_01, deterministic(Dependencies == [local_2_d-baz-(1:0:0)])) :-
		findall(
			DependencyRegistry-DependencyPack-DependencyVersion,
			packs::pack_dependency(local_1_d, foo, 2:0:0, DependencyRegistry, DependencyPack, DependencyVersion),
			Dependencies0
		),
		msort(Dependencies0, Dependencies).

	test(packs_packs_pack_dependency_6_02, deterministic(Dependencies == [local_1_d-foo-(2:0:0)])) :-
		findall(
			DependencyRegistry-DependencyPack-DependencyVersion,
			packs::pack_dependency(local_1_d, alt, 1:0:0, DependencyRegistry, DependencyPack, DependencyVersion),
			Dependencies0
		),
		msort(Dependencies0, Dependencies).

	test(packs_packs_loaded_pack_dependency_6_01, deterministic(Dependencies == [local_2_d-baz-(1:0:0)])) :-
		packs::directory(baz, Directory),
		os::path_concat(Directory, 'loader.lgt', Loader),
		logtalk_load(Loader),
		findall(
			DependencyRegistry-DependencyPack-DependencyVersion,
			packs::loaded_pack_dependency(local_1_d, foo, 2:0:0, DependencyRegistry, DependencyPack, DependencyVersion),
			Dependencies0
		),
		msort(Dependencies0, Dependencies).

	% pin and unpin packs

	test(packs_packs_pin_1_01, deterministic) :-
		packs::pin(foo).

	test(packs_packs_pinned_1_01, deterministic) :-
		packs::pinned(foo).

	test(packs_packs_unpin_1_01, deterministic) :-
		packs::unpin(foo).

	test(packs_packs_pinned_1_02, false) :-
		packs::pinned(foo).

	test(packs_packs_pin_0_01, deterministic) :-
		packs::pin.

	test(packs_packs_pin_0_02, all(Pinned == true)) :-
		packs::installed(_, _, _, Pinned).

	test(packs_packs_unpin_0_01, deterministic) :-
		packs::unpin.

	test(packs_packs_unpin_0_02, all(Pinned == false)) :-
		packs::installed(_, _, _, Pinned).

	% pack search predicates

	test(packs_packs_search_1_01, deterministic) :-
		packs::search(local).

	% save and restore setups

	test(packs_packs_save_2_01, deterministic(os::file_exists(Setup))) :-
		% avoid asking for passphrase when restoring due to the gpg encrypted pack
		packs::uninstall(gpg),
		^^file_path('test_files/setup.txt', Setup),
		packs::save(Setup).

	test(packs_packs_restore_2_01, deterministic) :-
		packs::uninstall,
		packs::clean,
		registries::delete,
		registries::clean.

	test(packs_packs_restore_2_02, false) :-
		packs::installed(_, _, _, _).

	test(packs_packs_restore_2_03, false) :-
		registries::defined(_, _, _, _).

	test(packs_packs_restore_2_04, deterministic) :-
		^^file_path('test_files/setup.txt', Setup),
		packs::restore(Setup, [compatible(false)]).

	test(packs_packs_restore_2_05, deterministic(HowDefined-Pinned == directory-true)) :-
		registries::defined(local_1_d, _, HowDefined, Pinned).

	test(packs_packs_restore_2_06, deterministic(HowDefined-Pinned == archive-true)) :-
		registries::defined(local_2_d, _, HowDefined, Pinned).

	test(packs_packs_restore_2_07, deterministic(Version-Pinned == (2:0:0)-false)) :-
		packs::installed(local_1_d, foo, Version, Pinned).

	test(packs_packs_restore_2_08, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(local_2_d, baz, Version, Pinned).

	test(packs_packs_save_2_02, error(domain_error(lock_setup, registry(local_1_d)))) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		packs::save(Setup, [lock(true)]).

	test(packs_packs_restore_2_09, deterministic) :-
		packs::uninstall,
		packs::clean,
		registries::delete,
		registries::clean.

	test(packs_packs_restore_2_10, false) :-
		^^file_path('test_files/lock_files/directory_registry.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_packs_restore_2_11, false) :-
		registries::defined(local_1_d, _, _, _).

	test(packs_packs_restore_2_12, false) :-
		registries::defined(local_2_d, _, _, _).

	test(packs_packs_restore_2_13, false) :-
		packs::installed(local_1_d, foo, _, _).

	test(packs_packs_restore_2_14, false) :-
		packs::installed(local_2_d, baz, _, _).

	test(packs_lock_checksum_required, error(consistency_error(compatible_options, lock(true), checksum(false)))) :-
		^^file_path('test_files/lock_files/directory_registry.txt', Setup),
		packs::restore(Setup, [lock(true), checksum(false)]).

	test(packs_lock_missing_commit, false) :-
		^^file_path('test_files/lock_files/missing_commit.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_unknown_fact, false) :-
		^^file_path('test_files/lock_files/unknown_fact.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_non_ground, false) :-
		^^file_path('test_files/lock_files/non_ground.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_duplicate_version, false) :-
		^^file_path('test_files/lock_files/duplicate_version.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_dangling_pin, false) :-
		^^file_path('test_files/lock_files/dangling_pin.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_operation_failure, false) :-
		^^file_path('test_files/lock_files/unavailable_registry.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	% git registry lockfile setup and restore

	test(packs_packs_restore_2_15, deterministic) :-
		packs::uninstall,
		packs::clean,
		registries::delete,
		registries::clean.

	test(packs_registries_add_1_03, deterministic(os::directory_exists(RepoDirectory))) :-
		^^file_path('test_files/logtalk_packs/repo_fixture', Destination),
		(   os::directory_exists(Destination) ->
			os::delete_directory_and_contents(Destination)
		;   true
		),
		os::make_directory_path(Destination),
		^^file_path('test_files/repo.zip', Archive),
		unzip_archive(Archive, Destination),
		^^file_path('test_files/logtalk_packs/repo_fixture/repo', RepoDirectory).

	test(packs_registries_add_3_07, deterministic) :-
		first_repo_commit(Commit),
		^^file_url('test_files/logtalk_packs/repo_fixture/repo', URL),
		registries::add(repo, URL, [commit(Commit)]).

	test(packs_packs_install_1_03, deterministic) :-
		packs::install(repo).

	test(packs_packs_install_4_13, deterministic(Version-Pinned == (1:0:0)-false)) :-
		packs::installed(repo, repo, Version, Pinned).

	test(packs_lock_save_preserves_destination, deterministic(Term == existing)) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		open(Setup, write, Output),
		write_lock_terms([existing], Output),
		close(Output),
		catch(packs::save(Setup, [lock(true)]), error(domain_error(lock_setup, _), _), true),
		open(Setup, read, Input),
		read(Input, Term),
		close(Input).

	test(packs_packs_save_2_03, error(domain_error(lock_setup, pack(repo, repo, 1:0:0)))) :-
		^^file_path('test_files/setup_repo_lock.txt', Setup),
		packs::save(Setup, [lock(true)]).

	test(packs_packs_restore_2_16, deterministic) :-
		packs::uninstall,
		packs::clean,
		registries::delete,
		registries::clean.

	test(packs_packs_restore_2_17, false) :-
		^^file_path('test_files/lock_files/directory_pack.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_packs_restore_2_18, false) :-
		packs::installed(repo, repo, _, _).

	test(packs_lock_fixture_setup, deterministic) :-
		unpack_lock_fixture,
		^^file_url('test_files/logtalk_packs/lock_fixture', URL),
		registries::add(lock_fixture, URL),
		packs::install(lock_fixture, lock_b, 2:0:0).

	test(packs_lock_exact_dependency_versions, deterministic(Installs == [lock_b-(1:0:0), lock_a-(1:0:0), lock_c-(1:0:0), lock_d-(1:0:0)])) :-
		fixture_lock_terms([lock_a, lock_b, lock_c, lock_d], Terms),
		advance_lock_fixture,
		capture_locked_restore(Terms, [], Installs).

	test(packs_lock_save_creates_file, deterministic(os::file_exists(Setup))) :-
		^^clean_file('test_files/setup_repo_lock.txt'),
		^^file_path('test_files/setup_repo_lock.txt', Setup),
		\+ os::file_exists(Setup),
		packs::save(Setup, [lock(true)]).

	test(packs_lock_save_dirty_registry, deterministic(First == Second)) :-
		registries::directory(lock_fixture, Directory),
		git::commit_hash(Directory, Commit),
		^^file_path('test_files/setup_repo_lock.txt', Setup),
		read_fixture_bytes(Setup, First),
		write_fixture_file(Directory, 'loader.lgt', [dirty]),
		catch(
			packs::save(Setup, [lock(true)]),
			error(domain_error(lock_setup, registry(lock_fixture)), _),
			Rejected = true
		),
		fixture_git_command(Directory, 'checkout -- loader.lgt'),
		^^assertion(Rejected == true),
		git::commit_hash(Directory, Commit),
		read_fixture_bytes(Setup, Second).

	test(packs_lock_save_staged_registry, deterministic) :-
		registries::directory(lock_fixture, Directory),
		write_fixture_file(Directory, 'loader.lgt', [dirty]),
		fixture_git_command(Directory, 'add loader.lgt'),
		^^clean_file('test_files/setup_lock.txt'),
		^^file_path('test_files/setup_lock.txt', Setup),
		catch(
			packs::save(Setup, [lock(true)]),
			error(domain_error(lock_setup, registry(lock_fixture)), _),
			Rejected = true
		),
		fixture_git_command(Directory, 'reset -q HEAD -- loader.lgt'),
		fixture_git_command(Directory, 'checkout -- loader.lgt'),
		^^assertion(Rejected == true),
		^^assertion(\+ os::file_exists(Setup)).

	test(packs_lock_save_untracked_registry, deterministic) :-
		registries::directory(lock_fixture, Directory),
		write_fixture_file(Directory, '.DS_Store', [untracked]),
		^^file_path('test_files/setup_lock.txt', Setup),
		packs::save(Setup, [lock(true)]),
		os::path_concat(Directory, '.DS_Store', Untracked),
		os::delete_file(Untracked),
		^^assertion(os::file_exists(Setup)).

	test(packs_lock_save_deterministic, deterministic(First == Second)) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		packs::save(Setup, [lock(true)]),
		read_fixture_bytes(Setup, First),
		packs::save(Setup, [lock(true)]),
		read_fixture_bytes(Setup, Second).

	test(packs_lock_saved_round_trip, deterministic) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		read_fixture_terms(Setup, Terms),
		member(lock_registry_commit(lock_fixture, Commit), Terms),
		!,
		packs::uninstall,
		registries::delete(lock_fixture, [force(true)]),
		packs::restore(Setup, [lock(true)]),
		registries::directory(lock_fixture, Directory),
		git::commit_hash(Directory, Commit),
		packs::installed(lock_fixture, lock_a, 1:0:0),
		packs::installed(lock_fixture, lock_b, 1:0:0),
		packs::installed(lock_fixture, lock_c, 1:0:0),
		packs::installed(lock_fixture, lock_d, 1:0:0).

	test(packs_lock_update_verifies_archives, deterministic(Installs == [lock_b-(1:0:0), lock_a-(1:0:0), lock_c-(1:0:0), lock_d-(1:0:0)])) :-
		fixture_lock_terms([lock_a, lock_b, lock_c, lock_d], Terms),
		capture_locked_restore(Terms, [update(true)], Installs).

	test(packs_lock_restore_removes_stale_files, deterministic) :-
		fixture_lock_terms([lock_a, lock_b, lock_c, lock_d], Terms),
		packs::directory(lock_b, PackDirectory),
		write_fixture_file(PackDirectory, 'obsolete.txt', [obsolete]),
		os::path_concat(PackDirectory, 'obsolete.txt', StalePackFile),
		registries::directory(lock_fixture, RegistryDirectory),
		write_fixture_file(RegistryDirectory, 'obsolete_pack.lgt', [obsolete]),
		os::path_concat(RegistryDirectory, 'obsolete_pack.lgt', StaleRegistryFile),
		write_fixture_file(RegistryDirectory, '.git/lock_restore_marker', [obsolete]),
		os::path_concat(RegistryDirectory, '.git/lock_restore_marker', RestoreMarker),
		restore_lock_terms(Terms),
		^^assertion(\+ os::file_exists(StalePackFile)),
		^^assertion(\+ os::file_exists(StaleRegistryFile)),
		^^assertion(\+ os::file_exists(RestoreMarker)),
		packs::installed(lock_fixture, lock_b, 1:0:0).

	test(packs_lock_restore_replaces_different_version, deterministic) :-
		fixture_lock_terms([lock_a, lock_b, lock_c, lock_d], Terms),
		packs::uninstall,
		packs::install(lock_fixture, lock_b, 2:0:0),
		packs::directory(lock_b, Directory),
		write_fixture_file(Directory, 'obsolete.txt', [obsolete]),
		os::path_concat(Directory, 'obsolete.txt', StaleFile),
		restore_lock_terms(Terms),
		^^assertion(\+ os::file_exists(StaleFile)),
		packs::installed(lock_fixture, lock_b, 1:0:0).

	test(packs_lock_corrupt_cache, deterministic) :-
		fixture_lock_terms([lock_b], Terms),
		packs::logtalk_packs(Storage),
		os::path_concat(Storage, 'archives/packs/lock_fixture/lock_b/v1.0.0.tar.gz', Cache),
		open(Cache, write, Stream),
		write_lock_terms([corrupted], Stream),
		close(Stream),
		observe_locked_restore(Terms, [], Outcome, Messages, Installs),
		Outcome == true,
		lgtunit::assertion(member(pack_archive_discarded(lock_b), Messages)),
		Installs == [lock_b-(1:0:0)].

	test(packs_lock_missing_integrity, deterministic) :-
		^^file_path('test_files/lock_files/missing_integrity.txt', Setup),
		observe_lock_file(Setup, [], Outcome, Messages, Installs),
		Outcome == false,
		Installs == [],
		lgtunit::assertion(\+ member(@'Restored setup', Messages)).

	test(packs_lock_conflicting_pack_version, false) :-
		^^file_path('test_files/lock_files/conflicting_versions.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_directive_rejected, false) :-
		^^file_path('test_files/lock_files/directive.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_missing_dependency, false) :-
		fixture_lock_terms([lock_a], Terms),
		restore_lock_terms(Terms).

	test(packs_lock_constraints_not_overridden, false) :-
		fixture_lock_terms([lock_impossible, lock_b], Terms),
		restore_lock_terms(Terms, [compatible(false), force(true)]).

	test(packs_lock_dependency_cycle, false) :-
		fixture_lock_terms([lock_cycle_a, lock_cycle_b], Terms),
		restore_lock_terms(Terms).

	test(packs_lock_archive_mismatch, deterministic) :-
		fixture_lock_terms([lock_bad], Terms),
		observe_locked_restore(Terms, [], Outcome, Messages, _),
		Outcome == false,
		lgtunit::assertion(member(pack_archive_checksum_failed(lock_bad, _), Messages)),
		lgtunit::assertion(\+ member(@'Restored setup', Messages)).

	test(packs_lock_final_verification_failure, deterministic) :-
		fixture_lock_terms([lock_a, lock_b], Terms),
		assertz(lock_version_fault),
		catch(
			observe_locked_restore(Terms, [], Outcome, Messages, _),
			Error,
			( 	retractall(lock_version_fault),
				throw(Error)
			)
		),
		retractall(lock_version_fault),
		Outcome == false,
		lgtunit::assertion(member(lock_restore_verification_failed, Messages)),
		lgtunit::assertion(\+ member(@'Restored setup', Messages)).

	test(packs_lock_force_false, deterministic) :-
		fixture_lock_terms([lock_a, lock_b], Terms),
		packs::directory(lock_b, Directory),
		write_fixture_file(Directory, 'obsolete.txt', [obsolete]),
		os::path_concat(Directory, 'obsolete.txt', StaleFile),
		observe_locked_restore(Terms, [force(false)], Outcome, _, Installs),
		^^assertion(Outcome == false),
		^^assertion(Installs == []),
		^^assertion(os::file_exists(StaleFile)),
		os::delete_file(StaleFile),
		packs::installed(lock_fixture, lock_b, 1:0:0).

	test(packs_lock_force_false_without_update, false) :-
		fixture_lock_terms([lock_a, lock_b], Terms),
		restore_lock_terms(Terms, [force(false)]).

	test(packs_lock_restore_pins, deterministic) :-
		fixture_lock_terms([lock_a, lock_b], Terms),
		append(Terms, [pinned_registry(lock_fixture), pinned_pack(lock_a)], PinnedTerms),
		restore_lock_terms(PinnedTerms, [clean(true)]),
		registries::pinned(lock_fixture),
		packs::pinned(lock_a).

	test(packs_lock_save_checksum_required, error(consistency_error(compatible_options, lock(true), checksum(false)))) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		packs::save(Setup, [lock(true), checksum(false)]).

	test(packs_lock_checksum_first_occurrence, deterministic) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		packs::save(Setup, [lock(true), checksum(true), checksum(false)]).

	test(packs_lock_duplicate_commit, false) :-
		^^file_path('test_files/lock_files/conflicting_commits.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	test(packs_lock_sha256_commit_validated, false) :-
		^^file_path('test_files/lock_files/sha256_commit.txt', Setup),
		packs::restore(Setup, [lock(true)]).

	% broken registry and pack specs

	test(packs_registries_add_1_02, deterministic) :-
		^^file_url('test_files/broken_d', URL),
		registries::add(URL).

	test(packs_registries_lint_1_02, false) :-
		registries::lint(broken_d).

	test(packs_packs_lint_1_02, false) :-
		packs::lint(broken).

	% uninstall packs

	test(packs_packs_uninstall_0_01, deterministic) :-
		packs::uninstall.

	test(packs_packs_uninstall_0_02, false) :-
		packs::installed(_, _, _, _).

	% delete registries

	test(packs_registries_unpin_0_03, deterministic) :-
		registries::unpin.

	test(packs_registries_delete_1_01, deterministic) :-
		(   registries::defined(local_1_d, _, _, _) ->
			registries::delete(local_1_d)
		;   true
		).

	% capture messages and installation events, inject verification faults,
	% and suppress packs tool output during tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(Message, _, packs, _) :-
		capture_lock_messages,
		assertz(lock_message_event(Message)),
		fail.
	logtalk::message_hook(pack_installed(lock_fixture, lock_a, _), comment, packs, _) :-
		retract(lock_version_fault),
		packs::directory(lock_a, Directory),
		write_fixture_file(Directory, 'VERSION.packs', [9:0:0]),
		fail.
	logtalk::message_hook(pack_installed(Registry, Pack, Version), comment, packs, _) :-
		capture_lock_installs,
		assertz(lock_install_event(Registry, Pack, Version)).
	logtalk::message_hook(_Message, _Kind, packs, _Tokens).

	% auxiliary predicates

	restore_lock_terms(Terms) :-
		restore_lock_terms(Terms, []).

	restore_lock_terms(Terms, Options) :-
		write_lock_setup(Terms, Setup),
		packs::restore(Setup, [lock(true)| Options]).

	write_lock_setup(Terms, Setup) :-
		^^file_path('test_files/setup_lock.txt', Setup),
		open(Setup, write, Stream),
		write_lock_terms(Terms, Stream),
		close(Stream).

	capture_locked_restore(Terms, Options, Installs) :-
		observe_locked_restore(Terms, Options, Outcome, _, Installs),
		Outcome == true.

	observe_locked_restore(Terms, Options, Outcome, Messages, Installs) :-
		write_lock_setup(Terms, Setup),
		observe_lock_file(Setup, Options, Outcome, Messages, Installs).

	observe_lock_file(Setup, Options, Outcome, Messages, Installs) :-
		retractall(lock_install_event(_, _, _)),
		retractall(lock_message_event(_)),
		assertz(capture_lock_installs),
		assertz(capture_lock_messages),
		catch(
			( 	packs::restore(Setup, [lock(true)| Options]) ->
				Outcome = true
			; 	Outcome = false
			),
			Error,
			( 	retractall(capture_lock_installs),
				retractall(capture_lock_messages),
				throw(Error)
			)
		),
		retractall(capture_lock_installs),
		retractall(capture_lock_messages),
		findall(Message, lock_message_event(Message), Messages),
		findall(Pack-Version, lock_install_event(_, Pack, Version), Installs).

	% when re-running tests, pack files are already loaded
	fixture_loaded_state(Pack, Loaded) :-
		packs::directory(Pack, Directory),
		os::path_concat(Directory, 'loader.lgt', Loader),
		( 	logtalk::loaded_file(Loader) ->
			Loaded = true
		; 	Loaded = false
		).

	unpack_lock_fixture :-
		^^file_path('test_files/logtalk_packs', Destination),
		os::make_directory_path(Destination),
		^^file_path('test_files/lock_fixture.zip', Archive),
		unzip_archive(Archive, Destination).

	fixture_pack_digest(lock_bad, '0000000000000000000000000000000000000000000000000000000000000000') :- !.
	fixture_pack_digest(_, '27ddfdb1bfd6efd86f4c1627bd7409ff0f9092551193007ca0d576c1f49fa959').

	fixture_lock_terms(Packs, Terms) :-
		^^file_path('test_files/logtalk_packs/lock_fixture', Directory),
		^^file_url('test_files/logtalk_packs/lock_fixture', URL),
		git::commit_hash(Directory, Commit),
		findall(pack(lock_fixture, Pack, 1:0:0), member(Pack, Packs), PackTerms),
		findall(
			lock_integrity(lock_fixture, Pack, 1:0:0, sha256, Digest),
			( 	member(Pack, Packs),
				fixture_pack_digest(Pack, Digest)
			),
			Integrities
		),
		append([lockfile_version(1), registry(lock_fixture, URL), lock_registry_commit(lock_fixture, Commit)| PackTerms], Integrities, Terms).

	advance_lock_fixture :-
		^^file_path('test_files/logtalk_packs/lock_fixture', Directory),
		write_fixture_file(Directory, 'advanced.txt', [advanced]),
		commit_lock_fixture(Directory).

	commit_lock_fixture(Directory) :-
		os::internal_os_path(Directory, OSDirectory),
		atomic_list_concat(['git -C "', OSDirectory, '" add . && git -C "', OSDirectory, '" -c user.name=Logtalk -c user.email=tests@logtalk.org -c commit.gpgsign=false commit -q -m fixture'], Command),
		os::shell(Command).

	fixture_git_command(Directory, Arguments) :-
		os::internal_os_path(Directory, OSDirectory),
		atomic_list_concat(['git -C "', OSDirectory, '" ', Arguments], Command),
		os::shell(Command).

	write_fixture_file(Directory, Basename, Terms) :-
		os::path_concat(Directory, Basename, File),
		open(File, write, Stream),
		write_lock_terms(Terms, Stream),
		close(Stream).

	read_fixture_bytes(File, Bytes) :-
		open(File, read, Stream, [type(binary)]),
		read_fixture_byte_stream(Stream, Bytes),
		close(Stream).

	read_fixture_byte_stream(Stream, Bytes) :-
		get_byte(Stream, Byte),
		( 	Byte =:= -1 ->
			Bytes = []
		; 	Bytes = [Byte| Rest],
			read_fixture_byte_stream(Stream, Rest)
		).

	read_fixture_terms(File, Terms) :-
		open(File, read, Stream),
		read_fixture_stream(Stream, Terms),
		close(Stream).

	read_fixture_stream(Stream, Terms) :-
		read(Stream, Term),
		( 	Term == end_of_file ->
			Terms = []
		; 	Terms = [Term| Rest],
			read_fixture_stream(Stream, Rest)
		).

	write_lock_terms([], _).
	write_lock_terms([Term| Terms], Stream) :-
		writeq(Stream, Term),
		write(Stream, '.\n'),
		write_lock_terms(Terms, Stream).

	first_repo_commit('95d1f1c90e86f04c0682a2c6fe33e1823eb7bea2').

	:- if(os::operating_system_type(windows)).

		unzip_archive(Archive, Destination) :-
			atomic_list_concat(['tar --no-same-owner -xf "', Archive, '" --directory "', Destination, '"'], Command),
			os::shell(Command).

	:- else.

		unzip_archive(Archive, Destination) :-
			atomic_list_concat(['bsdtar --no-same-owner -xf "', Archive, '" --directory "', Destination, '"'], Command),
			os::shell(Command).

	:- endif.

:- end_object.
