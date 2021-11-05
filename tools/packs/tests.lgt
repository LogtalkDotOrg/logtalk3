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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:7:0,
		author is 'Paulo Moura',
		date is 2021-11-05,
		comment is 'Unit tests for the "packs" tool.'
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	cover(packs_common).
	cover(registries).
	cover(packs).
	cover(registry_loader_hook).
	cover(packs_specs_hook).

	setup :-
		packs::uninstall,
		packs::clean,
		registries::delete,
		registries::clean.

	cleanup :-
		setup.

	% we start with no defined registries or installed packs

	test(packs_registries_logtalk_packs_1_01, true(LogtalkPacks == Storage)) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'test_files/logtalk_packs/', Storage),
		registries::logtalk_packs(LogtalkPacks).

	test(packs_packs_logtalk_packs_1_01, true(LogtalkPacks == Storage)) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'test_files/logtalk_packs/', Storage),
		packs::logtalk_packs(LogtalkPacks).

	test(packs_registries_logtalk_packs_0_01, true) :-
		registries::logtalk_packs.

	test(packs_packs_logtalk_packs_0_02, true) :-
		packs::logtalk_packs.

	test(packs_registries_directory_1_01, true(atom(Directory))) :-
		^^suppress_text_output,
		registries::directory(Directory).

	test(packs_packs_directory_1_01, true(atom(Directory))) :-
		^^suppress_text_output,
		packs::directory(Directory).

	test(packs_registries_list_0_01, true) :-
		^^suppress_text_output,
		registries::list.

	test(packs_registries_defined_3_01, false) :-
		^^suppress_text_output,
		registries::defined(_, _, _).

	test(packs_packs_available_0_01, true) :-
		^^suppress_text_output,
		packs::available.

	test(packs_packs_installed_0_01, true) :-
		^^suppress_text_output,
		packs::installed.

	test(packs_packs_installed_4_01, false) :-
		^^suppress_text_output,
		packs::installed(_, _, _, _).

	test(packs_packs_installed_3_01, false) :-
		^^suppress_text_output,
		packs::installed(_, _, _, _).

	test(packs_packs_outdated_0_01, true) :-
		^^suppress_text_output,
		packs::outdated.

	test(packs_packs_outdated_4_01, false) :-
		^^suppress_text_output,
		packs::outdated(_, _, _, _).

	test(packs_packs_orphaned_0_01, true) :-
		^^suppress_text_output,
		packs::orphaned.

	test(packs_packs_orphaned_2_01, false) :-
		^^suppress_text_output,
		packs::orphaned(_, _).

	test(packs_registries_clean_0_01, true) :-
		^^suppress_text_output,
		registries::clean.

	test(packs_packs_clean_0_01, true) :-
		^^suppress_text_output,
		packs::clean.

	test(packs_registries_lint_0_01, true) :-
		^^suppress_text_output,
		registries::lint.

	test(packs_packs_lint_0_01, true) :-
		^^suppress_text_output,
		packs::lint.

	test(packs_registries_update_0_01, true) :-
		^^suppress_text_output,
		registries::update.

	test(packs_packs_update_0_01, true) :-
		^^suppress_text_output,
		packs::update.

	test(packs_registries_help_0_01, true) :-
		^^suppress_text_output,
		registries::help.

	test(packs_packs_help_0_01, true) :-
		^^suppress_text_output,
		packs::help.

	% now we add a local registry

	test(packs_registries_add_1_01, true) :-
		^^suppress_text_output,
		this(This),
		object_property(This, file(_, Directory)),
		atomic_list_concat(['file://', Directory, 'test_files/local_1'], URL),
		registries::add(URL).

	test(packs_registries_lint_1_01, true) :-
		^^suppress_text_output,
		registries::lint(local_1).

	test(packs_registries_describe_1_01, true) :-
		^^suppress_text_output,
		registries::describe(local_1).

	test(packs_registries_directory_2_01, true(atom(Directory))) :-
		^^suppress_text_output,
		registries::directory(local_1, Directory).

	test(packs_registries_readme_2_01, true(Readme == File)) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'test_files/logtalk_packs/registries/local_1/README.md', File),
		registries::readme(local_1, Readme).

	test(packs_registries_readme_1_01, true) :-
		^^suppress_text_output,
		registries::readme(local_1).

	test(packs_registries_provides_2_01, true(Pairs == [local_1-foo])) :-
		^^suppress_text_output,
		findall(Registry-Pack, registries::provides(Registry, Pack), Pairs).

	test(packs_registries_update_1_01, true) :-
		^^suppress_text_output,
		registries::update(local_1).

	test(packs_registries_clean_1_01, true) :-
		^^suppress_text_output,
		registries::clean(local_1).

	test(packs_registries_pin_1_01, true) :-
		^^suppress_text_output,
		registries::pin(local_1).

	test(packs_registries_pin_1_02, true) :-
		^^suppress_text_output,
		registries::pin(local_1),
		registries::pin(local_1).

	test(packs_registries_unpin_1_01, true) :-
		^^suppress_text_output,
		registries::unpin(local_1).

	test(packs_registries_unpin_1_02, true) :-
		^^suppress_text_output,
		registries::unpin(local_1),
		registries::unpin(local_1).

	test(packs_registries_pinned_1_01, true) :-
		^^suppress_text_output,
		registries::pin(local_1),
		registries::pinned(local_1).

	test(packs_registries_pinned_1_02, false) :-
		^^suppress_text_output,
		registries::unpin(local_1),
		registries::pinned(local_1).

	test(packs_packs_lint_1_01, true) :-
		^^suppress_text_output,
		packs::lint(foo).

	test(packs_packs_available_1_01, true) :-
		^^suppress_text_output,
		packs::available(local_1).

	test(packs_packs_describe_1_01, true) :-
		^^suppress_text_output,
		packs::describe(foo).

	test(packs_packs_dependents_3_01, true(Dependents == [])) :-
		^^suppress_text_output,
		packs::dependents(local_1, foo, Dependents).

	test(packs_packs_dependents_2_01, true) :-
		^^suppress_text_output,
		packs::dependents(local_1, foo).

	test(packs_packs_dependents_1_01, true) :-
		^^suppress_text_output,
		packs::dependents(foo).

:- end_object.
