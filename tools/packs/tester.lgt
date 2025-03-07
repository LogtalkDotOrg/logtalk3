%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- initialization((
	logtalk_load_context(directory, Directory),
	atom_concat(Directory, 'test_files/logtalk_packs/', LogtalkPacks),
	retractall(logtalk_library_path(logtalk_packs, _)),
	assertz(logtalk_library_path(logtalk_packs, LogtalkPacks)),
	set_logtalk_flag(report, warnings),
	logtalk_load(basic_types(loader)),
	logtalk_load(os(loader)),
	logtalk_load(options(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		safety_hooks,
		pack_protocol,
		registry_protocol,
		packs_messages,
		packs_common,
		registries,
		packs
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
