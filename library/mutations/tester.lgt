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


:- initialization((
	set_logtalk_flag(report, warnings),
	logtalk_load(types(loader)),
	logtalk_load(arbitrary(loader)),
	logtalk_load(random(loader)),
	logtalk_load([
		mutations_store,
		mutations
	], [
		debug(on), source_data(on)
	]),
	logtalk_load([
		default_atom_mutations,
		default_integer_mutations,
		default_float_mutations,
		default_list_mutations,
		default_compound_mutations
	], [
		optimize(on), hook(mutations_store)
	]),
	logtalk_load(lgtunit(loader)),
	logtalk_load(test_mutations, [hook(mutations_store)]),
	logtalk_load(tests, [hook(lgtunit)]),
	tests::run
)).
