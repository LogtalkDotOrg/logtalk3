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
	logtalk_load(os(loader)),
	logtalk_load([
		backend_adapter_hook,
		default_workflow_hook,
		identity_hook,
		grammar_rules_hook,
		object_wrapper_hook,
		write_to_stream_hook,
		write_to_file_hook,
		print_goal_hook,
		suppress_goal_hook
	], [
		optimize(on)
	])
)).

:- if(current_logtalk_flag(modules, supported)).

	:- initialization((
		logtalk_load([
			prolog_module_hook
		], [
			optimize(on)
		])
	)).

:- endif.
