%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
	logtalk_load(basic_types(loader)),
	logtalk_load(
		[
			warnings,
			main_include_compiler_warning
		],
		[
			redefined_built_ins(warning),
			redefined_operators(warning),
			missing_directives(warning),
			duplicated_directives(warning),
			duplicated_clauses(warning),
			tail_recursive(warning),
			unknown_entities(warning),
			unknown_predicates(warning),
			undefined_predicates(warning),
			trivial_goal_fails(warning),
			always_true_or_false_goals(warning),
			lambda_variables(warning),
			grammar_rules(warning),
			arithmetic_expressions(warning),
			suspicious_calls(warning),
			singleton_variables(warning),
			portability(warning),
			steadfastness(warning),
			naming(warning),
			disjunctions(warning),
			conditionals(warning),
			catchall_catch(warning),
			deprecated(warning),
			report(on)
		]
	)
)).
