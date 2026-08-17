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
	logtalk_load(basic_types(loader)),
	logtalk_load(linear_algebra(loader)),
	logtalk_load(options(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		local_optimization_problem_protocol,
		local_optimization_solver,
		bfgs,
		conjugate_gradient,
		gradient_descent,
		nelder_mead,
		test_objects
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load([tests, gradient_tests, bfgs_tests], [hook(lgtunit)]),
	lgtunit::run_test_sets([
		% derivative-free solvers (shared tests/1 suite)
		tests(nelder_mead),
		% gradient-based solvers (shared tests/1 + gradient_tests/1)
		tests(gradient_descent),
		gradient_tests(gradient_descent),
		tests(conjugate_gradient),
		gradient_tests(conjugate_gradient),
		tests(bfgs),
		gradient_tests(bfgs),
		bfgs_tests
	])
)).
