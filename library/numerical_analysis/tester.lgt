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
	logtalk_load(options(loader)),
	logtalk_load(linear_algebra(loader)),
	logtalk_load(lgtunit(loader)),
	logtalk_load([
		univariate_function_protocol,
		root_finder_protocol,
		root_finder,
		bisection_root_finder,
		brent_root_finder,
		secant_root_finder,
		newton_root_finder,
		quadrature_protocol,
		quadrature,
		adaptive_simpson_quadrature,
		gauss_legendre_quadrature,
		interpolator_protocol,
		interpolator,
		piecewise_linear_interpolator,
		barycentric_interpolator,
		cubic_spline_interpolator,
		ode_system_protocol,
		ode_solver_protocol,
		ode_solver,
		euler_ode_solver,
		rk4_ode_solver,
		rk45_ode_solver,
		test_functions,
		test_ode_systems
	], [
		debug(on),
		source_data(on)
	]),
	logtalk_load([
		root_finding_tests,
		quadrature_tests,
		interpolation_tests,
		ode_solver_tests
	], [
		hook(lgtunit)
	]),
	lgtunit::run_test_sets([
		root_finding_tests,
		quadrature_tests,
		interpolation_tests,
		ode_solver_tests
	])
)).
