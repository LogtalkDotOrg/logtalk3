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


:- object(smt_trivial_sat,
	implements(smt_problem_protocol)).

	set_logic('QF_BV').
	declare_const(x, bv(8)).
	assert(bvugt(x, bv(0, 8))).
	check_sat.
	get_model.

:- end_object.


:- object(smt_trivial_unsat,
	implements(smt_problem_protocol)).

	set_logic('QF_BV').
	declare_const(x, bv(8)).
	assert(bvugt(x, bv(0, 8))).
	assert(bvult(x, bv(0, 8))).
	check_sat.

:- end_object.


:- object(smt_unsat_core_fixture,
	implements(smt_problem_protocol)).

	% Neither assertion is unsatisfiable alone; together they are, so a minimal
	% unsat core must include both names.
	set_logic('QF_BV').
	declare_const(x, bv(8)).
	assert_named(a1, bvugt(x, bv(5, 8))).
	assert_named(a2, bvult(x, bv(3, 8))).
	check_sat.
	get_unsat_core.

:- end_object.


:- object(smt_push_pop_fixture,
	implements(smt_problem_protocol)).

	set_logic('QF_BV').
	declare_const(x, bv(8)).
	assert(bvugt(x, bv(0, 8))).
	check_sat.
	push(1).
	assert(bvult(x, bv(0, 8))).
	check_sat.
	pop(1).
	check_sat.

:- end_object.


:- object(smt_raw_fixture,
	implements(smt_problem_protocol)).

	:- raw('; trusted raw comment').
	:- raw([59,32,116,114,117,115,116,101,100,32,114,97,119,32,99,111,109,109,101,110,116]).
	:- raw([';',' ',t,r,u,s,t,e,d,' ',r,a,w,' ',c,o,m,m,e,n,t]).
	:- raw("; trusted raw comment").

:- end_object.


:- object(smt_comment_fixture,
	implements(smt_problem_protocol)).

	:- comment('a readable comment').
	set_logic('QF_LIA').

:- end_object.


:- object(smt_unrelated_fixture).

	:- public(value/1).
	value(ordinary).

:- end_object.
