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


:- object(sat_trivial_sat,
	implements(sat_problem_protocol)).

	clause([p]).
	clause([neg(q), p]).

:- end_object.


:- object(sat_pigeonhole_unsat,
	implements(sat_problem_protocol)).

	% Two pigeons, one hole: both must occupy it, but not simultaneously.
	clause([in(1)]).
	clause([in(2)]).
	clause([neg(in(1)), neg(in(2))]).

:- end_object.


:- object(sat_empty_clause_fixture,
	implements(sat_problem_protocol)).

	clause([p]).
	clause([]).

:- end_object.


:- object(sat_raw_fixture,
	implements(sat_problem_protocol)).

	:- cnf_raw('c trusted raw comment').
	:- cnf_raw([99,32,116,114,117,115,116,101,100,32,114,97,119,32,99,111,109,109,101,110,116]).
	:- cnf_raw([c,' ',t,r,u,s,t,e,d,' ',r,a,w,' ',c,o,m,m,e,n,t]).
	:- cnf_raw("c trusted raw comment").

:- end_object.


:- object(sat_comment_fixture,
	implements(sat_problem_protocol)).

	:- comment('a readable comment').
	clause([p]).

:- end_object.


:- object(sat_unrelated_fixture).

	:- public(value/1).
	value(ordinary).

:- end_object.
