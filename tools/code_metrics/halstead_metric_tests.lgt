%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(halstead_metric_tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:1,
		author is 'Paulo Moura',
		date is 2020-04-13,
		comment is 'Unit tests for the Halstead complexity metric.'
	]).

	:- uses(halstead_metric, [entity_score/2]).

	cover(code_metric).
	cover(halstead_metric).

	% basic validity tests

	test(halstead_expanding) :-
		halstead_metric_valid(expanding).

	test(halstead_forwarding) :-
		halstead_metric_valid(forwarding).

	test(halstead_monitoring) :-
		halstead_metric_valid(monitoring).

	test(halstead_logtalk) :-
		halstead_metric_valid(logtalk).

	test(halstead_user) :-
		halstead_metric_valid(user).

	% metric math tests

	test(halstead_h_ptc) :-
		entity_score(h_ptc, pn_pan_cn_can_ev_el_v_d_e_t_b(Pn,PAn,Cn,CAn,_,_,_,_,_,_,_)),
		Pn == 2, PAn == 3,
		Cn == 0, CAn == 0.

	test(halstead_h_ctg) :-
		entity_score(h_ctg, pn_pan_cn_can_ev_el_v_d_e_t_b(Pn,PAn,Cn,CAn,_,_,_,_,_,_,_)),
		Pn == 2, PAn == 2,
		Cn == 5, CAn == 5.

	test(halstead_h_obj) :-
		entity_score(h_obj, pn_pan_cn_can_ev_el_v_d_e_t_b(Pn,PAn,Cn,CAn,_,_,_,_,_,_,_)),
		Pn == 4, PAn == 4,
		Cn == 4, CAn == 4.

	% auxiliary predicates

	halstead_metric_valid(Entity) :-
		entity_score(Entity, pn_pan_cn_can_ev_el_v_d_e_t_b(Pn,PAn,Cn,CAn,EV,EL,V,D,E,T,B)),
		integer(Pn), Pn >= 0,
		integer(PAn), PAn >= 0,
		integer(Cn), Cn >= 0,
		integer(CAn), CAn >= 0,
		integer(EV), EV >= 0, EV is Pn + PAn,
		integer(EL), EL >= 0, EL is Cn + CAn,
		number(V), V >= 0,
		number(D), D >= 0,
		number(E), E >= 0,
		integer(T), T >= 0,
		number(B), B >= 0.

:- end_object.
