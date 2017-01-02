%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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
		version is 2.3,
		author is 'Paulo Moura',
		date is 2016/10/27,
		comment is 'Unit tests for the "securemp" example.'
	]).

	:- discontiguous(fails/1).

	fails(rule_a) :-
		logtalk_load(rule_a).				% compile-time error

	succeeds(rule_a_variant) :-
		logtalk_load(rule_a_variant),		% runtime error
		catch({client_a_variant::double([1,2,3], _)}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == scale/3 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_a_variant, Type == object, DPI == scale/3
		).

	succeeds(rule_b_1) :-
		logtalk_load(rule_b_1).

	succeeds(rule_b_2) :-
		logtalk_load(rule_b_2),				% runtime error
		catch({client_b_2::test}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == term/0 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_b_2, Type == object, DPI == term/0
		).

	succeeds(rule_b_3) :-
		logtalk_load(rule_b_3),				% runtime error
		catch({client_b_3::test(_)}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == a/2 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_b_3, Type == object, DPI == a/2
		).

	succeeds(rule_b_3_variant) :-
		logtalk_load(rule_b_3_variant),		% suspicious meta-predicate
		{client_b_3_variant::test(X)},		% definition but no error
		X == 3.

	fails(rule_c) :-
		logtalk_load(rule_c).				% compile-time error

:- end_object.
