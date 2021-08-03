%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 2:4:0,
		author is 'Paulo Moura',
		date is 2021-08-03,
		comment is 'Unit tests for the "securemp" example.'
	]).

	:- discontiguous(fails/1).

	fails(rule_a) :-
		% compile-time error
		logtalk_load(rule_a).

	succeeds(rule_a_variant) :-
		^^suppress_text_output,
		% runtime error
		logtalk_load(rule_a_variant),
		catch({client_a_variant::double([1,2,3], _)}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == scale/3 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_a_variant, Type == object, DPI == scale/3
		).

	succeeds(rule_b_1) :-
		^^suppress_text_output,
		logtalk_load(rule_b_1).

	succeeds(rule_b_2) :-
		^^suppress_text_output,
		% runtime error
		logtalk_load(rule_b_2),
		catch({client_b_2::test}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == term/0 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_b_2, Type == object, DPI == term/0
		).

	succeeds(rule_b_3) :-
		^^suppress_text_output,
		% runtime error
		logtalk_load(rule_b_3),
		catch({client_b_3::test(_)}, error(existence_error(procedure,PI), _), true),
		ground(PI),
		(	PI == a/2 ->
			true
		;	% the client_a_variant::double/2 message may be optimized
			logtalk::decompile_predicate_indicators(PI, Entity, Type, DPI),
			Entity == library_b_3, Type == object, DPI == a/2
		).

	succeeds(rule_b_3_variant) :-
		% suspicious meta-predicate definition but no error
		logtalk_load(rule_b_3_variant),
		{client_b_3_variant::test(X)},
		X == 3.

	fails(rule_c) :-
		% compile-time error
		logtalk_load(rule_c).

	% suppress printing of compiler errors for the first two tests

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	% for "rule_a" test
	logtalk::message_hook(compiler_error(_,_,error(type_error(variable,scale(_)),clause(_))), error, core, _Tokens).
	% for "rule_c" test
	logtalk::message_hook(compiler_error(_,_,error(domain_error({1},2),clause(_))), error, core, _Tokens).

:- end_object.
