%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-06-08,
		comment is 'Unit tests for the "scopes" example.'
	]).

	cover(prototype).
	cover(descendant).

	test(scopes_01, true(Foo == 1)) :-
		prototype::foo(Foo).

	test(scopes_02, error(permission_error(access,protected_predicate,bar/1))) :-
		prototype::bar(_).

	test(scopes_03, error(permission_error(access,private_predicate,baz/1))) :-
		prototype::baz(_).

	test(scopes_04, error(existence_error(predicate_declaration,(local)/1))) :-
		prototype::local(_).

	test(scopes_05, true(Foo == 2)) :-
		descendant::p_foo(Foo).

	test(scopes_06, true(Bar == 2)) :-
		descendant::p_bar(Bar).

	test(scopes_07, true(Baz == 2)) :-
		descendant::p_baz(Baz).

	test(scopes_08, true(Foo == 1)) :-
		descendant::d_foo(Foo).

	test(scopes_09, true(Bar == 1)) :-
		descendant::d_bar(Bar).

:- end_object.
