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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2024-03-15,
		comment is 'Unit tests for the "hooks" example.'
	]).

	test(hooks_01, error(permission_error(access, private_predicate, item/1))) :-
		object::item(_).

	test(hooks_02, true(Items == [alpha, omega, zeta])) :-
		object::items(Items).

	test(hooks_03, true(Author == 'Paulo Moura')) :-
		object_property(object, info(Items)),
		list::memberchk(author(Author), Items).

	test(hooks_04, true(License == 'Apache-2.0')) :-
		object_property(object, info(Items)),
		list::memberchk(license(License), Items).

:- end_object.
