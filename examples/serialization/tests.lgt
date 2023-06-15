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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2023-04-10,
		comment is 'Unit tests for the "serialization" example.'
	]).

	cover(serializer).

	test(serialization_01) :-
		create_protocol(abc, [], [public([a/1,b/1,c/1])]),
		create_object(_, [implements(abc)], [], [a(1),b(1),c(1)]),
		create_object(_, [implements(abc)], [], [a(2),b(2),c(2)]),
		create_object(_, [implements(abc)], [], [a(3),b(3),c(3)]),
		serializer::save(abc, abc_objects).

	test(serialization_02) :-
		forall(conforms_to_protocol(Object,abc), abolish_object(Object)),
		serializer::restore(abc_objects),
		conforms_to_protocol(Object1, abc),
		Object1::a(A1), A1 == 1,
		Object1::b(B1), B1 == 1,
		Object1::c(C1), C1 == 1,
		conforms_to_protocol(Object2, abc),
		Object2::a(A2), A2 == 2,
		Object2::b(B2), B2 == 2,
		Object2::c(C2), C2 == 2,
		conforms_to_protocol(Object3, abc),
		Object3::a(A3), A3 == 3,
		Object3::b(B3), B3 == 3,
		Object3::c(C3), C3 == 3.

	cleanup :-
		^^clean_file(abc_objects).

:- end_object.
