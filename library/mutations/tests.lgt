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
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2023-11-23,
		comment is 'Unit tests for the "mutations" library.'
	]).

	cover(mutations_store).

	test(mutations_type_checking, true) :-
		forall(
			mutations_store::counter(Type, _),
			(	type::arbitrary(Type, Arbitrary),
				type::mutation(Type, Arbitrary, Mutation),
				^^assertion(type::valid(Type, Mutation))
			)
		).

	quick_check(
		mutations_user_defined,
		type::mutation({pair(atom,integer)}, +pair(atom,integer), -pair(atom,integer)),
		[ec(false), n(25)]
	).

:- end_object.
