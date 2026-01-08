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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2023-06-04,
		comment is 'Unit tests for the "delegates" example.'
	]).

	cover(delegator).
	cover(a_delegator).
	cover(a_delegator(_)).
	cover(a_delegate).
	cover(an_object).

	% without a delegate:
	test(delegates_1, true(String == 'default implementation')) :-
		a_delegator::operation(String).

	% with a delegate that does not implement thing/1:
	test(delegates_2, true(String == 'default implementation')) :-
		a_delegator::set_delegate(an_object),
		a_delegator::operation(String).

	% with a delegate that implements thing/1:
	test(delegates_3, true(String == 'delegate implementation')) :-
		a_delegator::set_delegate(a_delegate),
		a_delegator::operation(String).

	% same tests but using the parametric object implementation:
	test(delegates_4, true(String == 'default implementation')) :-
		a_delegator(an_object)::operation(String).

	test(delegates_5, true(String == 'delegate implementation')) :-
		a_delegator(a_delegate)::operation(String).

:- end_object.
