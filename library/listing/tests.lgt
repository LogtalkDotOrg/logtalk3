%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2024 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2024-01-25,
		comment is 'Unit tests for the "listing" library.'
	]).

	:- set_logtalk_flag(unknown_entities, silent).

	cover(listing).

	setup :-
		create_object(
			listing_test_object,
			[imports(listing)],
			[protected(a/1), dynamic(a/1), private(b/1), dynamic(b/1)],
			[a(1), a(2), a(3), b(4), b(5)]
		).

	test(listing_0, true(Assertion)) :-
		^^set_text_output(''),
		listing_test_object::listing,
		^^text_output_assertion('a(1).\na(2).\na(3).\n\nb(4).\nb(5).\n\n', Assertion).

	test(listing_1, true(Assertion)) :-
		^^set_text_output(''),
		listing_test_object::listing(a/1),
		^^text_output_assertion('a(1).\na(2).\na(3).\n\n', Assertion).

	cleanup :-
		abolish_object(listing_test_object).

:- end_object.
