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
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2021-11-17,
		comment is 'Unit tests for the "dif" library.'
	]).

	:- uses(dif, [
		dif/2, dif/1
	]).

	cover(dif).

	% dif/2 tests

	test(dif_dif_2_01, false) :-
		dif(X, X).

	test(dif_dif_2_02, true) :-
		dif(X, Y), X = 1, Y = 2.

	test(dif_dif_2_03, false) :-
		dif(X, Y), X = 1, Y = 1.

	% dif/1 tests

	test(dif_dif_1_01, false) :-
		dif([X, X, X]).

	test(dif_dif_1_02, true) :-
		dif([X, Y, Z]), X = 1, Y = 2, Z = 3.

	test(dif_dif_1_03, false) :-
		dif([X, Y, Z]), X = 1, Y = 2, Z = 1.

:- end_object.
