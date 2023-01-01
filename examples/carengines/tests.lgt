%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2023 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:4:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-26,
		comment is 'Unit tests for the "carengines" example.'
	]).

	cover(classic).
	cover(sport).
	cover(sedan).
	cover(coupe).

	test(carengines_01) :-
		findall(Predicate, sedan::current_predicate(Predicate), Solutions),
		list::msort(Solutions, SolutionsSorted),
		^^assertion(SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1]).

	test(carengines_02) :-
		findall(Predicate, coupe::current_predicate(Predicate), Solutions),
		list::msort(Solutions, SolutionsSorted),
		^^assertion(SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1]).

	test(carengines_03) :-
		findall(Name-Cylinders-HP-RPM, sedan::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		^^assertion(Solutions == ['M180.940'-6-94-4800]).

	test(carengines_04) :-
		findall(Name-Cylinders-HP-RPM, coupe::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		^^assertion(Solutions == ['M180.941'-6-115-3657]).

:- end_object.
