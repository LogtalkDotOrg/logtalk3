%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.2,
		author is 'Parker Jones and Paulo Moura',
		date is 2016/05/31,
		comment is 'Unit tests for the "carengines" example.'
	]).

	cover(carenginep).
	cover(classic).
	cover(sport).
	cover(sedan).
	cover(coupe).

	test(engines_1) :-
		findall(Predicate, sedan::current_predicate(Predicate), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1].

	test(engines_2) :-
		findall(Predicate, coupe::current_predicate(Predicate), Solutions),
		list::msort(Solutions, SolutionsSorted),
		SolutionsSorted == [bore_stroke/2, capacity/1, cylinders/1, fuel/1, horsepower_rpm/2, reference/1].

	test(engines_3) :-
		findall(Name-Cylinders-HP-RPM, sedan::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		Solutions == ['M180.940'-6-94-4800].

	test(engines_4) :-
		findall(Name-Cylinders-HP-RPM, coupe::(reference(Name), cylinders(Cylinders), horsepower_rpm(HP, RPM)), Solutions),
		Solutions == ['M180.941'-6-115-3657].

:- end_object.
