%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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
		version is 1.1,
		author is 'Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "people" example.'
	]).

	cover(person).
	cover(teacher).
	cover(student).
	cover(person(_, _)).
	cover(teacher(_, _, _)).
	cover(student(_, _, _)).

	test(people_1) :-
		person::new(Id1, 'Oscar the Grouch', '1969/11/10'),
		Id1::name(Name),
		Name == 'Oscar the Grouch',
		Id1::birth(Birth),
		Birth == '1969/11/10'.

	test(people_2) :-
		person::new(Id2, 'Cookie Monster', '1969/12/02'),
		Id2::name(Name),
		Name == 'Cookie Monster',
		Id2::birth(Birth),
		Birth == '1969/12/02'.

	test(people_3) :-
		teacher::new(Id3, 'Gordon Robinson', '1969/11/10', '3.2'),
		Id3::name(Name),
		Name == 'Gordon Robinson',
		Id3::birth(Birth),
		Birth == '1969/11/10',
		Id3::office(Office),
		Office == '3.2'.

	test(people_4) :-
		student::new(Id4, 'Roosevelt Franklin', '1969/11/10', 'Blue'),
		Id4::name(Name),
		Name == 'Roosevelt Franklin',
		Id4::birth(Birth),
		Birth == '1969/11/10',
		Id4::dorm(Dorm),
		Dorm == 'Blue'.

	test(people_5) :-
		{student('Roosevelt Franklin', Birth, Dorm)}::true,
		Birth == '1969/11/10',
		Dorm == 'Blue'.

:- end_object.
