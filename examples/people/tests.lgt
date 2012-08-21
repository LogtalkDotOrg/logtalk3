%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2012/07/03,
		comment is 'Unit tests for the "people" example.']).

	unit(person).
	unit(teacher).
	unit(student).
	unit(person(_, _)).
	unit(teacher(_, _, _)).
	unit(student(_, _, _)).

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
