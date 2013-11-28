%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.1,
		author is 'Parker Jones and Paulo Moura',
		date is 2012/05/13,
		comment is 'Unit tests for the "engines" example.'
	]).

	cover(enginep).
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
