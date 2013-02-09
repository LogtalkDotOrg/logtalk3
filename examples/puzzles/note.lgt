%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* Passing A Note logical puzzle by Sally Quinn

Jack has a major crush on Jill. During study hall, he finally gathers all of his courage and writes her a note asking her out on Saturday night. The note passes to five students (who all read it) before it gets to Jill. Just as Jill gets the note, Mrs. Wilson the teacher confiscates it. After reading the note, she wants to know all who were involved in the note passing incident. She questions her students, and receives the following responses.

The girl studying English passed it to Paul who passed it to the girl in green.
Josephine passed it to the boy in blue who gave it to Alexis who was reading.
The girl in black gave it to Jill.
Jack first gave it to Mary who was studying English.
The girl in black who was reading got the note from Derrick.

Mrs. Wilson remembers the following facts from study hall.

Paul was wearing yellow.
Derrick was studying French.
The girl in green was studying science.

From the information given by the students, can you determine what color each culprit was wearing, the subject they were studying and the order that they received the note? Use the chart below to help you.

Published on the web:
	http://www.norfacad.pvt.k12.va.us/puzzles/note.htm
*/


:- object(note).

	:- info([
		version is 1.0,
		date is 2004/5/1,
		author is 'Paulo Moura',
		comment is 'Passing a note logical puzzle']).

	:- public(students/1).
	:- mode(students(-list), one).
	:- info(students/1, [
		comment is 'Solution to the puzzle.',
		argnames is ['Solution']
	]).

	:- public(print/1).
	:- mode(print(+list), one).
	:- info(print/1, [
		comment is 'Pretty print solution to the puzzle.',
		argnames is ['Solution']
	]).

	students(Solution) :-
		template(Solution),
		next(s(Girl1, english, _, _), s(paul, _, _, _), Solution), girl(Girl1),
		next(s(paul, _, _, _), s(Girl2, _, green, _), Solution), girl(Girl2),
		next(s(josephine, _, _, _), s(Boy1, _, blue, _), Solution), boy(Boy1),
		next(s(Boy1, _, blue, _), s(alexis, reading, _, _), Solution),
		member(s(Girl3, _, black, 5), Solution), girl(Girl3),
		member(s(mary, english, _, 1), Solution),
		next(s(derrick, _, _, _), s(Girl4, reading, black, _), Solution), girl(Girl4),
		member(s(paul, _, yellow, _), Solution),
		member(s(derrick, french, _, _), Solution),
		member(s(Girl5, science, green, _), Solution), girl(Girl5),
		member(s(_, math, _, _), Solution),
		member(s(_, _, red, _), Solution).

	girl(alexis).
	girl(josephine).
	girl(mary).

	boy(derrick).
	boy(paul).

	print([]).
	print([Student| Students]) :-
		write(Student), nl,
		print(Students).	

	% h(Name, Subject, Color, Place)
	template([s(_, _, _, 1), s(_, _, _, 2), s(_, _, _, 3), s(_, _, _, 4), s(_, _, _, 5)]).

	member(A, [A, _, _, _, _]).
	member(B, [_, B, _, _, _]).
	member(C, [_, _, C, _, _]).
	member(D, [_, _, _, D, _]).
	member(E, [_, _, _, _, E]).

	next(A, B, [A, B, _, _, _]).
	next(B, C, [_, B, C, _, _]).
	next(C, D, [_, _, C, D, _]).
	next(D, E, [_, _, _, D, E]).

:- end_object.
