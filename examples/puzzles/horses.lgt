%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/* Horse show logical puzzle by Sally Quinn

"You’ll be late for your own funeral." Her mother’s words kept repeating in Alex’s head as she approached the fair grounds-late again! It was a curse, no matter how hard she tried, she was always late. This  time she had missed the first class of the local horse show. Not really  a major crisis, but she had to report the results to the local paper. She  couldn’t let her boss know that she had arrived late. His last words were, "If you are late one more time, you’re fired!". She needed to figure out another way to write her article accurately for the paper. As Alex entered the show grounds, she overheard bits of conversation which she jotted down.

"No horse and rider had the same name" ...
.."Danny rode his white mare..."...
..."  the horse named April finished behind the rider named Sue and ahead of the black mare"..
..." April rode the Chestnut gelding and finished one place ahead of the bay horse"....
.." The Gray horse named Gopher was not first or last  place"....
..."The horse named Doc was first, and the rider named Doc was third"

By looking at a program, Alex discovered that the horses and riders names were Doc, April, Sue, Danny, and Gopher. She knew that April and Sue were the only mares. Does Alex have enough information to fool her boss and write the article accurately?

Published on the web:
	http://www.norfacad.pvt.k12.va.us/puzzles/horse.htm
*/


:- object(horses).

	:- info([
		version is 1.0,
		date is 2004/5/1,
		author is 'Paulo Moura',
		comment is 'Horse show logical puzzle']).

	:- public(horses/1).
	:- mode(horses(-list), one).
	:- info(horses/1, [
		comment is 'Solution to the puzzle.',
		argnames is ['Solution']]).

	:- public(print/1).
	:- mode(print(+list), one).
	:- info(print/1, [
		comment is 'Pretty print solution to the puzzle.',
		argnames is ['Solution']]).

	horses(Solution) :-
		template(Solution),
		member(h(doc, _, _, _, 1), Solution),
		member(h(_, _, _, doc, 3), Solution),
		member(h(april, mare, _, _, _), Solution),
		member(h(sue, mare, _, _, _), Solution),
		member(h(gopher, gelding, _, _, _), Solution),
		member(h(doc, gelding, _, _, _), Solution),
		member(h(danny, gelding, _, _, _), Solution),
		member(h(gopher, _, gray, _, P), Solution), P \= 1, P \= 5,
		member(h(_, mare, white, danny, _), Solution),
		member(h(_, _, _, sue, Pi), Solution), member(h(april, _, _, _, Pj), Solution), Pi < Pj,
		member(h(april, _, _, _, Pj), Solution), member(h(_, mare, black, _, Pk), Solution), Pj < Pk,
		next(h(_, gelding, chestnut, april, _), h(_, _, bay, _, _), Solution),
		member(h(_, _, _, gopher, _), Solution),		
		\+ member(h(N, _, _, N, _), Solution).

	print([]).
	print([Place| Places]) :-
		print_place(Place),
		print(Places).	

	print_place(h(H, S, C, R, P)) :-
		write(P), write(' place: '),
		write(R), write(' riding '), write(H),
		write(', the '), write(C), write(' '), write(S), nl.

	% h(Name, Sex, Color, Rider, Place)
	template([h(_, _, _, _, 1), h(_, _, _, _, 2), h(_, _, _, _, 3), h(_, _, _, _, 4), h(_, _, _, _, 5)]).

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
