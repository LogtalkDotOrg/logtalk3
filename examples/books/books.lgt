%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% book(Name, Author, Year)
book('The Philosopher''s Stone', 'J. K. Rowling', 1997).
book('The Chamber of Secrets',   'J. K. Rowling', 1998).
book('The Prisoner of Azkaban',  'J. K. Rowling', 1999).
book('The Goblet of Fire',       'J. K. Rowling', 2000).
book('The Order of the Phoenix', 'J. K. Rowling', 2003).
book('The Half-Blood Prince',    'J. K. Rowling', 2005).
book('The Deathly Hallows',      'J. K. Rowling', 2007).

% extra(Book, Kind)
extra('The Philosopher''s Stone', quidditch_set).
extra('The Chamber of Secrets',   map).
extra('The Half-Blood Prince',    audio_cd).
extra('The Deathly Hallows',      horcrux_set).

% weight(Extra, Weight)
weight(quidditch_set, 278).
weight(horcrux_set,   123).


:- object(data_acquisition).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2019/11/22,
		comment is 'Data acquisition example, decoupled from data processing.'
	]).

	:- public(book/1).
	:- mode(book(-pair(atom,optional)), zero_or_more).
	:- info(book/1, [
		comment is 'Returns a book record represented as a pair title-optional where the optional represents the possible presence of a book extra material.'
	]).

	:- uses(user, [
		book/3, extra/2, weight/2
	]).

	book(Title-OptionalExtra) :-
		book(Title, _, _),
		% instead of using a special value to represent the absence of a book extra,
		% we use an optional to represent the possible existence of an extra; as some
		% extras have a known weight, we use a second optional for the weight
		optional::from_goal(extra(Title, Extra), Extra-OptionalWeight, OptionalExtra),
		optional::from_goal((extra(Title, Extra), weight(Extra, Weight)), Weight, OptionalWeight).

:- end_object.


:- object(data_processing).

	:- info([
		version is 1.1,
		author is 'Paulo Moura',
		date is 2019/11/22,
		comment is 'Data processing example, decoupled from data acquisition.'
	]).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Prints a list of the books with their optional extras.'
	]).

	% by using optional terms, we don't need to use control constructs
	% such as if-then-else or cut to handle optional values in the data

	print :-
		forall(
			data_acquisition::book(Title-Extra),
			(	write(Title), nl,
				optional(Extra)::if_present(print_extra)
			)
		).

	print_extra(Extra-Weight) :-
		write('  with free '), write(Extra),
		optional(Weight)::if_present([Grams]>>(write(' at '), write(Grams), write(' gr'))),
		nl.

:- end_object.
