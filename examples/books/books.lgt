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


% the raw data in this example are books, some of them sold with extras, and
% with some of extras weighting enough for their weight to be declared:

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

	:- public(book/4).
	:- mode(book(?atom, ?atom, ?integer, --optional), zero_or_more).
	:- info(book/4, [
		comment is 'Returns a book record represented as a pair title-optional where the optional represents the possible presence of a book extra, which in turn may have a registered weight.'
	]).

	:- uses(user, [
		book/3, extra/2, weight/2
	]).

	book(Title, Author, Year, OptionalExtra) :-
		book(Title, Author, Year),
		% instead of using a special value to represent the absence of a book extra,
		% we use an optional to represent the possible existence of an extra; as some
		% extras have a registered weight, we use a second optional for the weight
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

	% by using optionals, we don't need a representation for "null"
	% values and we also don't need to use control constructs such
	% as conditionals or cuts to handle optional values in the data

	% in this case, we use the optional/1::if_present/1 meta-predicate
	% that does nothing if the optional is empty

	print :-
		forall(
			data_acquisition::book(Title, _Author, _Year, Extra),
			(	write(Title), nl,
				optional(Extra)::if_present(print_extra)
			)
		).

	print_extra(Extra-Weight) :-
		write('  with free '), write(Extra),
		optional(Weight)::if_present([Grams]>>(write(' at '), write(Grams), write(' gr'))),
		nl.

	:- public(print_kg/0).
	:- mode(print_kg, one).
	:- info(print_kg/0, [
		comment is 'Prints a list of the books with their optional extras but with optional weight converted into Kilograms.'
	]).

	% we can easily map optionals without requiring additional code for
	% handling empty optionals; in this case we call the optional/1::map/2
	% meta-predicate to map an optional weight expressed in grams into an
	% optional weight expressed in kilograms

	print_kg :-
		forall(
			data_acquisition::book(Title, _Author, _Year, Extra),
			(	write(Title), nl,
				optional(Extra)::if_present(print_extra_kg)
			)
		).

	print_extra_kg(Extra-WeightGrams) :-
		write('  with free '), write(Extra),
		optional(WeightGrams)::map([Grams,Kilos]>>(Kilos is Grams / 1000), WeightKilos),
		optional(WeightKilos)::if_present([Kilograms]>>(write(' at '), write(Kilograms), write(' kg'))),
		nl.

	:- public(print_heavy_extras/0).
	:- mode(print_heavy_extras, one).
	:- info(print_heavy_extras/0, [
		comment is 'Prints a list heavy extras.'
	]).

	% we can chain optionals with some help from lambda expressions;
	% in this case, we call the optional/1::if_present/1 meta-predicate
	% with an optional/1::if_present/1 argument; we only consider books
	% with an extra with a registered weight

	print_heavy_extras :-
		forall(
			data_acquisition::book(_Title, _Author, _Year, Extra),
			optional(Extra)::if_present(
				[Name-Weight]>>(optional(Weight)::if_present(print_heavy_extra(Name)))
			)
		).

	print_heavy_extra(Name, Grams) :-
		write(Name), write(' at '), write(Grams), write(' gr'), nl.

	:- public(books_with_extras/1).
	:- mode(books_with_extras(--list(atom)), one).
	:- info(books_with_extras/1, [
		comment is 'Returns a list of the book titles that have extras.',
		argnames is ['Titles']
	]).

	% we can easily act on the presence (or absence) of optional terms;
	% in this case, we call optional/1::is_present/0 to simply fail for
	% books with no extras

	books_with_extras(Titles) :-
		findall(
			Title,
			(	data_acquisition::book(Title, _Author, _Year, Extra),
				optional(Extra)::is_present
			),
			Titles
		).

:- end_object.
