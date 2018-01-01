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


:- object(faa).

	:- info([
		version is 1.1,
		date is 2016/05/18,
		author is 'Paulo Moura',
		comment is 'Adaptation of the command language DCG example from the Amzi! Prolog manual.'
	]).

	:- public(main/0).
	:- mode(main, one).
	:- info(main/0, [
		comment is 'Starts iteractive command language interpreter.'
	]).

	:- private(booked/2).
	:- dynamic(booked/2).
	:- mode(booked(?atom, ?atom), zero_or_more).
	:- info(booked/2, [
		comment is 'Booked places in flight.',
		argnames is ['Passenger', 'Flight']
	]).

	main :-
		write('Fly Amzi! Air'), nl,
		repeat,
			do_command(Command),
		Command == exit.

	:- meta_predicate(do_command(0)).
	do_command(Command) :-
		write('enter command> '),
		read_tokens(Tokens),
		phrase(command(List), Tokens),
		Command =.. List,
		call(Command),
		!.

	read_tokens(Tokens) :-
		read_codes(Codes),
		codes_to_tokens(Codes, Tokens).

	read_codes(Codes) :-
		get_code(Code), 
		read_codes(Code, Codes).

	read_codes(10, [[]]) :-
		!.
	read_codes(13, [[]]) :-
		!.
	read_codes(32, [[]| Rest]) :-
		!, read_codes(Rest).
	read_codes(Code, [[Code| Codes]| Rest]) :-
		read_codes([Codes| Rest]).

	codes_to_tokens([], []).
	codes_to_tokens([List| Lists], [Token| Tokens]) :-
		atom_codes(Token, List),
		codes_to_tokens(Lists, Tokens).

	command([Op| Args]) --> operation(Op), arguments(Args).

	arguments([Arg| Args]) --> argument(Arg), arguments(Args).
	arguments([]) --> [].

	operation(report) --> [list].
	operation(book) --> [book].
	operation(exit) --> ([exit]; [quit]; [bye]).

	argument(passengers) --> [passengers].
	argument(flights) --> [flights].

	argument(Flight) --> [Flight], {flight(Flight)}.
	argument(Passenger) --> [Passenger].

	flight(aa101).
	flight(aa102).
	flight(aa103).

	report(flights) :-
		flight(Flight),
		write(Flight), nl,
		fail.
	report(_).

	report(passengers, Flight) :-
		booked(Passenger, Flight),
		write(Passenger), nl,
		fail.
	report(_, _).

	book(Passenger, Flight) :-
		assertz(booked(Passenger, Flight)).

	exit.

:- end_object.
