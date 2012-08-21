%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(faa).

	:- info([
		version is 1.0,
		date is 2004/5/10,
		author is 'Paulo Moura',
		comment is 'Adaptation of the command language DCG example from the Amzi! Prolog manual.']).

	:- public(main/0).
	:- mode(main, one).
	:- info(main/0, [
		comment is 'Starts iteractive command language interpreter.']).

	:- private(booked/2).
	:- dynamic(booked/2).
	:- mode(booked(?atom, ?atom), zero_or_more).
	:- info(booked/2, [
		comment is 'Booked places in flight.',
		argnames is ['Passenger', 'Flight']]).

	main :-
		write('Fly Amzi! Air'), nl,
		repeat,
			do_command(Command),
		Command == exit.

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
