%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(msglog,
	implements(monitoring)).	% built-in protocol for event handler methods

	:- info([
		version is 1.2,
		author is 'Paulo Moura',
		date is 2008/10/20,
		comment is 'Monitor for recording, replaying, and saving user messages.']).

	:- public(record/0).
	:- mode(record, one).
	:- info(record/0,
		[comment is 'Starts recording messages.']).

	:- public(stop/0).
	:- mode(stop, one).
	:- info(stop/0,
		[comment is 'Stops recording messages.']).

	:- public(replay/0).
	:- mode(replay, one).
	:- info(replay/0,
		[comment is 'Replays all recorded messages.']).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0,
		[comment is 'Prints recorded messages, one per line.']).

	:- public(erase/0).
	:- mode(erase, one).
	:- info(erase/0,
		[comment is 'Erases recorded messages.']).

	:- private(log_/2).
	:- dynamic(log_/2).
	:- mode(log_(+object, +nonvar), zero_or_more).
	:- info(log_/2,
		[comment is 'Table of recorded messages.',
		 argnames is ['Object', 'Message']]).

	record :-
		self(Self),
		abolish_events(_, _, _, _, Self),
		define_events(before, _, _, user, Self),
		set_logtalk_flag(events, allow).

	stop :-
		set_logtalk_flag(events, deny),
		self(Self),
		abolish_events(_, _, _, _, Self).

	replay :-
		self(Self),
		abolish_events(_, _, _, _, Self),
		forall(::log_(Object, Message), {Object::Message}).

	print :-
		forall(
			::log_(Object, Message),
			(writeq(Object), write('::'), writeq(Message), write('.'), nl)).

	erase :-
		::retractall(log_(_, _)).

	before(Object, Message, _) :-
		self(Self),
		(	Self = Object ->
			true
		;	::assertz(log_(Object, Message))
		).

:- end_object.
