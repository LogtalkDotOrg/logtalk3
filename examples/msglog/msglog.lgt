%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(msglog,
	% built-in protocol for event handler methods
	implements(monitoring)).

	:- info([
		version is 1:3:0,
		author is 'Paulo Moura',
		date is 2019-01-19,
		comment is 'Monitor for recording, replaying, and saving user messages.'
	]).

	:- public(record/0).
	:- mode(record, one).
	:- info(record/0, [
		comment is 'Starts recording messages.'
	]).

	:- public(stop/0).
	:- mode(stop, one).
	:- info(stop/0, [
		comment is 'Stops recording messages.'
	]).

	:- public(replay/0).
	:- mode(replay, one).
	:- info(replay/0, [
		comment is 'Replays all recorded messages.'
	]).

	:- public(print/0).
	:- mode(print, one).
	:- info(print/0, [
		comment is 'Prints recorded messages, one per line.'
	]).

	:- public(erase/0).
	:- mode(erase, one).
	:- info(erase/0, [
		comment is 'Erases recorded messages.'
	]).

	:- public(log/2).
	:- mode(log(?object_identifier, ?callable), zero_or_more).
	:- info(log_/2, [
		comment is 'Enumerates by backtracking all recorded messages.',
		argnames is ['Object', 'Message']
	]).

	:- private(log_/2).
	:- dynamic(log_/2).
	:- mode(log_(?object_identifier, ?callable), zero_or_more).
	:- info(log_/2, [
		comment is 'Table of recorded messages.',
		argnames is ['Object', 'Message']
	]).

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
			(writeq(Object), write('::'), writeq(Message), write('.'), nl)
		).

	erase :-
		::retractall(log_(_, _)).

	log(Object, Message) :-
		log_(Object, Message).

	before(Object, Message, _) :-
		self(Self),
		(	Self = Object ->
			true
		;	::assertz(log_(Object, Message))
		).

:- end_object.
