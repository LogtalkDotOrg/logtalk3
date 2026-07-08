%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(open_ai_event_stream).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-07-08,
		comment is 'Small Server-Sent Events helpers used by OpenAI streaming endpoints.'
	]).

	:- public(event/2).
	:- mode(event(+atom, -compound), one_or_error).
	:- info(event/2, [
		comment is 'Parses a single OpenAI event-stream frame atom into ``event(Name, Data)``. Unknown fields are ignored.',
		argnames is ['Frame', 'Event'],
		exceptions is [
			'``Frame`` is not an event-stream frame atom' - domain_error(open_ai_event_stream_frame, 'Frame')
		]
	]).

	:- public(frame/2).
	:- mode(frame(+compound, -atom), one_or_error).
	:- info(frame/2, [
		comment is 'Generates an event-stream frame atom from ``event(Name, Data)``.',
		argnames is ['Event', 'Frame'],
		exceptions is [
			'``Event`` is not an event-stream event term' - domain_error(open_ai_event_stream_event, 'Event')
		]
	]).

	event(Frame, event(Name, Data)) :-
		atom(Frame),
		!,
		(	frame_field(Frame, event, Name0) ->
			Name = Name0
		;	Name = message
		),
		(	frame_field(Frame, data, Data0) ->
			Data = Data0
		;	Data = ''
		).
	event(Frame, _) :-
		domain_error(open_ai_event_stream_frame, Frame).

	frame(event(Name, Data), Frame) :-
		atom(Name),
		atom(Data),
		!,
		atom_concat('event: ', Name, Prefix0),
		atom_concat(Prefix0, '\n', Prefix1),
		atom_concat(Prefix1, 'data: ', Prefix2),
		atom_concat(Prefix2, Data, Prefix3),
		atom_concat(Prefix3, '\n\n', Frame).
	frame(Event, _) :-
		domain_error(open_ai_event_stream_event, Event).

	frame_field(Frame, Field, Value) :-
		atom_concat(Field, ': ', Prefix),
		line(Frame, Line),
		atom_concat(Prefix, Rest, Line),
		!,
		Value = Rest.

	line(Frame, Line) :-
			atom_codes(Frame, Codes),
			lines(Codes, LineCodesList),
			member(LineCodes, LineCodesList),
			atom_codes(Line, LineCodes).

		lines(Codes, Lines) :-
			lines(Codes, [], [], Lines).

		lines([], Current, Lines0, Lines) :-
			reverse(Current, Line),
			reverse([Line| Lines0], Lines).
		lines([10| Codes], Current, Lines0, Lines) :-
			!,
			reverse(Current, Line),
			lines(Codes, [], [Line| Lines0], Lines).
		lines([Code| Codes], Current, Lines0, Lines) :-
			lines(Codes, [Code| Current], Lines0, Lines).

		reverse(List, Reversed) :-
			reverse(List, [], Reversed).

		reverse([], Reversed, Reversed).
		reverse([Head| Tail], Accumulator, Reversed) :-
			reverse(Tail, [Head| Accumulator], Reversed).

	member(Element, [Element| _]).
	member(Element, [_| Elements]) :-
		member(Element, Elements).

:- end_object.
