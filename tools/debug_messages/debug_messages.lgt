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


:- object(debug_messages).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2019/01/16,
		comment is 'Supports enabling and disabling of debug and debug(Group) messages in normal mode.',
		remarks is [
			'Motivation' - 'Allow selective enabling of debug messages, which are skipped by default in normal mode, without requiring turning on the "debug" flag.',
			'Limitations' - 'Debug messages are suppressed by the compiler when the "optimize" flag is turned on and thus cannot be enabled in this case.'
		]
	]).

	:- public(enable/1).
	:- mode(enable(@term), one).
	:- info(enable/1, [
		comment is 'Enables all debug and debug(Group) messages for the given component.',
		argnames is ['Component']
	]).

	:- public(disable/1).
	:- mode(disable(@term, @term), one).
	:- info(disable/1, [
		comment is 'Disables all debug and debug(Group) messages for the given component.',
		argnames is ['Component']
	]).

	:- public(enabled/1).
	:- mode(enabled(?term), zero_or_more).
	:- info(enabled/1, [
		comment is 'Enumerates by backtraking the components with enabled debug and debug(Group) messages.',
		argnames is ['Component']
	]).

	:- public(enable/2).
	:- mode(enable(@term, @term), one).
	:- info(enable/2, [
		comment is 'Enables debug(Group) messages for the given component and group.',
		argnames is ['Component', 'Group']
	]).

	:- public(disable/2).
	:- mode(disable(@term, @term), one).
	:- info(disable/2, [
		comment is 'Disables debug(Group) messages for the given component and group.',
		argnames is ['Component', 'Group']
	]).

	:- public(enabled/2).
	:- mode(enabled(?term, ?term), zero_or_more).
	:- info(enabled/2, [
		comment is 'Enumerates by backtraking the enabled debug(Group) messages for each component.',
		argnames is ['Component', 'Group']
	]).

	:- private(enabled_/1).
	:- dynamic(enabled_/1).
	:- mode(enabled_(?term), zero_or_more).
	:- info(enabled_/1, [
		comment is 'Table of components with currently enabled debug and debug(Group) messages.',
		argnames is ['Component']
	]).

	:- private(enabled_/2).
	:- dynamic(enabled_/2).
	:- mode(enabled_(?term, ?term), zero_or_more).
	:- info(enabled_/2, [
		comment is 'Table of currently enabled debug(Group) per component.',
		argnames is ['Component', 'Group']
	]).

	enable(Component) :-
		retractall(enabled_(_, _)),
		retractall(enabled_(_)),
		assertz(enabled_(Component)).

	disable(Component) :-
		retractall(enabled_(_, _)),
		retractall(enabled_(Component)).

	enabled(Component) :-
		enabled_(Component).

	enable(Component, Group) :-
		retractall(enabled_(_, _)),
		assertz(enabled_(Component, Group)).

	disable(Component, Group) :-
		retractall(enabled_(Component, Group)).

	enabled(Component, Group) :-
		enabled_(Component, Group).

	:- multifile(logtalk::message_hook/4).
	:- dynamic(logtalk::message_hook/4).

	logtalk::message_hook(_, debug, Component, Tokens) :-
		enabled_(Component),
		logtalk::message_prefix_stream(debug, Component, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

	logtalk::message_hook(_, debug(Group), Component, Tokens) :-
		(	enabled_(Component)
		;	enabled_(Component, Group)
		),
		logtalk::message_prefix_stream(debug(Group), Component, Prefix, Stream),
		logtalk::print_message_tokens(Stream, Prefix, Tokens).

:- end_object.
