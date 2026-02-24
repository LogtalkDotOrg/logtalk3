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


:- object(birds_mcp,
	implements(mcp_tool_protocol)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2026-02-24,
		comment is 'MCP tool provider for the bird identification expert system. Uses MCP elicitation to ask the user questions during bird identification, replacing the terminal I/O of the original birds example.',
		remarks is [
			'Elicitation' - 'This tool provider declares the ``elicitation`` capability so that the MCP server advertises it to the client. During bird identification, the server sends ``elicitation/create`` requests to ask the user about bird characteristics (yes/no questions and multiple-choice menus).',
			'Knowledge base' - 'Uses the bird taxonomy from the ``birds`` example (the ``order`` prototype hierarchy). The ``descriptors`` category defines the askable attributes.'
		]
	]).

	:- private(known_/3).
	:- dynamic(known_/3).

	:- uses(term_io, [
		write_to_atom/2
	]).

	% ==========================================================================
	% mcp_tool_protocol implementation
	% ==========================================================================

	capabilities([elicitation]).

	tools([
		tool(identify_bird, identify_bird, 0)
	]).

	:- public(identify_bird/0).
	:- mode(identify_bird, one).
	:- info(identify_bird/0, [
		comment is 'Identifies a bird by asking the user questions about its characteristics. Uses MCP elicitation to present yes/no questions and multiple-choice menus. Returns the identified bird species or a message that no bird could be identified.'
	]).

	tool_call(identify_bird, _Arguments, Elicit, Result) :-
		identify(Elicit, Result).

	% ==========================================================================
	% Bird identification logic
	% ==========================================================================

	identify(Elicit, Result) :-
		retractall(known_(_, _, _)),
		(	order::leaf(Bird),
			check(Elicit, Bird) ->
			bird_name(Bird, Name),
			atom_concat('Identified bird: ', Name, Text),
			Result = text(Text)
		;	Result = text('No bird could be identified from the given characteristics.')
		).

	% Check if a candidate bird matches by asking about each of its descriptors
	check(Elicit, Bird) :-
		forall(
			(order::descriptor(Name/Arity), functor(Predicate, Name, Arity), Bird::Predicate),
			ask_descriptor(Elicit, Predicate)
		).

	% ==========================================================================
	% Descriptor-based elicitation dispatch
	% ==========================================================================

	% Route each descriptor predicate to the appropriate question type
	ask_descriptor(Elicit, Predicate) :-
		Predicate =.. [Attribute, Value],
		(	menu_attribute(Attribute, Menu) ->
			menuask(Elicit, Attribute, Value, Menu)
		;	ask(Elicit, Attribute, Value)
		).

	% Attributes that use multiple-choice menus (matching the original expert)
	menu_attribute(flight, [ponderous, powerful, agile, flap_glide, other]).
	menu_attribute(flight_profile, [flat, v_shaped, other]).
	menu_attribute(size, [large, plump, medium, small]).
	menu_attribute(tail, [narrow_at_tip, forked, long_rusty, square, other]).

	% ==========================================================================
	% Yes/No questions via elicitation
	% ==========================================================================

	:- meta_predicate(ask(3, *, *)).

	ask(_Elicit, Attribute, Value) :-
		known_(yes, Attribute, Value),
		!.
	ask(_Elicit, Attribute, Value) :-
		known_(_, Attribute, Value),
		!, fail.
	ask(_Elicit, Attribute, _) :-
		known_(yes, Attribute, _),
		!, fail.
	ask(Elicit, Attribute, Value) :-
		ask_question(Elicit, Attribute, Value, Answer),
		(	Answer = accept(Content),
			has_pair(Content, answer, UserAnswer) ->
			asserta(known_(UserAnswer, Attribute, Value)),
			UserAnswer == yes
		;	% decline or cancel: treat as no
			asserta(known_(no, Attribute, Value)),
			fail
		).

	:- meta_predicate(ask_question(3, *, *, *)).

	ask_question(Elicit, Attribute, Value, Answer) :-
		% Build the question message
		atom_concat(Attribute, ': ', Temp1),
		atom_concat(Temp1, Value, Temp2),
		atom_concat(Temp2, '?', Message),
		% Build a yes/no schema
		Schema = {
			type-object,
			properties-{
				answer-{type-string, enum-[yes, no]}
			},
			required-[answer]
		},
		call(Elicit, Message, Schema, Answer).

	% ==========================================================================
	% Multiple-choice menus via elicitation
	% ==========================================================================

	:- meta_predicate(menuask(3, *, *, *)).

	menuask(_Elicit, Attribute, Value, _Menu) :-
		known_(yes, Attribute, Value),
		!.
	menuask(_Elicit, Attribute, _, _Menu) :-
		known_(yes, Attribute, _),
		!, fail.
	menuask(Elicit, Attribute, AskValue, Menu) :-
		% Build the question message
		atom_concat('What is the value for ', Attribute, Temp),
		atom_concat(Temp, '?', Message),
		% Build an enum schema with the menu options
		atoms_to_enum(Menu, EnumList),
		Schema = {
			type-object,
			properties-{
				answer-{type-string, enum-EnumList}
			},
			required-[answer]
		},
		call(Elicit, Message, Schema, Answer),
		(	Answer = accept(Content),
			has_pair(Content, answer, AnswerValue) ->
			asserta(known_(yes, Attribute, AnswerValue)),
			AskValue = AnswerValue
		;	% decline or cancel: fail
			fail
		).

	% ==========================================================================
	% Auxiliary predicates
	% ==========================================================================

	% Convert a bird object name to a display name
	% (replace underscores with spaces)
	bird_name(Bird, Name) :-
		write_to_atom(Bird, Name).

	% Convert a list of atoms to the same list (already suitable for JSON enum)
	atoms_to_enum([], []).
	atoms_to_enum([Atom| Rest], [Atom| EnumRest]) :-
		atoms_to_enum(Rest, EnumRest).

	% curly-term pair lookup
	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :- !.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
