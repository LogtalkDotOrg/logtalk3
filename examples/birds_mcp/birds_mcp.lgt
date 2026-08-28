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
	implements([mcp_resource_protocol, mcp_tool_protocol, mcp_multiround_protocol])).

	:- info([
		version is 2:0:0,
		author is 'Paulo Moura',
		date is 2026-08-28,
		comment is 'MCP tool provider for the bird identification expert system. Supports both the 2025-06-18 synchronous elicitation API (``tool_call/4``) and the 2026-07-28 multi-round tool results API (``tool_call_round/4``). Shared search and question helpers drive both paths.',
		remarks is [
			'2025-06-18 elicitation' - 'Declares the client ``elicitation`` capability. When the client advertises support, the server sends ``elicitation/create`` requests via the ``Elicit`` closure passed to ``tool_call/4``.',
			'2026-07-28 MRTR' - 'Implements ``tool_call_round/4`` from ``mcp_multiround_protocol``. Each round either returns ``input_required`` with the next yes/no or menu question, or ``complete`` with the identification result. Known answers are carried in opaque ``requestState``.',
			'Shared core' - 'Both paths use the same ``find_next/1`` search over ``known_/3``, the same question message/schema builders, and the same answer application helpers. 2025 calls the elicitation closure in a loop; 2026 returns ``input_required`` and resumes from ``requestState``.',
			'Knowledge base' - 'Uses the bird taxonomy from the ``birds`` example (the ``order`` prototype hierarchy).'
		]
	]).

	:- public(identify_bird/0).
	:- mode(identify_bird, one).
	:- info(identify_bird/0, [
		comment is 'Identifies a bird by asking the user questions about its characteristics. Under 2025-06-18 uses synchronous MCP elicitation; under 2026-07-28 uses multi-round input_required results.'
	]).

	:- private(known_/3).
	:- dynamic(known_/3).

	:- uses(list, [
		member/2
	]).

	:- uses(term_io, [
		write_to_atom/2, read_term_from_atom/3
	]).

	:- uses(user, [
		atomic_list_concat/2
	]).

	% ==========================================================================
	% mcp_tool_protocol implementation
	% ==========================================================================

	capabilities([resources, elicitation]).

	tools([
		tool(identify_bird, identify_bird, 0)
	]).

	% override the inferred empty output schema from the predicate declaration
	% (as the predicate have no arguments) to ensure a non-empty string result
	% (as required by e.g. VSCode) represented using the StructuredContent
	% argument in the structured(Items, StructuredContent) terms used to return
	% the tool results
	output_schema(identify_bird, {
		type-object,
		properties-{message-{type-string}},
		required-[message]
	}).

	% 2025-06-18 path
	tool_call(identify_bird, _Arguments, Elicit, Result) :-
		identify(Elicit, Result).

	% ==========================================================================
	% mcp_resource_protocol implementation
	% ==========================================================================

	resources([
		resource('logtalk://birds/attributes', bird_attributes, 'List of bird attributes', 'application/json'),
		resource('logtalk://birds/list', list_known_birds, 'List of known birds', 'application/json')
	]).

	resource_read('logtalk://birds/attributes', _Arguments, Result) :-
		findall(Attribute, order::descriptor(Attribute/_), Attributes),
		write_to_atom(Attributes, Text),
		Result = contents([
			text_content('logtalk://birds/attributes', 'application/json', Text)
		]).

	resource_read('logtalk://birds/list', _Arguments, Result) :-
		order::leaves(Birds),
		write_to_atom(Birds, Text),
		Result = contents([
			text_content('logtalk://birds/list', 'application/json', Text)
		]).

	% ==========================================================================
	% Shared identification search (known_/3 + find_next/1)
	% ==========================================================================

	% Outcome is one of:
	%   identified(Bird)
	%   need_ask(Attribute, Value)
	%   need_menu(Attribute, Value, Menu)
	%   none
	find_next(Outcome) :-
		order::leaf(Bird),
		bird_possible(Bird),
		(	bird_fully_matched(Bird) ->
			Outcome = identified(Bird)
		;	next_unanswered(Bird, Question),
			(	Question = ask(Attribute, Value) ->
				Outcome = need_ask(Attribute, Value)
			;	Question = menu(Attribute, Value, Menu) ->
				Outcome = need_menu(Attribute, Value, Menu)
			;	fail
			)
		),
		!.
	find_next(none).

	bird_possible(Bird) :-
		forall(
			(order::descriptor(Name/Arity), functor(Predicate, Name, Arity), Bird::Predicate),
			\+ descriptor_contradicted(Predicate)
		).

	descriptor_contradicted(Predicate) :-
		Predicate =.. [Attribute, Value],
		(	known_(yes, Attribute, Other),
			Other \== Value
		;	known_(no, Attribute, Value)
		).

	bird_fully_matched(Bird) :-
		forall(
			(order::descriptor(Name/Arity), functor(Predicate, Name, Arity), Bird::Predicate),
			descriptor_satisfied(Predicate)
		).

	next_unanswered(Bird, Question) :-
		order::descriptor(Name/Arity),
		functor(Predicate, Name, Arity),
		Bird::Predicate,
		\+ descriptor_satisfied(Predicate),
		!,
		Predicate =.. [Attribute, Value],
		(	menu_attribute(Attribute, Menu) ->
			Question = menu(Attribute, Value, Menu)
		;	Question = ask(Attribute, Value)
		).

	descriptor_satisfied(Predicate) :-
		Predicate =.. [Attribute, Value],
		known_(yes, Attribute, Value).

	menu_attribute(flight, [ponderous, powerful, agile, flap_glide, other]).
	menu_attribute(flight_profile, [flat, v_shaped, other]).
	menu_attribute(size, [large, plump, medium, small]).
	menu_attribute(tail, [narrow_at_tip, forked, long_rusty, square, other]).

	% ==========================================================================
	% Shared question builders (message + schema; title helps VSCode Q/A labels)
	% ==========================================================================

	yes_no_question(Attribute, Value, Message, Schema) :-
		atomic_list_concat([Attribute, ': ', Value, '?'], Message),
		Schema = {
			type-object,
			properties-{
				answer-{type-string, title-Message, enum-[yes, no]}
			},
			required-[answer]
		}.

	menu_question(Attribute, Menu, Message, Schema) :-
		atomic_list_concat(['What is the value for ', Attribute, '?'], Message),
		Schema = {
			type-object,
			properties-{
				answer-{type-string, title-Message, enum-Menu}
			},
			required-[answer]
		}.

	% ==========================================================================
	% Shared answer application (Known list <-> dynamic known_/3)
	% ==========================================================================

	setup_known(Known) :-
		retractall(known_(_, _, _)),
		assert_known_list(Known).

	assert_known_list([]).
	assert_known_list([known(Answer, Attribute, Value)| Rest]) :-
		assertz(known_(Answer, Attribute, Value)),
		assert_known_list(Rest).

	clear_known :-
		retractall(known_(_, _, _)).

	% Apply a form elicitation answer to the dynamic store (2025 loop).
	apply_elicit_answer(ask(Attribute, Value), accept(Content)) :-
		has_pair(Content, answer, UserAnswer),
		!,
		assertz(known_(UserAnswer, Attribute, Value)).
	apply_elicit_answer(ask(Attribute, Value), _) :-
		assertz(known_(no, Attribute, Value)).
	apply_elicit_answer(menu(Attribute, _AskValue, _Menu), accept(Content)) :-
		has_pair(Content, answer, AnswerValue),
		!,
		assertz(known_(yes, Attribute, AnswerValue)).
	apply_elicit_answer(menu(_, _, _), _).

	% Apply input_response/2 list to a Known list (2026 rounds).
	apply_responses(Pending, Responses, Known0, Known, Status) :-
		(	member(input_response(q, accept(Content)), Responses) ->
			apply_accept_to_known(Pending, Content, Known0, Known),
			Status = continue
		;	member(input_response(q, decline), Responses) ->
			Known = Known0,
			Status = declined
		;	member(input_response(q, cancel), Responses) ->
			Known = Known0,
			Status = cancelled
		;	default_response(Pending, Known0, Known),
			Status = continue
		).

	apply_accept_to_known(ask(Attribute, Value), Content, Known, [known(UserAnswer, Attribute, Value)| Known]) :-
		has_pair(Content, answer, UserAnswer),
		!.
	apply_accept_to_known(ask(Attribute, Value), _Content, Known, [known(no, Attribute, Value)| Known]).
	apply_accept_to_known(menu(Attribute, _, _), Content, Known, [known(yes, Attribute, AnswerValue)| Known]) :-
		has_pair(Content, answer, AnswerValue),
		!.
	apply_accept_to_known(menu(_, _, _), _Content, Known, Known).

	default_response(ask(Attribute, Value), Known, [known(no, Attribute, Value)| Known]).
	default_response(menu(_, _, _), Known, Known).

	complete_result(Text, Result) :-
		Result = structured([text(Text)], {message-Text}).

	no_bird_text('No bird could be identified from the given characteristics.').

	% ==========================================================================
	% 2025-06-18 path: same search, elicit in-process
	% ==========================================================================

	:- meta_predicate(identify(3, *)).

	identify(Elicit, Result) :-
		retractall(known_(_, _, _)),
		identify_loop(Elicit, Result),
		clear_known.

	:- meta_predicate(identify_loop(3, *)).

	identify_loop(Elicit, Result) :-
		find_next(Outcome),
		(	Outcome = identified(Bird) ->
			atom_concat('Identified bird: ', Bird, Text),
			complete_result(Text, Result)
		;	Outcome = need_ask(Attribute, Value) ->
			yes_no_question(Attribute, Value, Message, Schema),
			call(Elicit, Message, Schema, Answer),
			apply_elicit_answer(ask(Attribute, Value), Answer),
			identify_loop(Elicit, Result)
		;	Outcome = need_menu(Attribute, Value, Menu) ->
			menu_question(Attribute, Menu, Message, Schema),
			call(Elicit, Message, Schema, Answer),
			apply_elicit_answer(menu(Attribute, Value, Menu), Answer),
			identify_loop(Elicit, Result)
		;	% Outcome == none
			no_bird_text(Text),
			complete_result(Text, Result)
		).

	% ==========================================================================
	% 2026-07-28 path: same search, input_required + requestState
	% ==========================================================================
	%
	% requestState is an opaque string (write_to_atom of a curly-term):
	%   {known-[...], pending-{type-..., ...}}

	tool_call_round(identify_bird, _Arguments, Context, RoundResult) :-
		Context = request_context(_ClientCaps, InputResponses, RequestState, _Progress),
		(	RequestState == none ->
			next_round([], RoundResult)
		;	decode_state(RequestState, Known0, Pending) ->
			apply_responses(Pending, InputResponses, Known0, Known1, Status),
			(	Status == continue ->
				next_round(Known1, RoundResult)
			;	no_bird_text(Text),
				complete_result(Text, Complete),
				RoundResult = complete(Complete)
			)
		;	next_round([], RoundResult)
		).

	next_round(Known, RoundResult) :-
		setup_known(Known),
		find_next(Outcome),
		(	Outcome = identified(Bird) ->
			atom_concat('Identified bird: ', Bird, Text),
			complete_result(Text, Complete),
			RoundResult = complete(Complete)
		;	Outcome = need_ask(Attribute, Value) ->
			yes_no_question(Attribute, Value, Message, Schema),
			encode_state(Known, ask(Attribute, Value), State),
			RoundResult = input_required(
				[input_request(q, form_elicitation(Message, Schema))],
				State
			)
		;	Outcome = need_menu(Attribute, Value, Menu) ->
			menu_question(Attribute, Menu, Message, Schema),
			encode_state(Known, menu(Attribute, Value, Menu), State),
			RoundResult = input_required(
				[input_request(q, form_elicitation(Message, Schema))],
				State
			)
		;	no_bird_text(Text),
			complete_result(Text, Complete),
			RoundResult = complete(Complete)
		),
		clear_known.

	% requestState encode / decode

	decode_state(StateAtom, Known, Pending) :-
		atom(StateAtom),
		StateAtom \== '',
		catch(read_term_from_atom(StateAtom, State, []), _, fail),
		!,
		decode_state_term(State, Known, Pending).
	decode_state(State, Known, Pending) :-
		decode_state_term(State, Known, Pending).

	decode_state_term(State, Known, Pending) :-
		has_pair(State, known, KnownRaw),
		has_pair(State, pending, PendingRaw),
		decode_known(KnownRaw, Known),
		decode_pending(PendingRaw, Pending).

	decode_known([], []).
	decode_known([{Pairs}| Rest], [known(Answer, Attribute, Value)| Out]) :-
		curly_member(answer-Answer, Pairs),
		curly_member(attribute-Attribute, Pairs),
		curly_member(value-Value, Pairs),
		decode_known(Rest, Out).

	decode_pending(PendingRaw, ask(Attribute, Value)) :-
		has_pair(PendingRaw, type, ask),
		has_pair(PendingRaw, attribute, Attribute),
		has_pair(PendingRaw, value, Value),
		!.
	decode_pending(PendingRaw, menu(Attribute, Value, Menu)) :-
		has_pair(PendingRaw, type, menu),
		has_pair(PendingRaw, attribute, Attribute),
		has_pair(PendingRaw, value, Value),
		has_pair(PendingRaw, menu, Menu).

	encode_state(Known, Pending, StateAtom) :-
		encode_known(Known, KnownJson),
		encode_pending(Pending, PendingJson),
		Term = {known-KnownJson, pending-PendingJson},
		write_to_atom(Term, StateAtom).

	encode_known([], []).
	encode_known([known(Answer, Attribute, Value)| Rest], [{answer-Answer, attribute-Attribute, value-Value}| Out]) :-
		encode_known(Rest, Out).

	encode_pending(ask(Attribute, Value), {type-ask, attribute-Attribute, value-Value}).
	encode_pending(menu(Attribute, Value, Menu), {type-menu, attribute-Attribute, value-Value, menu-Menu}).

	% ==========================================================================
	% Auxiliary predicates
	% ==========================================================================

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
