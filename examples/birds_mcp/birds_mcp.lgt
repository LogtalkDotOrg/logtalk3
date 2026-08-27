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
		date is 2026-08-27,
		comment is 'MCP tool provider for the bird identification expert system. Supports both the 2025-06-18 synchronous elicitation API (``tool_call/4``) and the 2026-07-28 multi-round tool results API (``tool_call_round/4``).',
		remarks is [
			'2025-06-18 elicitation' - 'Declares the client ``elicitation`` capability. When the client advertises support, the server sends ``elicitation/create`` requests via the ``Elicit`` closure passed to ``tool_call/4``.',
			'2026-07-28 MRTR' - 'Implements ``tool_call_round/4`` from ``mcp_multiround_protocol``. Each round either returns ``input_required`` with the next yes/no or menu question, or ``complete`` with the identification result. Known answers are carried in opaque ``requestState``.',
			'Knowledge base' - 'Uses the bird taxonomy from the ``birds`` example (the ``order`` prototype hierarchy).'
		]
	]).

	:- private(known_/3).
	:- dynamic(known_/3).

	:- uses(list, [
		member/2
	]).

	:- uses(term_io, [
		write_to_atom/2, read_term_from_atom/3
	]).

	% ==========================================================================
	% mcp_tool_protocol implementation
	% ==========================================================================

	capabilities([resources, elicitation]).

	tools([
		tool(identify_bird, identify_bird, 0)
	]).

	:- public(identify_bird/0).
	:- mode(identify_bird, one).
	:- info(identify_bird/0, [
		comment is 'Identifies a bird by asking the user questions about its characteristics. Under 2025-06-18 uses synchronous MCP elicitation; under 2026-07-28 uses multi-round input_required results.'
	]).

	% 2025-06-18 path
	tool_call(identify_bird, _Arguments, Elicit, Result) :-
		identify(Elicit, Result).

	% ==========================================================================
	% mcp_resource_protocol implementation
	% ==========================================================================

    resources([
        resource('logtalk://birds/attributes', bird_atributes, 'List of bird attributes', 'application/json'),
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
	% 2026-07-28 multi-round path
	% ==========================================================================
	%
	% requestState encoding (MCP 2026-07-28 requires an opaque *string* on the wire):
	%   none  - first round
	%   atom  - Prolog term written with write_to_atom/2 of
	%           {known-[...], pending-{type-..., ...}}
	%           Known = [known(Answer,Attr,Val)|...]
	%           Pending = ask(Attr,Val) | menu(Attr,Val,Menu)

	tool_call_round(identify_bird, _Arguments, Context, RoundResult) :-
		Context = request_context(_ClientCaps, InputResponses, RequestState, _Progress),
		(	RequestState == none ->
			next_round([], RoundResult)
		;	decode_state(RequestState, Known0, Pending) ->
			apply_responses(Pending, InputResponses, Known0, Known1, Status),
			(	Status == continue ->
				next_round(Known1, RoundResult)
			;	RoundResult = complete(structured([text('No bird could be identified from the given characteristics.')], {}))
			)
		;	next_round([], RoundResult)
		).

	% Accept either a curly-term (in-process) or an atom string echoed by the client.
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
	decode_known([{Pairs}| Rest], [known(A, Attr, V)| Out]) :-
		curly_member(answer-A, Pairs),
		curly_member(attribute-Attr, Pairs),
		curly_member(value-V, Pairs),
		decode_known(Rest, Out).

	decode_pending(P, ask(Attr, Val)) :-
		has_pair(P, type, ask),
		has_pair(P, attribute, Attr),
		has_pair(P, value, Val), !.
	decode_pending(P, menu(Attr, Val, Menu)) :-
		has_pair(P, type, menu),
		has_pair(P, attribute, Attr),
		has_pair(P, value, Val),
		has_pair(P, menu, Menu).

	% Always produce an atom so the adapter can put requestState as a string
	% on the wire without further transformation surprises.
	encode_state(Known, Pending, StateAtom) :-
		encode_known(Known, KnownJson),
		encode_pending(Pending, PendingJson),
		Term = {known-KnownJson, pending-PendingJson},
		write_to_atom(Term, StateAtom).

	encode_known([], []).
	encode_known([known(A, Attr, V)| Rest], [{answer-A, attribute-Attr, value-V}| Out]) :-
		encode_known(Rest, Out).

	encode_pending(ask(Attr, Val), {type-ask, attribute-Attr, value-Val}).
	encode_pending(menu(Attr, Val, Menu), {type-menu, attribute-Attr, value-Val, menu-Menu}).

	apply_responses(ask(Attribute, Value), Responses, Known0, Known, Status) :-
		(	member(input_response(q, accept(Content)), Responses),
			has_pair(Content, answer, UserAnswer) ->
			Known = [known(UserAnswer, Attribute, Value)| Known0],
			Status = continue
		;	member(input_response(q, decline), Responses) ->
			Known = Known0, Status = declined
		;	member(input_response(q, cancel), Responses) ->
			Known = Known0, Status = cancelled
		;	Known = [known(no, Attribute, Value)| Known0],
			Status = continue
		).
	apply_responses(menu(Attribute, _AskValue, _Menu), Responses, Known0, Known, Status) :-
		(	member(input_response(q, accept(Content)), Responses),
			has_pair(Content, answer, AnswerValue) ->
			Known = [known(yes, Attribute, AnswerValue)| Known0],
			Status = continue
		;	member(input_response(q, decline), Responses) ->
			Known = Known0, Status = declined
		;	member(input_response(q, cancel), Responses) ->
			Known = Known0, Status = cancelled
		;	Known = Known0, Status = continue
		).

	next_round(Known, RoundResult) :-
		setup_known(Known),
		(	find_next(Known, Outcome) ->
			(	Outcome = identified(Bird) ->
				bird_name(Bird, Name),
				atom_concat('Identified bird: ', Name, Text),
				RoundResult = complete(structured([text(Text)], {}))
			;	Outcome = need_ask(Attribute, Value) ->
				atom_concat(Attribute, ': ', T1),
				atom_concat(T1, Value, T2),
				atom_concat(T2, '?', Message),
				Schema = {
					type-object,
					properties-{answer-{type-string, enum-[yes, no]}},
					required-[answer]
				},
				encode_state(Known, ask(Attribute, Value), State),
				RoundResult = input_required(
					[input_request(q, form_elicitation(Message, Schema))],
					State
				)
			;	Outcome = need_menu(Attribute, Value, Menu) ->
				atom_concat('What is the value for ', Attribute, Temp),
				atom_concat(Temp, '?', Message),
				atoms_to_enum(Menu, EnumList),
				Schema = {
					type-object,
					properties-{answer-{type-string, enum-EnumList}},
					required-[answer]
				},
				encode_state(Known, menu(Attribute, Value, Menu), State),
				RoundResult = input_required(
					[input_request(q, form_elicitation(Message, Schema))],
					State
				)
			;	% Outcome == none,
				RoundResult = complete(structured([text('No bird could be identified from the given characteristics.')], {}))
			)
		;	RoundResult = complete(structured([text('No bird could be identified from the given characteristics.')], {}))
		),
		clear_known.

	% Find either a fully matching bird or the next unanswered descriptor.
	% Only consider birds still possible given answers already known; without
	% this filter, a menu answer other than the first bird's expected value
	% leaves that bird "unsatisfied" and the same menu is re-asked forever
	% (appearing as if only the first option is accepted).
	find_next(_Known, Outcome) :-
		order::leaf(Bird),
		bird_possible(Bird),
		(	bird_fully_matched(Bird) ->
			Outcome = identified(Bird)
		;	next_unanswered(Bird, Question) ->
			(	Question = ask(A, V) -> Outcome = need_ask(A, V)
			;	Question = menu(A, V, M) -> Outcome = need_menu(A, V, M)
			;	fail
			)
		;	fail
		),
		!.
	find_next(_, none).

	% A bird is still possible unless some known answer contradicts one of its
	% descriptors (different yes-value for the same attribute, or an explicit no).
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

	setup_known(Known) :-
		retractall(known_(_, _, _)),
		assert_known_list(Known).

	assert_known_list([]).
	assert_known_list([known(A, Attr, V)| Rest]) :-
		asserta(known_(A, Attr, V)),
		assert_known_list(Rest).

	clear_known :-
		retractall(known_(_, _, _)).

	% ==========================================================================
	% 2025-06-18 Bird identification logic (synchronous elicitation)
	% ==========================================================================

	identify(Elicit, Result) :-
		retractall(known_(_, _, _)),
		(	order::leaf(Bird),
			check(Elicit, Bird) ->
			bird_name(Bird, Name),
			atom_concat('Identified bird: ', Name, Text),
			Result = structured([text(Text)], {})
		;	Result = structured([text('No bird could be identified from the given characteristics.')], {})
		).

	check(Elicit, Bird) :-
		forall(
			(order::descriptor(Name/Arity), functor(Predicate, Name, Arity), Bird::Predicate),
			ask_descriptor(Elicit, Predicate)
		).

	ask_descriptor(Elicit, Predicate) :-
		Predicate =.. [Attribute, Value],
		(	menu_attribute(Attribute, Menu) ->
			menuask(Elicit, Attribute, Value, Menu)
		;	ask(Elicit, Attribute, Value)
		).

	menu_attribute(flight, [ponderous, powerful, agile, flap_glide, other]).
	menu_attribute(flight_profile, [flat, v_shaped, other]).
	menu_attribute(size, [large, plump, medium, small]).
	menu_attribute(tail, [narrow_at_tip, forked, long_rusty, square, other]).

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
		;	asserta(known_(no, Attribute, Value)),
			fail
		).

	:- meta_predicate(ask_question(3, *, *, *)).

	ask_question(Elicit, Attribute, Value, Answer) :-
		atom_concat(Attribute, ': ', Temp1),
		atom_concat(Temp1, Value, Temp2),
		atom_concat(Temp2, '?', Message),
		Schema = {
			type-object,
			properties-{
				answer-{type-string, enum-[yes, no]}
			},
			required-[answer]
		},
		call(Elicit, Message, Schema, Answer).

	:- meta_predicate(menuask(3, *, *, *)).

	menuask(_Elicit, Attribute, Value, _Menu) :-
		known_(yes, Attribute, Value),
		!.
	menuask(_Elicit, Attribute, _, _Menu) :-
		known_(yes, Attribute, _),
		!, fail.
	menuask(Elicit, Attribute, AskValue, Menu) :-
		atom_concat('What is the value for ', Attribute, Temp),
		atom_concat(Temp, '?', Message),
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
		;	fail
		).

	% ==========================================================================
	% Auxiliary predicates
	% ==========================================================================

	bird_name(Bird, Name) :-
		write_to_atom(Bird, Name).

	atoms_to_enum([], []).
	atoms_to_enum([Atom| Rest], [Atom| EnumRest]) :-
		atoms_to_enum(Rest, EnumRest).

	has_pair({Pairs}, Key, Value) :-
		curly_member(Key-Value, Pairs).

	curly_member(Pair, (Pair, _)) :-
		!.
	curly_member(Pair, (_, Rest)) :-
		!,
		curly_member(Pair, Rest).
	curly_member(Pair, Pair).

:- end_object.
