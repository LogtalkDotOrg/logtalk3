%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(registry_loader_hook,
	implements(expanding)).

	:- info([
		version is 0:9:0,
		author is 'Paulo Moura',
		date is 2021-10-15,
		comment is 'Hook object for filtering registry loader file contents.'
	]).

	term_expansion((:- initialization(Goal)), (:- initialization(ExpandedGoal))) :-
		ground(Goal),
		expand(Goal, ExpandedGoal).
	% suppress anything else
	term_expansion(_, []).

	expand((Goal, Goals), (ExpandedGoal, ExpandedGoals)) :-
		expand(Goal, ExpandedGoal),
		expand(Goals, ExpandedGoals).
	expand(logtalk_load(Files, _), logtalk_load(Files, [hook(packs_specs_hook), source_data(on)])) :-
		atom_or_atom_list(Files).
	expand(logtalk_load(Files), logtalk_load(Files, [hook(packs_specs_hook), source_data(on)])) :-
		atom_or_atom_list(Files).

	atom_or_atom_list((-)) :-
		!,
		fail.
	atom_or_atom_list([]) :-
		!.
	atom_or_atom_list([File| Files]) :-
		!,
		atom(File),
		atom_or_atom_list(Files).
	atom_or_atom_list(File) :-
		atom(File).

:- end_object.


:- object(packs_specs_hook,
	implements(expanding)).

	:- info([
		version is 0:9:0,
		author is 'Paulo Moura',
		date is 2021-10-15,
		comment is 'Hook object for filtering registry and pack specification file contents.'
	]).

	% filter directives
	term_expansion((:- Directive), (:- Directive)) :-
		ground(Directive),
		valid_directive(Directive).
	% suppress rules
	term_expansion((_ :- _), []).
	% filter facts
	term_expansion(Fact, Fact) :-
		ground(Fact),
		valid_fact(Fact).
	% suppress anything else
	term_expansion(_, []).

	valid_directive(object(_, implements(registry_protocol))).
	valid_directive(object(_, implements(pack_protocol))).
	valid_directive(info(_)).
	valid_directive(end_object).

	% common spec
	valid_fact(name(Name)) :- atom(Name).
	valid_fact(description(Description)) :- atom(Description).
	valid_fact(home(URL)) :- safe_url(URL).
	% registry spec
	valid_fact(clone(URL)) :- safe_url(URL).
	valid_fact(archive(URL)) :- safe_url(URL).
	% registry spec
	valid_fact(license(License)) :- atom(License).
	valid_fact(version(_, _, URL, _, _, _)) :- safe_url(URL).

	safe_url(URL) :-
		(	sub_atom(URL, 0, _, _, 'https://') ->
			true
		;	sub_atom(URL, 0, _, _, 'file://')
		),
		\+ sub_atom(URL, _, _, _, '?').

:- end_object.
