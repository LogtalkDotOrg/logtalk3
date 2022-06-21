%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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
		version is 0:12:0,
		author is 'Paulo Moura',
		date is 2022-06-21,
		comment is 'Hook object for filtering registry loader file contents.'
	]).

	term_expansion((:- initialization(Goal)), (:- initialization(ExpandedGoal))) :-
		ground(Goal),
		expand(Goal, ExpandedGoal).
	% virtual terms
	term_expansion(begin_of_file, begin_of_file).
	term_expansion(end_of_file, end_of_file).
	% suppress anything else
	term_expansion(_, []) :-
		logtalk_load_context(source, Loader),
		logtalk::print_message(error, packs, 'Invalid registry loader file: ~w'+[Loader]).

	expand((Goal, Goals), (ExpandedGoal, ExpandedGoals)) :-
		expand(Goal, ExpandedGoal),
		expand(Goals, ExpandedGoals).
	expand(logtalk_load(Files, _), logtalk_load(Files, [hook(packs_specs_hook), source_data(on)])) :-
		safe_file_paths(Files).
	expand(logtalk_load(Files), logtalk_load(Files, [hook(packs_specs_hook), source_data(on)])) :-
		safe_file_paths(Files).

	safe_file_paths((-)) :-
		!,
		fail.
	safe_file_paths([]) :-
		!.
	safe_file_paths([File| Files]) :-
		!,
		safe_file_path(File),
		safe_file_paths(Files).
	safe_file_paths(File) :-
		safe_file_path(File).

	safe_file_path(File) :-
		atom_chars(File, Chars),
		phrase(safe_file_path, Chars).

	safe_file_path -->
		% reject paths that start with a / or a .
		[Char], {character::is_alphanumeric(Char)},
		valid_chars.

	valid_chars -->
		valid_char,
		valid_chars.
	valid_chars -->
		[].

	valid_char --> ['/'].
	valid_char --> ['.'].
	valid_char --> ['-'].
	valid_char --> ['_'].
	valid_char --> [Char], {character::is_alphanumeric(Char)}.

:- end_object.


:- object(packs_specs_hook,
	implements(expanding)).

	:- info([
		version is 0:12:0,
		author is 'Paulo Moura',
		date is 2022-06-21,
		comment is 'Hook object for filtering registry and pack specification file contents.'
	]).

	% filter directives
	term_expansion((:- Directive), (:- Directive)) :-
		ground(Directive),
		valid_directive(Directive).
	% filter clauses (only facts are accepted)
	term_expansion(Fact, Fact) :-
		ground(Fact),
		valid_fact(Fact).
	% virtual terms
	term_expansion(begin_of_file, begin_of_file).
	term_expansion(end_of_file, end_of_file).
	% suppress anything else
	term_expansion(_, []) :-
		logtalk_load_context(source, Spec),
		logtalk::print_message(error, packs, 'Invalid registry/pack spec file: ~w'+[Spec]).

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
		atom_chars(URL, Chars),
		phrase(safe_url, Chars).

	safe_url -->
		valid_protocol,
		valid_chars.

	valid_protocol -->
		[h,t,t,p,s,':','/','/'].
	valid_protocol -->
		[f,i,l,e,':','/','/'].

	valid_chars -->
		valid_char,
		valid_chars.
	valid_chars -->
		[].

	valid_char --> ['/'].
	valid_char --> ['.'].
	valid_char --> ['-'].
	valid_char --> ['_'].
	valid_char --> [Char], {character::is_alphanumeric(Char)}.

:- end_object.
