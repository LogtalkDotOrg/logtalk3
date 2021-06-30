%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2021 Paul Brown <pbrown@optimusprime.ai>
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


:- object(tkinter).

	:- info([
		version is 1:0:0,
		author is 'Paul Brown',
		date is 2021-06-28,
		comment is 'A Tk Interface for testing'
	]).

	:- uses(navltree, [as_curly_bracketed/2]).
	:- uses(avltree,  [as_dictionary/2]).
	:- uses(term_io,  [write_term_to_atom/3]).
	:- uses(json,     [generate/2]).

	:- public(go/0).
	:- info(go/0, [
		comment is 'Start the tkinter REPL'
	]).
	go :-
		% Tag so Tcl/Tk knows it can send queries
		write(user_output, 'LOGTALK READY\n'),
		% Flush required for ECLiPSe
		flush_output(user_output),
		repeat,
			catch(try_query, error(Error, _), response(error, Error)),
		fail,
		% Silence linter warning
		!.

	try_query :-
		read_term(user_input, Input, [variable_names(VariableNames)]),
		(	do_query(Input, VariableNames, Response)
		->	json_response(Response)
		;	response(fail, Input, VariableNames)
		).

	:- meta_predicate(do_query(*, *, *)).
	do_query(Input, VariableNames, Response) :-
		write_term_to_atom(Input, Query, [variable_names(VariableNames)]),
		{Input},
		ground_pairs_keys(VariableNames, VariableNames, Variables, Names),
		as_dictionary(Variables, Unifications),
		as_dictionary([
			variable_names-Names,
			status-success,
			query-Query,
			unifications-Unifications
		], Response).

	response(error, ErrorTerm) :-
		write_term_to_atom(ErrorTerm, Error, []),
		as_dictionary([status-error, error-Error], Dict),
		json_response(Dict).

	response(fail, Input, VariableNames) :-
		write_term_to_atom(Input, Query, [variable_names(VariableNames)]),
		as_dictionary([status-fail, query-Query], Dict),
		json_response(Dict).

	json_response(Response) :-
		as_curly_bracketed(Response, JSON),
		generate(stream(user_output), JSON),
		nl(user_output),
		flush_output.

	% In one pass filter any non-ground values, convert from `=` pairs to `-`
	% pairs, and accumulate the pairs keys
	ground_pairs_keys(Pairs, VariableNames, Ground, Keys) :-
		ground_pairs_keys(Pairs, VariableNames, H-H, Ground, I-I, Keys), !.

	ground_pairs_keys([], _, Ground-[], Ground, Keys-[], Keys).
	ground_pairs_keys([K=V|Pairs], VariableNames, GAcc-GH, Ground, KAcc-KH, Keys) :-
		(	nonvar(V)
		->	write_term_to_atom(V, VA, [variable_names(VariableNames)]),
			GH = [K-VA|NGH], KH = [K|NKH]
		;	GH = NGH, KH = NKH
		),
		ground_pairs_keys(Pairs, VariableNames, GAcc-NGH, Ground, KAcc-NKH, Keys).

:- end_object.
