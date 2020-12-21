%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2020-12-21,
		comment is 'Unit tests for the "bench" example.'
	]).

	test(boyer_01, true) :-
		boyer::top.

	test(browse_01, true, [condition(predicate_property(length(_,_), built_in))]) :-
		browse::top.

	test(chat_parser_01, true) :-
		chat_parser::top.

	test(crypt_01, true) :-
		crypt::top.

	test(derive_01, true) :-
		derive::top.

	test(divide10_01, true) :-
		divide10::top.

	test(fast_mu_01, true, [condition(predicate_property(length(_,_), built_in))]) :-
		fast_mu::top.

	test(flatten_01, true, [condition(current_logtalk_flag(prolog_dialect, swi))]) :-
		flatten::top.

	test(log10_01, true) :-
		log10::top.

	test(meta_qsort_01, true) :-
		meta_qsort::top.

	test(mu_01, true(Result == [[3,m,u,i,i,u],[3,m,u,i,i,i,i,i],[2,m,i,i,i,i,i,i,i,i],[2,m,i,i,i,i],[2,m,i,i],[a,m,i]])) :-
		mu::theorem([m,u,i,i,u], 5, Result).

	test(nand_01, true, [condition(predicate_property(recorda(_,_,_), built_in))]) :-
		nand::top.

	test(nreverse_01, true(Result == [5,4,3,2,1])) :-
		nreverse::nreverse([1,2,3,4,5], Result).

	test(ops8_01, true) :-
		ops8::top.

	test(perfect_01, true, [condition(current_prolog_flag(bounded, false))]) :-
		perfect::top.

	test(poly_10_01, true) :-
		poly_10::top.

	test(prover_01, true) :-
		prover::top.

	test(qsort_01, true(Result == [1,2,3,4,5,7,9])) :-
		qsort::qsort([3,1,2,7,4,9,5], Result, []).

	test(queens_8_01, true) :-
		queens_8::top.

	test(query_01, true) :-
		query::top.

	test(reducer_01, true(Result == 6)) :-
		reducer::try(fac(3), Result).

	test(reducer_02, true(Result == [1,2,3])) :-
		reducer::try(quick([3,1,2]), Result).

	test(sendmore_01, true) :-
		sendmore::top.

	test(serialise_01, true(Result == [2,3,6,4,1,9,2,8,1,5,1,4,7,4,1,5,1,8,2,9,1,4,6,3,2])) :-
		atom_codes('ABLE WAS I ERE I SAW ELBA', Codes),
		serialise::serialise(Codes, Result).

	test(simple_analyzer_01, true) :-
		simple_analyzer::top.

	test(tak_01, true(Result == 7)) :-
		tak::tak(18, 12, 6, Result).

	test(times10_01, true) :-
		times10::top.

	test(unify_01, true) :-
		unify::top.

	test(zebra_01, true) :-
		zebra::top.

:- end_object.
