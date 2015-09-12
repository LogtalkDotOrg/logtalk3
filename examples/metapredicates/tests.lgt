%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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



% entities for testing meta-predicates calling meta-predicates and passing
% closures corresponding to control constructs

:- object(library).

	:- public(my_call/1).
	:- meta_predicate(my_call(0)).
	my_call(Goal) :-
		call(Goal).

	:- public(my_call/2).
	:- meta_predicate(my_call(1, *)).
	my_call(Closure, Arg) :-
		call(Closure, Arg).

	:- public(self_closure/1).
	self_closure(X) :-
		my_call(::q, X).

	:- protected(q/1).
	q(library).

	a(library).

:- end_object.



:- object(extended_library,
	extends(library)).

	q(extended_library).

:- end_object.



:- object(client).

	:- public(test1/1).
	test1(L) :-
		library::my_call(setof(E, a(E), L)).

	:- public(test2/1).
	test2(L) :-
		library::my_call(setof(E, call(a, E), L)).

	:- public(test3/1).
	test3(L) :-
		library::my_call(call(setof, E, call(a, E), L)).

	a(1). a(2). a(3).

:- end_object.



:- object(parent).

	:- public(self_closure/1).
	self_closure(X) :-
		library::my_call(::q, X).

	:- protected(q/1).
	q(parent).

:- end_object.



:- object(proto,
	extends(parent)).

	:- public(super_closure/1).
	super_closure(X) :-
		library::my_call(^^q, X).

	q(proto).

:- end_object.



% entities for testing calling meta-predicates from within categories

:- category(test_category).

	:- public(p/1).
	p(Out) :-
		meta::map(double, [1,2,3], Out).

	double(X, Y) :-
		Y is 2*X.

:- end_category.



:- object(test_object,
	imports(test_category)).

	double(X, Y) :-
		Y is 3*X.

:- end_object.



% the follwing two objects, "m2" and "m1", implement a Logtalk version
% of a Ulrich Neumerkel example posted in the SWI-Prolog mailing list
% on December 7, 2010 on the "Should var/1 be module-aware?" thread

:- object(m2).

	:- public(p/2).
	:- meta_predicate(p(*, 1)).
	p(X, Cont) :-
		call(Cont, g(X)).

	g(m2).

:- end_object.



:- object(m1).

	:- public(r/2).
	r(X, Y) :-
		m2::p(Y, ','(g(X))).

	g(m1).

:- end_object.



:- category(ctg).

	:- private(cp1/2).
	cp1(F, L) :-
		FX =.. [F, X],
		findall(X, FX, L).

	:- private(cp2/2).
	cp2(_, L) :-
		findall(X, a(X), L).

	a(1).
	a(2).
	a(3).

:- end_category.


:- object(obj,
	imports(ctg)).

	:- public(op1d/1).
	op1d(L) :-
		^^cp1(a, L).

	:- public(op2d/1).
	op2d(L) :-
		^^cp2(a, L).

	:- public(op1s/1).
	op1s(L) :-
		::cp1(a, L).

	:- public(op2s/1).
	op2s(L) :-
		::cp2(a, L).

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.5,
		author is 'Parker Jones and Paulo Moura',
		date is 2014/02/25,
		comment is 'Unit tests for the "metapredicates" example.'
	]).

	% This example defines a plain Prolog predicate even_integer/1. As the
	% definition is in the pseudo-object "user", moving the call to another object
	% switches the calling context to that object, so even_integer/1 is no longer
	% found. In order to solve this issue, the definition of even_integer/1 and
	% sum_squares/3 predicates are copied the testing object from "predicates.lgt".

	% some simple predicates to use with library meta-predicates (e.g. fold_left/4
	% and partition/4) compiled as plain Prolog code and thus defined in the "user"
	% pseudo-object:

	sum_squares(X, Y, Z) :-
		Z is X*X + Y*Y.

	even_integer(I) :-
		I mod 2 =:= 0.

	test(metapredicates_1) :-
		sort(user)::sort([3,1,4,2,9], Sorted),
		Sorted == [1, 2, 3, 4, 9].

	test(metapredicates_2) :-
		meta::partition(even_integer, [1,2,3,4,5], Included, Excluded),
		Included == [2, 4], Excluded == [1, 3, 5].

	test(metapredicates_3) :-
		meta::fold_left(sum_squares, 0, [1,2,3], Result),
		Result == 34.

	test(metapredicates_4) :-
		meta::fold_left(atom_concat, 'PREFIX', [abc,def,ghi], Result),
		Result=='PREFIXabcdefghi'.

	test(metapredicates_5) :-
		meta::fold_right(atom_concat, 'SUFIX', [abc,def,ghi], Result),
		Result==abcdefghiSUFIX.

	test(metapredicates_6) :-
		meta::fold_left(predicates::sum, 0, [1,2,3,4,5], Result),
		Result == 15.

	test(metapredicates_7) :-
		meta::fold_left(predicates::product, 1, [1,2,3,4,5], Result),
		Result == 120.

	test(metapredicates_8) :-
		meta::fold_left(predicates::tuple, (0,0), [(1,2),(3,4),(6,4)], Result),
		Result == (10, 10).

	test(metapredicates_9) :-
		meta::scan_left(sum_squares, 0, [1,2,3], Result),
		Result == [0, 1, 5, 34].

	test(metapredicates_10) :-
		meta::scan_right(predicates::sum, 5, [1,2,3,4], Result),
		Result == [15, 14, 12, 9, 5].

	test(metapredicates_11) :-
		meta::map(integer, [1,2,3,4,5]).

	test(metapredicates_12) :-
		meta::map(char_code, [a,b,c,d,e], Codes),
		Codes == [97, 98, 99, 100, 101].

	% tests for calling meta-predicates with other meta-predicates as meta-arguments

	test(metapredicates_13) :-
		client::test1(L),
		L == [1, 2, 3].

	test(metapredicates_14) :-
		client::test2(L),
		L == [1, 2, 3].

	test(metapredicates_15) :-
		client::test3(L),
		L == [1, 2, 3].

	% tests for calling meta-predicates with closure corresponding to control constructs

	test(metapredicates_16) :-
		proto::self_closure(X),
		X == proto.

	test(metapredicates_17) :-
		parent::self_closure(X),
		X == parent.

	test(metapredicates_18) :-
		library::self_closure(X),
		X == library.

	test(metapredicates_19) :-
		extended_library::self_closure(X),
		X == extended_library.

	test(metapredicates_20) :-
		proto::super_closure(X),
		X == parent.

	test(metapredicates_21) :-
		test_object::p(L),
		L == [2, 4, 6].

	test(metapredicates_22) :-
		m1::r(X, Y),
		X == m1, Y == m2.

	test(metapredicates_23) :-
		fibonacci::nth(10, Nth),
		Nth == 55.

	test(metapredicates_24) :-
		company::company(C1),
		company::get_salary(company(C1), S1),
		S1 == 179998.

	test(metapredicates_25) :-
		company::company(C1),
		company::cut_salary(company(C1), C2),
		company::get_salary(C2, S2),
		S2 == 89999.

	test(metapredicates_26) :-
		meta::findall_member(N, [1, 2, 3, 4, 5], (N mod 2 =:= 0), L),
		L == [2, 4].

	test(metapredicates_27) :-
		meta::findall_member(N, [1, 2, 3, 4, 5], (N mod 2 =:= 0), L, [6, 8]),
		L == [2, 4, 6, 8].

	test(metapredicates_28) :-
		obj::op1d(L1d),
		L1d == [1, 2, 3],
		obj::op1s(L1s),
		L1s == [1, 2, 3],
		obj::op2d(L2d),
		L2d == [1, 2, 3],
		obj::op2s(L2s),
		L2s == [1, 2, 3].

	test(metapredicates_29) :-
		wrappers_client::p(L),
		L == [1, 2, 3].

	test(metapredicates_30) :-
		wrappers_client::q(L),
		L == [1, 2, 3].

	test(metapredicates_31) :-
		wrappers_client::r(L),
		L == [2, 1, 3].

	test(metapredicates_32) :-
		wrappers_client::s(L),
		L == [2, 1, 3].

:- end_object.
