%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
		version is 1:5:0,
		author is 'Parker Jones and Paulo Moura',
		date is 2020-12-24,
		comment is 'Unit tests for the "mi" example.'
	]).

	:- uses(lgtunit, [
		op(700, xfx, '=~='), '=~='/2,
		assertion/2, assertion/1,
		variant/2
	]).

	cover(xyz).
	cover(t).
	cover(xyzt).
	cover(xyz(_, _, _)).
	cover(t(_)).
	cover(xyzt(_, _, _, _)).

	test(mi_01, true) :-
		xyzt::rotate(1, 2, 3),
		xyzt::translate(4),
		xyzt::xyzt(X, Y, Z, T),
		assertion(t, T == 4),
		assertion(x, X == 1),
		assertion(y, Y == 2),
		assertion(z, Z == 3).

	test(mi_02, true) :-
		findall(
			Pred-Arity-Object-Functor,
			(	xyzt::current_predicate(Functor/Arity),
				functor(Pred, Functor, Arity),
				xyzt::predicate_property(Pred, declared_in(Object))
			),
			Solutions
		),
		list::msort(Solutions,SolutionsSorted),
		assertion(variant(
			SolutionsSorted,
			[
				t(_)-1-t-t,
				translate(_)-1-t-translate,
				rotate(_,_,_)-3-xyz-rotate,
				xyz(_,_,_)-3-xyz-xyz,
				xyzt(_,_,_,_)-4-xyzt-xyzt
			]
		)).

	test(mi_03, true(Distance =~= 5.385164807134504)) :-
		xyzt(2,3,4,7)::distance(Distance).

	test(mi_04, true(Time == 7)) :-
		xyzt(2,3,4,7)::time(Time).

	test(mi_05, variant(SolutionsSorted, [distance(_)-1-xyz(_,_,_)-distance, time(_)-1-t(_)-time])) :-
		findall(
			Pred-Arity-Object-Functor,
			(	xyzt(2,3,4,7)::current_predicate(Functor/Arity),
				functor(Pred, Functor, Arity),
				xyzt(2,3,4,7)::predicate_property(Pred, declared_in(Object))
			),
			Solutions
		),
		list::msort(Solutions,SolutionsSorted).

:- end_object.
