%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(dcgtest).

	:- info([
		version is 1.01,
		date is 2010/03/29,
		author is 'Paulo Moura',
		comment is 'Test cases for the Logtalk DCG translator.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Runs the Logtalk DCG translator on the test cases.'
	]).

	run :-
		write('Testing expand_term/2 predicate ...'), nl, nl,
		gr_tr_test(N, GR, Result),
		write(N), write(':  '), writeq(GR), write('  ---  '),
		write(Result), write(' expected'), nl,
		(	catch(
				expand_term(GR, Clause),
				Error,
				(write('  error: '), write(Error), nl, fail)) ->
			write('  '), writeq(Clause)
		;	write('  expansion failed!')
		),
		nl, nl,
		fail.
	run.

	write_error(Error) :-
		write('  ERROR: '), writeq(Error), nl, fail.

	% terminal tests with list notation:
	gr_tr_test(101, (p --> []), success).
	gr_tr_test(102, (p --> [b]), success).
	gr_tr_test(103, (p --> [abc, xyz]), success).
	gr_tr_test(104, (p --> [abc | xyz]), error).
	gr_tr_test(105, (p --> [[], {}, 3, 3.2, a(b)]), success).
	gr_tr_test(106, (p --> [_]), success).

	% terminal tests with string notation:
	gr_tr_test(151, (p --> "b"), success).
	gr_tr_test(152, (p --> "abc", "q"), success).
	gr_tr_test(153, (p --> "abc" ; "q"), success).

	% simple non-terminal tests:
	gr_tr_test(201, (p --> b), success).
	gr_tr_test(202, (p --> 3), error).
	gr_tr_test(203, (p(X) --> b(X)), success).

	% conjunction tests:
	gr_tr_test(301, (p --> b, c), success).
	gr_tr_test(311, (p --> true, c), success).
	gr_tr_test(312, (p --> fail, c), success).
	gr_tr_test(313, (p(X) --> call(X), c), success).

	% disjunction tests:
	gr_tr_test(351, (p --> b ; c), success).
	gr_tr_test(352, (p --> q ; []), success).
	gr_tr_test(353, (p --> [a] ; [b]), success).

	% if-then-else tests:
	gr_tr_test(401, (p --> b -> c), success).
	gr_tr_test(411, (p --> b -> c; d), success).
	gr_tr_test(421, (p --> b -> c1, c2 ; d), success).
	gr_tr_test(422, (p --> b -> c ; d1, d2), success).
	gr_tr_test(431, (p --> b1, b2 -> c ; d), success).
	gr_tr_test(441, (p --> [x] -> [] ; q), success).

	% negation tests:
	gr_tr_test(451, (p --> \+ b, c), success).
	gr_tr_test(452, (p --> b, \+ c, d), success).

	% cut tests:
	gr_tr_test(501, (p --> !, [a]), success).
	gr_tr_test(502, (p --> b, !, c, d), success).
	gr_tr_test(503, (p --> b, !, c ; d), success).
	gr_tr_test(504, (p --> [a], !, {fail}), success).
	gr_tr_test(505, (p(a), [X] --> !, [X, a], q), success).
	gr_tr_test(506, (p --> a, ! ; b), success).

	% {}/1 tests:
	gr_tr_test(601, (p --> {b}), success).
	gr_tr_test(602, (p --> {3}), error).
	gr_tr_test(603, (p --> {c,d}), success).
	gr_tr_test(604, (p --> '{}'((c,d))), success).
	gr_tr_test(605, (p --> {a}, {b}, {c}), success).
	gr_tr_test(606, (p --> {q} -> [a] ; [b]), success).
	gr_tr_test(607, (p --> {q} -> [] ; b), success).
	gr_tr_test(608, (p --> [foo], {write(x)}, [bar]), success).
	gr_tr_test(609, (p --> [foo], {write(hello)},{nl}), success).
	gr_tr_test(610, (p --> [foo], {write(hello), nl}), success).

	% "metacall" tests:
	:- set_logtalk_flag(singleton_variables, silent).
	gr_tr_test(701, (p --> X), success).
	:- set_logtalk_flag(singleton_variables, warning).
	gr_tr_test(702, (p --> _), success).

	% non-terminals corresponding to "graphic" characters
	% or built-in operators/predicates:
	gr_tr_test(801, ('[' --> b, c), success).
	gr_tr_test(802, ((=) --> b, c), success).

	% pushback tests:
	gr_tr_test(901, (p, [t] --> b, c), success).
	gr_tr_test(902, (p, [t] --> b, [t]), success).
	gr_tr_test(903, (p, [t] --> b, [s, t]), success).
	gr_tr_test(904, (p, [t] --> b, [s], [t]), success).
	gr_tr_test(905, (p(X), [X] --> [X]), success).
	gr_tr_test(906, (p(X, Y), [X, Y] --> [X, Y]), success).
	gr_tr_test(907, (p(a), [X] --> !, [X, a], q), success).
	gr_tr_test(908, (p, [a,b] --> [foo], {write(hello), nl}), success).
	gr_tr_test(909, (p, [t1], [t2] --> b, c), error).
	gr_tr_test(910, (p, b --> b), error).
	gr_tr_test(911, ([t], p --> b), error).
	gr_tr_test(911, ([t1], p, [t2] --> b), error).

:- end_object.
