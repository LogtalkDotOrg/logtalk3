%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2020-2021 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: GPL-2.0-or-later
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-01-30,
		comment is 'Unit tests for the "toychr" port.'
	]).

	cover(toychrdb).

	test(dom_01, true(N == 3)) :-
		dom::chr_is(_, (dom(N, [1,2,3]), dom(N, [3,4,5]))).

	test(gcd_01, true(GCD == gcd(3))) :-
		gcd::chr_is(GCD, (gcd(9), gcd(6))).

	test(leq_01, variant(Result, (leq(X,Z), leq(Y,Z), leq(X,Y)))) :-
		leq::chr_is(Result, (leq(X,Y), leq(Y,Z))).

	test(fib_01, true(N == 8)) :-
		fib::chr_is(_, fib(5,N)).

	test(primes_01, true(Result == (prime(2), prime(3), prime(5), prime(7), prime(11)))) :-
		primes::chr_is(Result, candidate(11)).

:- end_object.
