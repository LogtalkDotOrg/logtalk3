%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright (c) 2010, Victor Lagerkvist
%  SPDX-License-Identifier: BSD-3-Clause
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this
%    list of conditions and the following disclaimer.
%
%  * Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
%  * Neither the name of the copyright holder nor the names of its
%    contributors may be used to endorse or promote products derived from
%    this software without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
%  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
%  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(bench_puzzle,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for solving the mu-puzzle from GEB.'
	]).

	append([], Ys, Ys) if true.
	append([X|Xs], Ys, [X|Zs]) if
		append(Xs, Ys, Zs).

	theorem(_, [m, i]) if true.
	theorem(_, []) if fail.
	theorem(Depth, R) if
		Depth > 0 and
		D is Depth - 1 and
		theorem(D, S) and
		rules(S, R).

	rules(S, R) if rule1(S, R).
	rules(S, R) if rule2(S, R).
	rules(S, R) if rule3(S, R).
	rules(S, R) if rule4(S, R).

	rule1(S, R) if
		append(X, [i], S) and
		append(X, [i,u], R).

	rule2([m|T], [m|R]) if
		append(T, T, R).

	rule3([], _) if
		fail.

	rule3(R, T) if
		append([i,i,i], S, R),
		append([u], S, T).
	rule3([H|T], [H|R]) if
		rule3(T, R).

	rule4([], _) if
		{fail}.

	rule4(R, T) if
		append([u,u], T, R).

	rule4([H|T], [H|R]) if
		rule4(T, R).

	test_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, i, u]).
	test_non_theorem([m, i, u, i, u, i, u, i, u, i, u, i, u, i, u, x, u]).

	bench_goal(theorem(4, T)) :- test_theorem(T).
	bench_goal(theorem(4, T)) :- test_non_theorem(T).

:- end_object.
