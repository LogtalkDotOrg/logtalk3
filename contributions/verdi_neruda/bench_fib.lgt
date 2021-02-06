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


:- object(bench_fib,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for playing with Fibonacci numbers.'
	]).

	%%Calculates the n:th Fibonacci number recursively.

	fib_rec(1, 1) if true.
	fib_rec(2, 1) if true.
	fib_rec(N, X) if
		{N > 2} and
		{N1 is N - 1} and
		{N2 is N - 2} and
		fib_rec(N1, X1) and
		fib_rec(N2, X2) and
		{X is X1 + X2}.

	%%Calculates the n:th Fibonacci number iteratively.

	fib_iter(1, 1) if true.
	fib_iter(2, 1) if true.
	fib_iter(N, X) if
		{N > 2} and
		fib_iter(2, N, 1, 1, X).

	fib_iter(N, N, X, _, X) if true.
	fib_iter(N0, N, X2, X1, X) if
		{N0 < N} and
		{N1 is N0 + 1} and
		{X3 is X2 + X1} and
		fib_iter(N1, N, X3, X2, X).

	bench_goal(fib_rec(5, 5)).
	bench_goal(fib_rec(10, 55)).

	bench_goal(fib_iter(5, 5)).
	bench_goal(fib_iter(10, 55)).

:- end_object.
