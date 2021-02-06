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


:- object(bench_queens,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for solving the 4-queen puzzle.'
	]).

	%%Benchmark 7 - Solving the 4-queen puzzle.

	queens(N,Qs) if
		range(1,N,Ns) and
		queens(Ns,[],Qs).

	queens([],Qs,Qs) if true.
	queens(UnplacedQs,SafeQs,Qs) if
		select(UnplacedQs,UnplacedQs1,Q) and
		not_attack(SafeQs,Q) and
		queens(UnplacedQs1,[Q|SafeQs],Qs).

	not_attack(Xs,X) if
		not_attack(Xs,X,1).

	not_attack([],_,_) if true.
	not_attack([Y|Ys],X,N) if
		{X =\= Y+N} and
		{X =\= Y-N} and
		{N1 is N+1} and
		not_attack(Ys,X,N1).

	select([X|Xs],Xs,X) if true.
	select([Y|Ys],[Y|Zs],X) if select(Ys,Zs,X).

	range(N,N,[N]) if true.
	range(M,N,[M|Ns]) if
		{M < N} and
		{M1 is M+1} and
		range(M1,N,Ns).

	bench_goal(queens(4, [2, 4, 1, 3])).
	bench_goal(queens(4, [2, 4, 3, 1])).

:- end_object.
