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


:- object(bench_isotree,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for testing if two binary trees are isomorphic.'
	]).

	%% Determining whether two binary trees are isomorphic.

	isotree(void, void) if true.

	%Normal version.
	isotree(t(X, L1, R1), t(X, L2, R2)) if
		isotree(L1, L2) and
		isotree(R1, R2).
	isotree(t(X, L1, R1), t(X, L2, R2)) if
		isotree(L1, R2) and
		isotree(R1, L2).

	%%Goals in body swapped.
%	 isotree(t(X, L1, R1), t(X, L2, R2)) if
%		 isotree(R1, R2) and
%		 isotree(L1, L2).
%	 isotree(t(X, L1, R1), t(X, L2, R2)) if
%		 isotree(R1, L2) and
%		 isotree(L1, R2).

	tree1(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(0, t(3, void, void), t(0, void, void)), t(2, t(4, void, void), t(3, t(2, void, void), t(3, void, void)))))).

	tree1_iso(t(2, t(3, t(0, t(2, void, void), t(0, t(0, void, void), t(0, t(0, void, void), t(0, void, void)))), t(1, void, void)), t(0, t(2, t(3, t(2, void, void), t(3, void, void)), t(4, void, void)), t(0, t(0, void, void), t(3, void, void))))).

	tree2(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree2_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(2, void, void)))))).

	tree3(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(4, void, void), t(2, t(2, t(0, void, void), t(4, void, void)), t(4, void, void))), t(1, t(1, t(2, void, void), t(2, void, void)), t(0, t(2, void, void), t(3, void, void)))))).

	tree3_non_iso(t(1, t(3, t(1, void, void), t(3, void, void)), t(1, t(4, t(2, t(4, void, void), t(2, t(4, void, void), t(0, void, void))), t(4, void, void)), t(1, t(0, t(2, void, void), t(3, void, void)), t(1, t(2, void, void), t(x, void, void)))))).

	bench_goal(isotree(T, IsoT)) :- tree1(T), tree1_iso(IsoT).
	bench_goal(isotree(T, IsoT)) :- tree2(T), tree2_iso(IsoT).
	bench_goal(isotree(T, NonIsoT)) :- tree3(T), tree3_non_iso(NonIsoT).

:- end_object.
