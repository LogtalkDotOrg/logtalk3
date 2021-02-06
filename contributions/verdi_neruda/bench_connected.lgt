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


:- object(bench_connected,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database with edges and a traversal predicate.'
	]).

	%%Simple edge/2 collection.
	edge(1, 3) if true.
	edge(3, 5) if true.
	edge(5, 7) if true.
	edge(7, 9) if true.

	edge(0, 2) if true.
	edge(2, 4) if true.
	edge(4, 6) if true.
	edge(6, 8) if true.

	edge(1, 0) if true.
	edge(3, 2) if true.
	edge(5, 4) if true.
	edge(7, 8) if true.
	edge(9, 8) if true.

	edge(0, 3) if true.
	edge(2, 5) if true.
	edge(4, 7) if true.
	edge(6, 9) if true.

	connected(X, Z) if
		edge(X,Y) and
		connected(Y, Z).
	connected(X, Y) if edge(X, Y).

%%These are various permutations of connected/2. Uncomment them if they are of interest.

%	 connected(X, Z) if
%		 connected(Y, Z) and
%		 edge(X,Y).
%	 connected(X, Y) if edge(X, Y).

%	 connected(X, Y) if edge(X, Y).
%	 connected(X, Z) if
%		 edge(X,Y) and
%		 connected(Y, Z).

%	 connected(X, Y) if edge(X, Y).
%	 connected(X, Z) if
%		 connected(Y, Z) and
%		 edge(X,Y).

	bench_goal(connected(1, 2)).
	bench_goal(connected(1, 3)).
	bench_goal(connected(1, 4)).
	bench_goal(connected(1, 5)).
	bench_goal(connected(1, 6)).
	bench_goal(connected(1, 7)).
	bench_goal(connected(1, 8)).
	bench_goal(connected(1, 9)).

:- end_object.
