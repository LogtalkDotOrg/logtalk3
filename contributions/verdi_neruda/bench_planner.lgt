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


:- object(bench_planner,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for planning in a blocks world.'
	]).

	member(X, [X|_]) if true.
	member(X, [_|Xs]) if
		member(X, Xs).

	transform(State1,State2,Plan) if
		transform(State1,State2, [State1], Plan).

	transform(State,State,_,[]) if true.
	transform(State1,State2,Visited,[Action|Actions]) if
	   legal_action(Action,State1) and
	   update(Action,State1,State) and
	   not(member(State,Visited)) and
	   transform(State,State2,[State|Visited],Actions).

	legal_action(to_place(Block,Y,Place),State) if
	   on(Block,Y,State) and
	   clear(Block,State) and
	   place(Place) and
	   clear(Place,State).
	legal_action(to_block(Block1,Y,Block2),State) if
	   on(Block1,Y,State) and
	   clear(Block1,State) and
	   block(Block2) and
	   {Block1 \== Block2} and
	   clear(Block2,State).

	clear(X,State) if not(above(X, State)).
	above(X, State) if member(on(_, X), State).
	on(X,Y,State) if member(on(X,Y),State).

	update(to_block(X,Y,Z),State,State1) if
	   substitute(on(X,Y), on(X,Z),State,State1).
	update(to_place(X,Y,Z),State,State1) if
	   substitute(on(X,Y),on(X,Z),State,State1).

	substitute(X,Y,[X|Xs],[Y|Xs]) if true.
	substitute(X,Y,[X1|Xs],[X1|Ys]) if
		{X \== X1} and
		substitute(X,Y,Xs,Ys).

	block(a) if true. block(b) if true. block(c) if true.

	place(p) if true. place(q) if true. place(r) if true.

	initial_state(test, [on(a, b), on(b, p), on(c, r)]).
	final_state(test, [on(a, b), on(b, c), on(c, r)]).

	bench_goal(transform(I, F, Plan)) :-
		initial_state(Name, I),
		final_state(Name, F).

:- end_object.
