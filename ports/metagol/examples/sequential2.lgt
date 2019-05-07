%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions
%  are met:
%
%  1. Redistributions of source code must retain the above copyright
%     notice, this list of conditions and the following disclaimer.
%
%  2. Redistributions in binary form must reproduce the above copyright
%     notice, this list of conditions and the following disclaimer in
%     the documentation and/or other materials provided with the
%     distribution.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%  FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%  COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%  BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%  POSSIBILITY OF SUCH DAMAGE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- set_logtalk_flag(hook, metagol).


:- object(sequential2,
	extends(metagol)).

	%% tell metagol to use BK
	body_pred(mother/2).
	body_pred(father/2).

	%% metarules
	metarule([P,Q],   [P,A,B], [[Q,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

	%% background knowledge
	mother(ann,amy).
	mother(ann,andy).
	mother(amy,amelia).
	mother(linda,gavin).
	father(steve,amy).
	father(steve,andy).
	father(gavin,amelia).
	father(andy,spongebob).
	father(spongebob,sally).

	%% learn parent, then grandparent, then great-grandparent
	:- public(learn/0).
	learn :-
		Pos1 = [
			parent(ann,andy),
			parent(steve,andy),
			parent(ann,amy),
			parent(ann,andy)
		],
		::learn(Pos1,[],Prog1),
		::pprint(Prog1),
		::assert_prim(Prog1),
		::assert_program(Prog1),

		Pos2 = [
			grandparent(steve,amelia),
			grandparent(ann,amelia),
			grandparent(linda,amelia),
			grandparent(ann,spongebob)
		],
		::learn(Pos2,[],Prog2),
		::pprint(Prog2),
		::assert_prim(Prog2),
		::assert_program(Prog2),

		Pos3 = [
			great_grandparent(ann,sally),
			great_grandparent(steve,sally)
		],

		::learn(Pos3, [], Prog3),
		::pprint(Prog3).

:- end_object.
