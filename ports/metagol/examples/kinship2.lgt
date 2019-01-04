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


:- object(kinship2,
	extends(metagol)).

	%% tell metagol to use the BK
	prim(parent/2).

	%% metarules
	metarule([P,Q],([P,A,B]:-[[Q,A,B]])).
	metarule([P,Q],([P,A,B]:-[[Q,A,C],[P,C,B]])).

	%% background knowledge
	parent(queen_victoria,edward_seventh).
	parent(edward_seventh,george_fifth).
	parent(george_fifth,george_sixth).
	parent(george_sixth,elizabeth_second).
	parent(queen_mother,elizabeth_second).
	parent(elizabeth_second,prince_charles).
	parent(prince_philip,prince_charles).
	parent(prince_charles,prince_william).
	parent(prince_charles,prince_harry).
	parent(princess_diana,prince_william).
	parent(princess_diana,prince_harry).
	parent(prince_william,prince_george).

	:- public(learn/1).
	learn(Clauses) :-
		Pos = [
			ancestor(elizabeth_second,prince_charles),
			ancestor(george_sixth,prince_harry),
			ancestor(queen_mother,prince_william)
		],
		::learn(Pos, [], Prog),
		::pclauses(Prog, Clauses).

	:- public(learn/0).
	learn :-
		learn(Clauses),
		::pprint_clauses(Clauses).

:- end_object.
