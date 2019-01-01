%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>  
%  
%  Copyright 2016 Metagol authors
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


:- object(graph_reachability,
	extends(metagol)).

%% metagol settings
max_clauses(2).

%% tell metagol to use the BK
prim(edge/2).

%% metarules
metarule([P, Q], ([P, A, B]:-[[Q, A, B]])).
metarule([P, Q], ([P, A, B]:-[[Q, A, C], [P, C, B]])).

%% background knowledge
edge(a, b).
edge(b, c).
edge(c, a).
edge(a, d).

:- public(learn/1).
learn(Clauses) :-
	Pos = [p(a, b), p(a, c), p(a, a)],
	::learn(Pos,[], Prog),
	::pclauses(Prog, Clauses).

:- public(learn/0).
learn :-
	learn(Clauses),
	meta::maplist(::pprint_clause, Clauses).

:- end_object.
