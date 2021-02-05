%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  Copyright 2016 Metagol authors
%  Copyright 2018-2019 Paulo Moura
%  All rights reserved.
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


:- set_logtalk_flag(hook, metagol).


:- object(strings1,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% tell metagol to use the BK
	body_pred(copy1/2).
	body_pred(skip1/2).

	%% metarules
	metarule([P,Q],   [P,A,B], [[Q,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,B],[R,A,B]]).
	metarule([P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).
	metarule([P,Q],   [P,A,B], [[Q,A,C],[P,C,B]]).

	%% background knowledge
	copy1([H|RestIn]/[H|RestOut],[H|RestIn]/RestOut).
	skip1([_|RestIn]/Out,RestIn/Out).

	learn(Clauses) :-
		Pos = [
			f(['a','b','c']/['a','a','b','b','c','c'],_/[]),
			f(['a','a','c']/['a','a','a','a','c','c'],_/[]),
			f(['a','c']/['a','a','c','c'],_/[])
		],
		^^learn(Pos, [], Prog),
		^^program_to_clauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
