%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%
%  SPDX-FileCopyrightText: 2018-2019 Paulo Moura
%  SPDX-FileCopyrightText: 2016 Metagol authors
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


:- object(son,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% tell Metagol to use the BK
	body_pred(father/2).
	body_pred(brother/2).
	body_pred(sister/2).

	%% metarules
	metarule([P,Q,R], [P,A,B], [[Q,B,A],[R,A]]).
	metarule([P,Q,_], [P,A],   [[Q,A,_]]).

	%% background knowledge
	father(a,b).
	father(a,c).
	father(d,e).
	father(d,f).
	father(g,h).
	father(g,i).
	brother(b,c).
	brother(c,b).
	brother(e,f).
	sister(f,c).
	sister(h,i).
	sister(i,h).

	learn(Clauses) :-
		Pos = [
			target(b,a),
			target(c,a),
			target(e,d)
		],
		Neg = [
			target(a,b),
			target(b,c),
			target(c,d),
			target(d,e),
			target(e,f),
			target(f,g),
			target(g,h),
			target(h,i)
		],
		^^learn(Pos, Neg, Prog),
		^^program_to_clauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		^^pprint_clauses(Clauses).

:- end_object.
