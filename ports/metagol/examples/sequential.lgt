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


:- object(sequential,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% tell metagol to use BK
	body_pred(mother/2).
	body_pred(father/2).

	%% metarules
	metarule(ident, [P,Q], [P,A,B], [[Q,A,B]]).
	metarule(chain, [P,Q,R], [P,A,B], [[Q,A,C],[R,C,B]]).

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
	learn(Clauses) :-
		T1 = [
			parent(ann,andy),
			parent(steve,andy),
			parent(ann,amy),
			parent(ann,andy)
		]/[],

		T2 = [
			grandparent(steve,amelia),
			grandparent(ann,amelia),
			grandparent(linda,amelia),
			grandparent(ann,spongebob)
		]/[],

		T3 = [
			great_grandparent(ann,sally),
			great_grandparent(steve,sally)
		]/[],

		::learn_seq([T1,T2,T3], Prog),
		^^program_to_clauses(Prog, Clauses).

	learn :-
		learn(Clauses),
		meta::maplist(::pprint_clause, Clauses).

:- end_object.
