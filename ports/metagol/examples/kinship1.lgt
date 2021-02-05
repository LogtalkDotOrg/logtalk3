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


:- object(kinship1,
	implements(metagol_example_protocol),
	extends(metagol)).

	%% preds that metagol can use in the body of a clause
	body_pred(mother/2).
	body_pred(father/2).

	body_pred(shoe/1).

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

	%% learn grandparent by inventing parent
	:- public(learn1/1).
	learn1(Clauses) :-
		Pos = [
			grandparent(ann,amelia),
			grandparent(steve,amelia),
			grandparent(ann,spongebob),
			grandparent(steve,spongebob),
			grandparent(linda,amelia)
		],
%%		Neg = [grandparent(amy,amelia)],
		Neg = [],
		^^learn(Pos, Neg, Prog),
		^^program_to_clauses(Prog, Clauses).

	:- public(learn1/0).
	learn1 :-
		learn1(Clauses),
		::pprint_clauses(Clauses).

	%% example of a failure
	:- public(learn2/0).
	learn2 :-
		Pos = [grandparent(ann,amelia)],
		Neg = [grandparent(ann,amelia)],
		(	::learn(Pos, Neg) ->
			false
		;	logtalk::print_message(comment, metagol, @'failed to learn a theory')
		).

:- end_object.
