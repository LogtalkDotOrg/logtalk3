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


:- op(1200, xfx, if).
:- op(1000, xfy, and).


:- object(magic_expansion(Mode),
	implements(expanding),
	imports(flatting),
	extends(debug_expansion(Mode))).

	:- info([
		version is 1:0:1,
		author is 'Victor Lagerkvist',
		date is 2020-11-11,
		comment is 'Expands rules of the form p if f and g to the more manageable rule(p, [f,g]) and performs magic transformation of clauses.'
	]).

	goal_expansion(Term, Expansion) :-
		^^goal_expansion(Term, Expansion).

	term_expansion((Head if Goals), MagicClauses) :-
		findall(
			rule(MagicHead, MagicBody, NegOrPos),
			magic_clause(Head, Goals, MagicHead, MagicBody, NegOrPos),
			MagicClauses).
%	debug((write('MagicClauses are: '), write(MagicClauses), nl)).

	magic_clause(Head, Goals, MagicHead, MagicBody, NegOrPos) :-
		phrase(::flatten_goals(Goals), Body, []),
		magic::magicise(Head, Body, MagicHead, MagicBody),
		(	list::member(not(_), MagicBody) ->
			NegOrPos = negative
		;	NegOrPos = positive
		).

:- end_object.
