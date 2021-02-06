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


:- object(bench_dcg,
	implements(databasep)).

	:- info([
		version is 1:0:0,
		author is 'Victor Lagerkvist',
		date is 2010-06-13,
		comment is 'A simple database for parsing natural language using a compiled DCG.'
	]).

	sentence(A, C) if
		noun_phrase(A, B) and
		verb_phrase(B, C).

	noun_phrase(A, B) if
		noun_phrase2(A, B).
	noun_phrase(A, C) if
		determiner(A, B) and
		noun_phrase2(B, C).

	verb_phrase(A, C) if
		verb(A, B) and
		noun_phrase(B, C).
	verb_phrase(A, B) if
		verb(A, B).

	verb([contains|A], A) if true.
	verb([eats|A], A) if true.

	noun([pieplate|A], A) if true.
	noun([surprise|A], A) if true.
	noun([man|A], A) if true.

	adjective([decorated|A], A) if true.
	adjective([corpulent|A], A) if true.

	determiner([the|A], A) if true.
	determiner([a|A], A) if true.

	noun_phrase2(A, C) if
		adjective(A, B) and
		noun_phrase2(B, C).
	noun_phrase2(A, B) if
		noun(A, B).

	bench_goal(sentence([the, corpulent, man, contains, a, decorated, pieplate], [])).
	bench_goal(sentence([the, corpulent, man, contains, a, decorated, platepie], [])).

:- end_object.
