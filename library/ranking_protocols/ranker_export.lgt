%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
%
%  Licensed under the Apache License, Version 2.0 (the "License");
%  you may not use this file except in compliance with the License.
%  You may obtain a copy of the License at
%
%      http://www.apache.org/licenses/LICENSE-2.0
%
%  Unless required by applicable law or agreed to in writing, software
%  distributed under the License is distributed on an "AS IS" BASIS,
%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%  See the License for the specific language governing permissions and
%  limitations under the License.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- category(ranker_export).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-17,
		comment is 'Shared predicates for exporting learned rankers.'
	]).

	:- uses(format, [
		format/3
	]).

	:- public(ranker_to_file/4).
	:- mode(ranker_to_file(+object_identifier, +compound, +callable, +atom), one).
	:- info(ranker_to_file/4, [
		comment is 'Exports a learned ranker to a file using the ranker clauses provided by the importing object.',
		argnames is ['Dataset', 'Ranker', 'Functor', 'File']
	]).

	ranker_to_file(Dataset, Ranker, Functor, File) :-
		::ranker_to_clauses(Dataset, Ranker, Functor, Clauses),
		open(File, write, Stream),
		write_comment_header(Functor, Ranker, Stream),
		write_clauses(Clauses, Stream),
		close(Stream).

	write_comment_header(Functor, Ranker, Stream) :-
		format(Stream, '% exported ranker predicate: ~q~n', [Functor]),
		(   ::diagnostics(Ranker, Diagnostics) ->
			format(Stream, '% diagnostics: ~q~n', [Diagnostics])
		;   true
		).

	write_clauses([], _Stream).
	write_clauses([Clause| Clauses], Stream) :-
		format(Stream, '~q.~n', [Clause]),
		write_clauses(Clauses, Stream).

:- end_category.
