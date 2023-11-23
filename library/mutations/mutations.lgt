%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2023 Paulo Moura <pmoura@logtalk.org>
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


:- category(mutations,
	complements(type)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-11-23,
		comment is 'Adds mutations support to the library ``type`` object.'
	]).

	:- public(mutation/3).
	:- mode(mutation(@callable, @term, -term), one).
	:- info(mutation/3, [
		comment is 'Returns a random mutation of a term into another term of the same type. The input ``Term`` is assume to be valid for the given ``Type``.',
		argnames is ['Type', 'Term', 'Mutation']
	]).

	mutation(Type, Term, Mutation) :-
		mutations_store::mutation(Type, Term, Mutation).

:- end_category.
