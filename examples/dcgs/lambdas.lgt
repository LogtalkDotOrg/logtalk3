%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(lambdas).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2018-08-16,
		comment is 'Example using lambda expressions in grammar rules.',
		source is 'Adapted from example posted by Kuniaki Mukai in the SWI-Prolog mailing list.'
	]).

	% silent variables with dual role in lambda expressions warnings
	:- set_logtalk_flag(lambda_variables, silent).

	:- public(aa//1).

	aa([]) --> [].
	aa([X,X|Xs]) --> {X}/[[X|Y],Y]>>true, aa(Xs).

	% note that the definition of the aa//1 non-terminal is just to
	% *exemplify* the use of lambda expressions in grammar rules as
	% the same functionality could be simply implemented as follows:

	:- public(bb//1).

	bb([]) --> [].
	bb([X,X|Xs]) --> [X], bb(Xs).

:- end_object.


:- object(debug).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-05-01,
		comment is 'Example using call//1 and a lambda expressions to access the grammar rule input list for debugging.'
	]).

	:- public(copy/2).
	copy(Original, Copy) :-
		phrase(do_copy(Copy), Original).

	do_copy(All) -->
		list(List), {writeq(List), nl},
		copy(All).

	copy([H| T]) -->
		[H], list(List), {writeq(List), nl}, copy(T).
	copy([]) -->
		[].

	list(List) --> call({List}/[List,List]>>true).

	% we could also have used
	%
	% list(List, List, List).
	%
	% but this simple predicate definition breaks DCGs abstraction

:- end_object.
