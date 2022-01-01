%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-11-16,
		comment is 'Unit tests for the ISO Prolog standard compound term syntax.'
	]).

	% tests from the Logtalk portability work

	test(lgt_compound_01, true(functor(T,[],2))) :-
		^^set_text_input('[](2,3). '),
		{read(T)}.

	test(lgt_compound_02, true(functor(T,{},2))) :-
		^^set_text_input('{}(2,3). '),
		{read(T)}.

	test(lgt_compound_03, true(functor(T,'.',3))) :-
		^^set_text_input('\'.\'(2,3,5). '),
		{read(T)}.

	cleanup :-
		^^clean_text_input.

:- end_object.
