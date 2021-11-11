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


% test all possible syntaxes for the directive
:- multifile(a/0).
:- multifile((b/1, c/2)).
:- multifile([d/3, e/4]).


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Paulo Moura',
		date is 2021-11-11,
		comment is 'Unit tests for the ISO Prolog standard multifile/1 directive.'
	]).

	test(multifile_1_single) :-
		{predicate_property(a, (multifile))}.

	test(multifile_1_conjunction) :-
		{predicate_property(b(_), (multifile)),
		 predicate_property(c(_,_), (multifile))}.

	test(multifile_1_list) :-
		{predicate_property(d(_,_,_), (multifile)),
		 predicate_property(e(_,_,_,_), (multifile))}.

:- end_object.
