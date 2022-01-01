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


:- object(blind_search(_Bound_),
	instantiates(class),
	specializes(search_strategy)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-02-07,
		comment is 'Blind search state space strategies.',
		parnames is ['Bound']
	]).

	:- public(bound/1).
	:- mode(bound(?integer), zero_or_one).
	:- info(bound/1, [
		comment is 'Search depth bound.',
		argnames is ['Bound']
	]).

	:- protected(search/4).
	:- mode(search(+object, +nonvar, +integer, -list), zero_or_more).
	:- info(search/4, [
		comment is 'State space search solution.',
		argnames is ['Space', 'State', 'Bound', 'Path']
	]).

	bound(_Bound_).

	solve(Space, State, Path) :-
		::bound(Bound),
		::search(Space, State, Bound, Path).

:- end_object.
