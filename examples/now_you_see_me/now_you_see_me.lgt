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


% start with the four horseman visible on stage
:- initialization(magic::show).


:- object(four_horseman).

	:- protected(horseman/1).
	:- dynamic(horseman/1).
	horseman(danny).
	horseman(merritt).
	horseman(henley).
	horseman(jack).

:- end_object.


:- object(magic,
	extends(four_horseman)).

	:- dynamic(horseman/1).

	:- public(hide/0).
	hide :-
		% asserting this clause overrides the inherited
		% definition for the horseman/1 predicate for
		% this object and for descendant objects
		assertz((horseman(_) :- fail)).

	:- public(show/0).
	show :-
		% retracting this clause restores the visibility of the
		% inherited definition for the horseman/1 predicate for
		% this object and for descendant objects
		retractall(horseman(_)).

:- end_object.


:- object(stage,
	extends(magic)).

	:- public(list/0).
	list :-
		% list all visible horseman; note that, because the
		% horseman/1 predicate is dynamic, static binding is
		% not possible for the ^^/1 call 
		forall(^^horseman(Horseman), (write(Horseman), nl)).

	:- public(all/1).
	all(All) :-
		% return a list with all visible horseman
		findall(Horseman, ^^horseman(Horseman), All).

:- end_object.
