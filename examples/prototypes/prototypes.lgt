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


% Alf believes he is the only survivor of his species; no point in
% defining a class if there is only going to be a single instance:

% a prototype, which is also a stand-alone object

:- object(alf).

	% prototypes declare predicates for themselves (and any derived prototypes)
	:- public([
		name/1, planet/1, stomachs/1, favorite_food/1, chases/1, motto/1
	]).

	name('Gordon Shumway').
	planet('Melmac').
	stomachs(8).
	favorite_food(cats).
	chases('Lucky').
	motto('Are you going to finish that sandwich?').

:- end_object.


% later on, Alf finds out that his best friend, Skip, and his
% girlfriend, Rhonda, also survived Melmac's explosion; as they
% are all melmacians, they share most attributes (and add some
% of their own):

% "skip", a derived prototype from "alf", its parent prototype

:- object(skip,
	extends(alf)).

	:- public(best_friend/1).

	best_friend(alf).
	name('Skip').
	% still longing for a nice cat to eat since Melmac exploded
	chases(_) :-
		fail.

:- end_object.


% "rhonda" is also a prototype derived from "alf"

:- object(rhonda,
	extends(alf)).

	:- public(boyfriend/1).

	boyfriend(alf).
	name('Rhonda').
	% still longing for a nice cat to eat since Melmac exploded
	chases(_) :-
		fail.

:- end_object.
