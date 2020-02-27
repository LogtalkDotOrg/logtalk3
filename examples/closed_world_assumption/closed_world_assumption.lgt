%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% objects can contain both predicate declarations and predicate definitions

:- object(house).

	% predicate declarations
	:- public([
		cellar/0, ground/0, attic/0, porch/0,
		shed/0, garage/0, garden/0,
		pleasant/0, practical/0, fun/0
	]).

	% predicate definitions
	ground.
	porch.
	attic.
	garden.

	pleasant :-
		porch,
		garden.

	practical :-
		shed,
		garage.

	fun :-
		pool.

:- end_object.
