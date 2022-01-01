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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Lazy initialization design pattern:
%
% https://en.wikipedia.org/wiki/Lazy_initialization


% we start by defining a metaclass with a simple interface for
% lazy creation of required instances, which are accessed by
% using a key

:- object(metaclass,
	instantiates(metaclass)).

	:- public(show_all/0).
	show_all :-
		forall(
			::instance_(Key, _),
			(write(Key), nl)
		).

	:- public(new/2).
	new(Key, Instance) :-
		(	::instance_(Key, Instance) ->
			true
		;	self(Class),
			create_object(Instance, [instantiates(Class)], [], []),
			::assertz(instance_(Key, Instance))
		).

	:- private(instance_/2).
	:- dynamic(instance_/2).

:- end_object.


% an example class using lazy initialization

:- object(fruit,
	instantiates(metaclass)).

:- end_object.
