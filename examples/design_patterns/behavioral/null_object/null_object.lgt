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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Null object design pattern:
%
% https://en.wikipedia.org/wiki/Null_object_pattern


:- object(animal).

	:- public(make_sound/0).

:- end_object.


% descendant objects provide suitable implementations of the inherited
% interface

:- protocol(dog,
	extends(animal)).

	make_sound :-
		write('Woof...'), nl.

:- end_protocol.


:- object(cat,
	implements(animal)).

	make_sound :-
		write('Meowww...'), nl.

:- end_object.


% the null object provide an implementation of the inherited
% interface that does nothing

:- object(null_animal,
	implements(animal)).

	% be silent
	make_sound.

:- end_object.
