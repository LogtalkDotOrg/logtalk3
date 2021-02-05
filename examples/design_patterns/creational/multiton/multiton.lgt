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
% page on the Multiton design pattern:
%
% https://en.wikipedia.org/wiki/Multiton_pattern


% we start by defining a multiton metaclass providing a simple
% interface for multiton classes
%
% this interface uses lazy creation of the named instances
%
% we simplify the example by using the instances identifiers
% as the keys for acessing the dictionary of named instances

:- object(multiton_metaclass,
	instantiates(multiton_metaclass)).

	:- public(instances/1).
	instances(Instances) :-
		findall(Instance, ::instance_(Instance), Instances).

	:- public(instance/1).
	instance(Instance) :-
		self(Self),
		::instance_(Instance),
		(	current_object(Instance) ->
			true
		;	create_object(Instance, [instantiates(Self)], [], [])
		).

	:- private(instance_/1).

:- end_object.


% a multiton class only needs to define the keys for the
% named instances (which in this simplified example are
% the same as the instance identifiers)

:- object(multiton,
	instantiates(multiton_metaclass)).

	instance_(i1).
	instance_(i2).
	instance_(i3).

:- end_object.
