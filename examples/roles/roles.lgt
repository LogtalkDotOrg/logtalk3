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


% an object with no instantiation or specialization relation with another
% object plays the role of a prototype:

:- object(prototype).

	:- public(foo/1).
	foo(1).

:- end_object.


% prototypes can be derived from other prototypes using the "extends"
% relation; the prototypes that are extended play the role of parent
% prototypes:

:- object(parent).

	:- public(foo/1).
	foo(1).

:- end_object.


:- object(descendant,
	extends(parent)).

	:- public(bar/2).
	bar(1,2).

	foo(2).

:- end_object.


% to define objects playing the role of classes and/or instances, an
% object must have at least an instantiation or a specialization
% relation with another object

% a class can be its own metaclass (thus preventing infinite regression):

:- object(superclass,
	instantiates(superclass)).

	:- public(foo/1).
	foo(1).

:- end_object.

% a subclass is role a that an object plays when it specializes another
% object, which plays the role of its superclass:

:- object(subclass,
	specializes(superclass)).

	:- public(bar/2).
	bar(1,2).

:- end_object.

% an object that instantiates another object plays the role of an
% instance while the instantiated object plays the role of its class:

:- object(instance,
	instantiates(subclass)).

	foo(2).

:- end_object.

:- object(empty_instance,
	instantiates(subclass)).

:- end_object.
