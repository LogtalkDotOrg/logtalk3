%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(root,
	% avoid infinite metaclass regression by
	% making the class its own metaclass
	instantiates(root)).

	:- public(ivar/1).
	:- mode(ivar(?integer), zero_or_one).

	:- public(set_ivar/1).
	:- mode(set_ivar(+integer), one).

	:- private(ivar_/1).
	:- dynamic(ivar_/1).
	:- mode(ivar_(?integer), zero_or_one).

	% default value for ivar_/1, stored locally in the class
	ivar_(0).

	% retrieve ivar_/1 value from "self", i.e. from
	% the instance that received the ivar/1 message
	ivar(Value) :-
		::ivar_(Value).

	set_ivar(Value) :-
		% retract old ivar_/1 from "self"
		::retractall(ivar_(_)),
		% assert the new value into "self"
		::asserta(ivar_(Value)).

:- end_object.


:- object(instance1,
	instantiates(root)).

:- end_object.


:- object(instance2,
	instantiates(root)).

:- end_object.


:- object(instance3,
	instantiates(root)).

:- end_object.
