%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2017 Paulo Moura <pmoura@logtalk.org>
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


:- object(root,					% avoid infinite metaclass regression by
	instantiates(root)).		% making the class its own metaclass

	:- private(ivar_/1).
	:- dynamic(ivar_/1).
	:- mode(ivar_(?integer), zero_or_one).

	:- public(ivar/1).
	:- mode(ivar(?integer), zero_or_one).

	:- public(set_ivar/1).
	:- mode(set_ivar(+integer), one).

	ivar_(0).					% default value for ivar_/1, stored locally in the class

	ivar(Value) :-				% retrieve ivar_/1 value from "self", i.e. from
		::ivar_(Value).			% the instance that received the ivar/1 message

	set_ivar(Value) :-
		::retractall(ivar_(_)),		% retract old ivar_/1 from "self"
		::asserta(ivar_(Value)).	% assert the new value into "self"

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
