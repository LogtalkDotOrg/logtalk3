%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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

	:- private(cv_/1).
	:- dynamic(cv_/1).
	:- mode(cv_(?integer), zero_or_one).

	:- public(cv/1).
	:- mode(cv(?integer), zero_or_one).

	:- public(set_cv/1).
	:- mode(set_cv(+integer), one).

	% cv_/1 value is stored locally, in this class
	cv_(0).

	cv(Value) :-
		% retrive cv_/1 value, shared for all instances
		cv_(Value).

	set_cv(Value) :-
		% retract old cv_/1 value from this class
		retractall(cv_(_)),
		% assert the new value into this class
		asserta(cv_(Value)).

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
