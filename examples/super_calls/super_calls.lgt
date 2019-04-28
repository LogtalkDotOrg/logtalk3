%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2019 Paulo Moura <pmoura@logtalk.org>
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


% the first set of objects illustrate that "super" calls preserve "self"

:- object(parent).

	:- public(get_local/1).
	get_local(Local) :-
		::local(Local).

	:- protected((local)/1).
	local(parent).

:- end_object.


:- object(prototype,
	extends(parent)).

	:- public(correct/1).
	correct(Local) :-
		% ^^/2 goals, aka super calls, preserve self
		^^get_local(Local).

	:- public(wrong/1).
	wrong(Local) :-
		% ::/2 goals (necessarily) reset self to the message receiver
		parent::get_local(Local).

	local(prototype).

:- end_object.


% the second set of objects illustrate that "super" calls require static
% binding when the called object is declared dynamic

:- object(top).

	:- public(d/1).
	:- dynamic(d/1).
	d(parent).

:- end_object.


:- object(middle,
	extends(top)).

:- end_object.


:- object(bottom,
	extends(middle)).

	:- public(value/1).
	value(Value) :-
		^^d(Value).

:- end_object.
