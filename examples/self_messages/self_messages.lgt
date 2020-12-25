%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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


:- object(parent).

	:- public(get_local/1).
	get_local(Local) :-
		% call the local/1 predicate in "self"
		::local(Local).

	:- public(get_default/1).
	get_default(Default) :-
		% call the default/1 predicate in "self"
		::default(Default).

	:- public(get_undefined/1).
	get_undefined(Undefined) :-
		% call the undefined/1 predicate in "self"
		::undefined(Undefined).

	:- protected((local)/1).
	local(parent).

	:- protected(default/1).
	default(parent).

	:- protected(undefined/1).

:- end_object.


:- object(prototype,
	extends(parent)).

	% override the inherited definition for this predicate
	local(prototype).

:- end_object.
