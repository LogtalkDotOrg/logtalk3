%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>
%  Copyright 1998-2015 Paulo Moura <pmoura@logtalk.org>
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


:- object(secondary).

	:- multifile(primary::p/3).
	primary::p(Priority, Associativity, Operator) :-
		% the next call must use the "secondary" object
		% database, not the "primary" object database 
		current_op(Priority, Associativity, Operator).

	% scoped operarors; seen by the reflection built-in methods
	:- public(op(601, xfx, op_public)).
	:- protected(op(601, xfx, op_protected)).
	:- private(op(601, xfx, op_private)).

	% local operator; invisible to the reflection built-in methods
	:- op(601, xfx, op_local).

:- end_object.
