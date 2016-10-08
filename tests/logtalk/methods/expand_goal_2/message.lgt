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


% no goal_expansion/2 hook predicate defined
:- object(obj_om_01).

:- end_object.


% missing implements(expanding) but with a
% goal_expansion/2 hook predicate definition
:- object(obj_om_02).

	goal_expansion(goal, 'GOAL').

:- end_object.


% public scope for the goal_expansion/2 hook predicate
:- object(obj_om_03,
	implements(expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


% protected scope for the goal_expansion/2 hook predicate
:- object(obj_om_04,
	implements(protected::expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


% private scope for the goal_expansion/2 hook predicate
:- object(obj_om_05,
	implements(private::expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


% public scope for the inherited goal_expansion/2 hook predicate
:- object(obj_om_06_root,
	implements(expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


:- object(obj_om_06,
	extends(obj_om_06_root)).

:- end_object.


% protected scope for the inherited goal_expansion/2 hook predicate
:- object(obj_om_07_root,
	implements(protected::expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


:- object(obj_om_07,
	extends(obj_om_07_root)).

:- end_object.


% private scope for the inherited goal_expansion/2 hook predicate
:- object(obj_om_08_root,
	implements(private::expanding)).

	goal_expansion(goal, 'GOAL').

:- end_object.


:- object(obj_om_08,
	extends(obj_om_08_root)).

:- end_object.


% the goal_expansion/2 hook predicate is recursively
% called until a fixed-point is reached
:- object(obj_om_09,
	implements(expanding)).

	goal_expansion(goal0, goal1).
	goal_expansion(goal1, goal2).
	goal_expansion(goal2, goal3).
	goal_expansion(goal3, goal4).
	goal_expansion(goal4, goal).

:- end_object.


% the goal_expansion/2 hook predicate is recursively
% called until a fixed-point is reached
:- object(obj_om_10_root,
	implements(expanding)).

	goal_expansion(goal0, goal1).
	goal_expansion(goal1, goal2).
	goal_expansion(goal2, goal3).
	goal_expansion(goal3, goal4).
	goal_expansion(goal4, goal).

:- end_object.


:- object(obj_om_10,
	extends(obj_om_10_root)).

:- end_object.
