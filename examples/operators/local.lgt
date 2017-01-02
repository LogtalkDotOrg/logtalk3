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


% simple example of defining an operator local to a source file


:- op(200, xfx, edge).	% global operator, visible within all
						% entities defined in this source file

:- object(graph).

	:- public(path/3).
	:- public((edge)/2).

	path(Start, End, [Start, End]) :-
		::(Start edge End).
	path(Start, End, [Start| Path]) :-
		::(Start edge Node),
		path(Node, End, Path).

:- end_object.


:- object(graph1,
	extends(graph)).

	a edge b.
	a edge c.
	b edge d.
	c edge d.

:- end_object.


:- object(graph2,
	extends(graph)).

	v1 edge v2.
	v1 edge v3.
	v2 edge v4.
	v3 edge v4.

:- end_object.


:- op(0, xfx, edge).	% "undefine" the operator, effectively
						% making it local to this source file
