%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2022 José Antonio Riaza Valverde <riazavalverde@gmail.com>
%  Copyright 2022 Paulo Moura <pmoura@logtalk.org>
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


:- object(kruskal).

	:- public(kruskal/2).

	:- uses(union_find, [
		new/2, find/4, union/4
	]).

	kruskal(g(Vertices-Edges), g(Vertices-Tree)) :-
		new(Vertices, UF),
		keysort(Edges, Sorted),
		kruskal(UF, Sorted, Tree).

	kruskal(_, [], []).
	kruskal(UF0, [Edge|Edges], [Edge|Tree]) :-
		Edge = _-(V1, V2),
		find(UF0, V1, R1, UF1),
		find(UF1, V2, R2, UF2),
		R1 \== R2, !,
		union(UF2, V1, V2, UF3),
		kruskal(UF3, Edges, Tree).
	kruskal(UF, [_|Edges], Tree) :-
		kruskal(UF, Edges, Tree).

:- end_object.
