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


% code adapted to Logtalk by Paulo Moura from one of the examples
% found on the ECLiPSe 5.10#141 documentation (August 2008)

% 
% ECLiPSe sample code - Steiner triplets
%
% The following program computes so-called Steiner triplets.
% These are triplets of numbers from 1 to N such that any
% two triplets have at most one element in common.
%
% Here is an example of running this program:
% 
% ?- steiner(9,X).
% 
% X = [[1, 2, 3], [1, 4, 5], [1, 6, 7], [1, 8, 9],
%      [2, 4, 6], [2, 5, 8], [2, 7, 9], [3, 4, 9],
%      [3, 5, 7], [3, 6, 8], [4, 7, 8], [5, 6, 9]]
% Yes (2.89s cpu, solution 1, maybe more) ? ;
% 


% the constraint solver libraries must always be loaded prior to compilation of 
% the individual example files:
:- lib(ic).
:- lib(ic_sets).


:- object(steiner).

	:- public(steiner/2).

	:- use_module(ic, [(#=<)/2]).
	:- use_module(ic_sets, [(#)/2, intsets/4, insetdomain/4]).

	:- op(1100, xfy, do).               % ECLiPSe "do" operator is not available when the library(iso) is used

	steiner(N, Sets) :-
		NB is N * (N-1) // 6,           % compute number of triplets
		intsets(Sets, NB, 1, N),        % initialise the set variables
		( foreach(S,Sets) do
		    #(S,3)                      % constrain their cardinality to 3
		),
		( fromto(Sets,[S1|Ss],Ss,[]) do
		    ( foreach(S2,Ss), param(S1) do
		        #(S1 /\ S2, C),         % constrain the cardinality
		        C #=< 1                 % of pairwise intersections to 1
		    )
		),
		label_sets(Sets).               % search

	label_sets([]).
	label_sets([S|Ss]) :-
		insetdomain(S,_,_,_),
		label_sets(Ss).

:- end_object.
