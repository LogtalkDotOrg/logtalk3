%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
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


% The following example is adapted from the following paper where it is used
% to illustrate the Cyclomatic Complexity and Unique Predicate Nodes metrics:
% 
% @article{MOORES199845,
% 	title = "Applying Complexity Measures to Rule-Based Prolog Programs",
% 	journal = "Journal of Systems and Software",
% 	volume = "44",
% 	number = "1",
% 	pages = "45 - 52",
% 	year = "1998",
% 	issn = "0164-1212",
% 	doi = "https://doi.org/10.1016/S0164-1212(98)10042-0",
% 	url = "http://www.sciencedirect.com/science/article/pii/S0164121298100420",
% 	author = "Trevor T Moores"
% }


:- object(expert_system).

	:- public(begin/0).

	:- private(fact/2).
	:- dynamic(fact/2).

	begin :-
		diagnose(X), !, nl,
		write('Your illness may be: '), write(X), nl, nl, clear_facts.
	begin :-
		write('Cannot diagnose your illness.'), nl, nl, clear_facts.

	diagnose('A cold.') :-
		ask('Do you have a runny nose?', runny_nose),
		ask('Do you have a high temperature?', high_temperature).

	ask(X, Y) :-
		write(X), nl,
		read(Reply), sub_atom(Reply, 0, 1, _, 'y'), assertz(fact(true, Y)).
	ask( _, Y) :-
		assertz(fact(false, Y)), fail.

	clear_facts :-
		retract(fact(_, _)), fail.
	clear_facts :- !.

:- end_object.
