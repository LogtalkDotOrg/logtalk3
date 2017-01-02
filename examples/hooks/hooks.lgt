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


:- object(hook,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.32,
		author is 'Paulo Moura',
		date is 2015/09/07,
		comment is 'Example of an object defining compiler hook predicates.'
	]).

	% the term_expansion/2 predicate is called for every term in the source file
	% being compiled using this hook object:
	term_expansion((:- info(Original)), [(:- info(New))]) :-
		expand_key_values(Original, New).
	term_expansion((:- local_data(Predicate)), [(:- private(Predicate)), (:- dynamic(Predicate))]).

	% the goal_expansion/2 predicate is called for every goal in predicate clause
	% bodies in the source file being compiled using this hook object:
	goal_expansion(write(Term), (numbervars(Term, 0, _), write_term(Term, [quoted(true), numbervars(true)]))).
	goal_expansion(findall(Term, Goal, List), (setof(Term, Goal, List) -> true; List = [])).

	expand_key_values([], []).
	expand_key_values([Info| Infos], [ExpInfo| ExpInfos]) :-
		(	Info = (Key is Value), key_value(Key, Value, ExpValue) ->
			ExpInfo = (Key is ExpValue)
		;	ExpInfo = Info
		),
		expand_key_values(Infos, ExpInfos).

	key_value(author, pm, 'Paulo Moura, pmoura@logtalk.org').
	key_value(license, apache2, 'Apache License 2.0').

:- end_object.
