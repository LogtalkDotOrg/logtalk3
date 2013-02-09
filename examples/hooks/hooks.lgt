%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(hook,
	implements(expanding)).		% built-in protocol for term and goal expansion methods

	:- info([
		version is 1.31,
		author is 'Paulo Moura',
		date is 2012/08/02,
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
	key_value(license, gpl3, 'GNU General Public License 3.0').

:- end_object.
