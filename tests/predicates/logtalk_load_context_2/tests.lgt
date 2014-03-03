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
	implements(expanding)).

	:- public(result/2).
	:- dynamic(result/2).

	term_expansion((:- end_object), _) :-
		logtalk_load_context(source, Path),
		assertz(result(source, Path)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(file, Path),
		assertz(result(file, Path)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(basename, Basename),
		assertz(result(basename, Basename)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(directory, Directory),
		assertz(result(directory, Directory)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(target, PrologFile),
		assertz(result(target, PrologFile)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_identifier, Entity),
		assertz(result(entity_identifier, Entity)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_prefix, Prefix),
		assertz(result(entity_prefix, Prefix)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(entity_type, Type),
		assertz(result(entity_type, Type)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(term_position, Position),
		assertz(result(term_position, Position)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(variable_names, VariableNames),
		assertz(result(variable_names, VariableNames)),
		fail.
	term_expansion((:- end_object), _) :-
		logtalk_load_context(stream, Stream),
		assertz(result(stream, Stream)),
		fail.

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2014/02/08,
		comment is 'Unit tests for the logtalk_load_context/2 built-in predicate.'
	]).

	:- uses(hook, [
		result/2
	]).

	test(logtalk_load_context_2_1) :-
		\+ logtalk_load_context(_, _).

	test(logtalk_load_context_2_2) :-
		this(This),
		object_property(This, file(_, Directory)),
		write(directory-Directory), nl,
		atom_concat(Directory, 'sample.lgt', Source),
		logtalk_load(Source, [hook(hook)]),
		result(source, Source0), Source0 == Source,
		result(file, Source1), Source1 == Source,
		result(basename, Basename), Basename == 'sample.lgt',
		result(directory, Directory0), Directory0 == Directory,
		result(target, PrologFile0), atom(PrologFile0),
		result(entity_identifier, EntityIdentifier), EntityIdentifier == sample,
		result(entity_prefix, EntityPrefix), logtalk::entity_prefix(sample, EntityPrefix),
		result(entity_type, EntityType), EntityType == object,
		result(term_position, TermPosition), ground(TermPosition),
		result(variable_names, VariableNames), variable_names_list(VariableNames),
		result(stream, Stream), ground(Stream).

	variable_names_list((-)) :-
		!,
		fail.
	variable_names_list([]).
	variable_names_list([Pair| Pairs]) :-
		variable_names_pair(Pair),
		variable_names_list(Pairs).

	variable_names_pair((-)) :-
		!,
		fail.
	variable_names_pair(Name = Variable) :-
		atom(Name),
		var(Variable).

:- end_object.
