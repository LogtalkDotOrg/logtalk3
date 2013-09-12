
:- object(hook,
	implements(expanding)).

	:- public(result/2).
	:- dynamic(result/2).

	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(source, Path),
		assertz(result(source, Path)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(file, Path),
		assertz(result(file, Path)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(basename, Basename),
		assertz(result(basename, Basename)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(directory, Directory),
		assertz(result(directory, Directory)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(target, PrologFile),
		assertz(result(target, PrologFile)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(entity_identifier, Entity),
		assertz(result(entity_identifier, Entity)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(entity_prefix, Prefix),
		assertz(result(entity_prefix, Prefix)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(entity_type, Type),
		assertz(result(entity_type, Type)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(term_position, Position),
		assertz(result(term_position, Position)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(stream, Stream),
		assertz(result(stream, Stream)),
		fail.

:- end_object.



:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/12,
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
		atom_concat(Directory, 'sample.lgt', Source),
		logtalk_load(Source, [hook(hook)]),
		result(source, Source0), Source0 == Source,
		result(file, Source0), Source0 == Source,
		result(basename, Basename), Basename == 'sample.lgt',
		result(directory, Directory0), Directory0 == Directory,
		result(target, PrologFile0), atom(PrologFile0),
		result(entity_identifier, EntityIdentifier), EntityIdentifier == sample,
		result(entity_prefix, EntityPrefix), logtalk::entity_prefix(sample, EntityPrefix),
		result(entity_type, EntityType), EntityType == object,
		result(term_position, TermPosition), ground(TermPosition),
		result(stream, Stream), ground(Stream).

:- end_object.
