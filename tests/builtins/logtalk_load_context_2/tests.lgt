
:- object(hook,
	implements(expanding)).

	:- public(result/2).
	:- dynamic(result/2).

	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(source, Path),
		assertz(result(source, Path)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(file, File),
		assertz(result(file, File)),
		fail.
	term_expansion((:- end_object), (:- end_object)) :-
		logtalk_load_context(directory, Directory),
		assertz(result(directory, Directory)),
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
		date is 2012/12/05,
		comment is 'Unit tests for the logtalk_load_context/2 built-in predicate.'
	]).

	test(logtalk_load_context_2_1) :-
		\+ logtalk_load_context(_, _).

	test(logtalk_load_context_2_2) :-
		this(This),
		object_property(This, file(_, Directory)),
		atom_concat(Directory, 'sample.lgt', Source),
		logtalk_load(Source, [hook(hook)]),
		hook::result(source, Source0), Source0 == Source,
		hook::result(file, File), File == 'sample.lgt',
		hook::result(directory, Directory0), Directory0 == Directory,
		hook::result(entity_identifier, EntityIdentifier), EntityIdentifier == sample,
		hook::result(entity_prefix, EntityPrefix), logtalk::entity_prefix(sample, EntityPrefix),
		hook::result(entity_type, EntityType), EntityType == object,
		hook::result(term_position, TermPosition), ground(TermPosition),
		hook::result(stream, Stream), ground(Stream).

:- end_object.
