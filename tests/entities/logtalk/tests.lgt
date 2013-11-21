%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the `LICENSE.txt` file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2013/09/28,
		comment is 'Unit tests for the "logtalk" built-in object.'
	]).

	test(logtalk_1) :-
		current_object(logtalk),
		object_property(logtalk, built_in).

	test(logtalk_2) :-
		logtalk::entity_prefix(foo, Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		Entity == foo.

	test(logtalk_3) :-
		logtalk::entity_prefix(foo(_), Prefix),
		logtalk::entity_prefix(Entity, Prefix),
		nonvar(Entity),
		Entity = foo(_).

	test(logtalk_4) :-
		logtalk::compile_predicate_heads(bar(_), logtalk, Compiled, _),
		logtalk::decompile_predicate_heads(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		nonvar(Decompiled),
		Decompiled = bar(_).

	test(logtalk_5) :-
		logtalk::compile_predicate_indicators(bar/1, logtalk, Compiled),
		logtalk::decompile_predicate_indicators(Compiled, Entity, Type, Decompiled),
		Entity == logtalk,
		Type == object,
		Decompiled == bar/1.

:- end_object.
