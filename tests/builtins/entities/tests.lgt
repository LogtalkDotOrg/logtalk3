
:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2011/05/04,
		comment is 'Unit tests for the entity built-in methods.']).

	:- discontiguous(succeeds/1).
	:- discontiguous(fails/1).
	:- discontiguous(throws/2).

	% current_object/1 tests

	throws(current_object_1_1, error(type_error(object_identifier, 1), logtalk(current_object(1), _))) :-
		current_object(1).

	succeeds(current_object_1_2) :-
		current_object(logtalk).

	fails(current_object_1_3) :-
		current_object(non_exisiting_object).

	% current_protocol/1 tests

	throws(current_protocol_1_1, error(type_error(protocol_identifier, 1), logtalk(current_protocol(1), _))) :-
		current_protocol(1).

	succeeds(current_protocol_1_2) :-
		current_protocol(monitoring).

	fails(current_protocol_1_3) :-
		current_object(non_exisiting_protocol).

	% current_category/1 tests

	throws(current_category_1_1, error(type_error(category_identifier, 1), logtalk(current_category(1), _))) :-
		current_category(1).

	fails(current_category_2_2) :-
		current_category(non_exisiting_category).

	% object_property/2 tests

	throws(object_property_2_1, error(type_error(object_identifier, 1), logtalk(object_property(1, static), _))) :-
		object_property(1, static).

	throws(object_property_2_2, error(type_error(callable, 1), logtalk(object_property(logtalk, 1), _))) :-
		object_property(logtalk, 1).

	throws(object_property_2_3, error(domain_error(object_property, foo), logtalk(object_property(logtalk, foo), _))) :-
		object_property(logtalk, foo).

	fails(object_property_2_4) :-
		object_property(non_exisiting_object, _).

	fails(object_property_2_5) :-
		object_property(logtalk, (dynamic)).

	succeeds(object_property_2_6) :-
		findall(Prop, object_property(logtalk, Prop), _).

	% protocol_property/2 tests

	throws(protocol_property_2_1, error(type_error(protocol_identifier, 1), logtalk(protocol_property(1, static), _))) :-
		protocol_property(1, static).

	throws(protocol_property_2_2, error(type_error(callable, 1), logtalk(protocol_property(monitoring, 1), _))) :-
		protocol_property(monitoring, 1).

	throws(protocol_property_2_3, error(domain_error(protocol_property, foo), logtalk(protocol_property(monitoring, foo), _))) :-
		protocol_property(monitoring, foo).

	fails(protocol_property_2_4) :-
		protocol_property(non_exisiting_protocol, _).

	fails(protocol_property_2_5) :-
		protocol_property(monitoring, (dynamic)).

	succeeds(protocol_property_2_6) :-
		findall(Prop, protocol_property(monitoring, Prop), _).

	% category_property/2 tests

	throws(category_property_2_1, error(type_error(category_identifier, 1), logtalk(category_property(1, static), _))) :-
		category_property(1, static).

	throws(category_property_2_2, error(type_error(callable, 1), logtalk(category_property(monitoring, 1), _))) :-
		category_property(monitoring, 1).

	throws(category_property_2_3, error(domain_error(category_property, foo), logtalk(category_property(monitoring, foo), _))) :-
		category_property(monitoring, foo).

	fails(category_property_2_4) :-
		category_property(non_exisiting_category, _).

	fails(category_property_2_5) :-
		category_property(monitoring, (dynamic)).

	succeeds(category_property_2_6) :-
		findall(Prop, category_property(monitoring, Prop), _).

	% built-in entitiy tests

	succeeds(user) :-
		current_object(user),
		object_property(user, built_in),
		object_property(user, static),
		(	current_logtalk_flag(threads, supported) ->
			object_property(user, threaded)
		;	true
		).

	succeeds(logtalk) :-
		current_object(logtalk),
		object_property(logtalk, built_in),
		object_property(logtalk, static),
		(	current_logtalk_flag(threads, supported) ->
			object_property(logtalk, threaded)
		;	true
		),
		implements_protocol(logtalk, expanding),
		implements_protocol(logtalk, monitoring).

	succeeds(debugger) :-
		current_object(debugger),
		object_property(debugger, built_in),
		object_property(debugger, static).

	succeeds(expanding) :-
		current_protocol(expanding),
		protocol_property(expanding, built_in),
		protocol_property(expanding, static).

	succeeds(monitoring) :-
		current_protocol(monitoring),
		protocol_property(monitoring, built_in),
		protocol_property(monitoring, static).

	% create_object/4 tests

	throws(create_object_1, error(instantiation_error, logtalk(create_object(_, _, _, _), _))) :-
		create_object(_, _, _, _).

	throws(create_object_2, error(type_error(object_identifier, 1), logtalk(create_object(1, [], [], []), _))) :-
		create_object(1, [], [], []).

	throws(create_object_3, error(permission_error(modify, object, logtalk), logtalk(create_object(logtalk, [], [], []), _))) :-
		create_object(logtalk, [], [], []).

	throws(create_object_4, error(permission_error(modify, protocol, monitoring), logtalk(create_object(monitoring, [], [], []), _))) :-
		create_object(monitoring, [], [], []).

	throws(create_object_5, error(type_error(list, atom), logtalk(create_object(_, atom, [], []), _))) :-
		create_object(_, atom, [], []).

	throws(create_object_6, error(type_error(list, atom), logtalk(create_object(_, [], atom, []), _))) :-
		create_object(_, [], atom, []).

	throws(create_object_7, error(type_error(list, atom), logtalk(create_object(_, [], [], atom), _))) :-
		create_object(_, [], [], atom).

	% create_category/4 tests

	throws(create_category_1, error(instantiation_error, logtalk(create_category(_, _, _, _), _))) :-
		create_category(_, _, _, _).

	throws(create_category_2, error(type_error(category_identifier, 1), logtalk(create_category(1, [], [], []), _))) :-
		create_category(1, [], [], []).

	throws(create_category_4, error(permission_error(modify, protocol, monitoring), logtalk(create_category(monitoring, [], [], []), _))) :-
		create_category(monitoring, [], [], []).

	throws(create_category_5, error(type_error(list, atom), logtalk(create_category(_, atom, [], []), _))) :-
		create_category(_, atom, [], []).

	throws(create_category_6, error(type_error(list, atom), logtalk(create_category(_, [], atom, []), _))) :-
		create_category(_, [], atom, []).

	throws(create_category_7, error(type_error(list, atom), logtalk(create_category(_, [], [], atom), _))) :-
		create_category(_, [], [], atom).

	% create_protocol/3 tests

	throws(create_protocol_1, error(instantiation_error, logtalk(create_protocol(_, _, _), _))) :-
		create_protocol(_, _, _).

	throws(create_protocol_2, error(type_error(protocol_identifier, 1), logtalk(create_protocol(1, [], []), _))) :-
		create_protocol(1, [], []).

	throws(create_protocol_3, error(permission_error(modify, protocol, monitoring), logtalk(create_protocol(monitoring, [], []), _))) :-
		create_protocol(monitoring, [], []).

	throws(create_protocol_4, error(permission_error(modify, object, logtalk), logtalk(create_protocol(logtalk, [], []), _))) :-
		create_protocol(logtalk, [], []).

	throws(create_protocol_5, error(type_error(list, atom), logtalk(create_protocol(_, atom, []), _))) :-
		create_protocol(_, atom, []).

	throws(create_protocol_6, error(type_error(list, atom), logtalk(create_protocol(_, [], atom), _))) :-
		create_protocol(_, [], atom).

	% abolish_object/1 tests

	throws(abolish_object_1, error(instantiation_error, logtalk(abolish_object(_), _))) :-
		abolish_object(_).

	throws(abolish_object_2, error(type_error(object_identifier, 1), logtalk(abolish_object(1), _))) :-
		abolish_object(1).

	throws(abolish_object_3, error(existence_error(object, non_exisiting_object), logtalk(abolish_object(non_exisiting_object), _))) :-
		abolish_object(non_exisiting_object).

	throws(abolish_object_4, error(permission_error(modify, static_object, logtalk), logtalk(abolish_object(logtalk), _))) :-
		abolish_object(logtalk).

	% abolish_category/1 tests

	throws(abolish_category_1, error(instantiation_error, logtalk(abolish_category(_), _))) :-
		abolish_category(_).

	throws(abolish_category_2, error(type_error(category_identifier, 1), logtalk(abolish_category(1), _))) :-
		abolish_category(1).

	throws(abolish_category_3, error(existence_error(category, non_exisiting_category), logtalk(abolish_category(non_exisiting_category), _))) :-
		abolish_category(non_exisiting_category).

	% abolish_protocol/1 tests

	throws(abolish_protocol_1, error(instantiation_error, logtalk(abolish_protocol(_), _))) :-
		abolish_protocol(_).

	throws(abolish_protocol_2, error(type_error(protocol_identifier, 1), logtalk(abolish_protocol(1), _))) :-
		abolish_protocol(1).

	throws(abolish_protocol_3, error(existence_error(protocol, non_exisiting_protocol), logtalk(abolish_protocol(non_exisiting_protocol), _))) :-
		abolish_protocol(non_exisiting_protocol).

	throws(abolish_protocol_4, error(permission_error(modify, static_protocol, monitoring), logtalk(abolish_protocol(monitoring), _))) :-
		abolish_protocol(monitoring).

	% implements_protocol/2 tests

	throws(implements_protocol_2_1, error(type_error(object_identifier, 1), logtalk(implements_protocol(1, _), _))) :-
		implements_protocol(1, _).

	throws(implements_protocol_2_2, error(type_error(protocol_identifier, 1), logtalk(implements_protocol(_, 1), _))) :-
		implements_protocol(_, 1).

	% implements_protocol/3 tests

	throws(implements_protocol_3_1, error(type_error(object_identifier, 1), logtalk(implements_protocol(1, _, _), _))) :-
		implements_protocol(1, _, _).

	throws(implements_protocol_3_2, error(type_error(protocol_identifier, 1), logtalk(implements_protocol(_, 1, _), _))) :-
		implements_protocol(_, 1, _).

	throws(implements_protocol_3_3, error(type_error(scope, 1), logtalk(implements_protocol(_, _, 1), _))) :-
		implements_protocol(_, _, 1).

	% imports_category/2 tests

	throws(imports_category_2_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _), _))) :-
		imports_category(1, _).

	throws(imports_category_2_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1), _))) :-
		imports_category(_, 1).

	% imports_category/3 tests

	throws(imports_category_3_1, error(type_error(object_identifier, 1), logtalk(imports_category(1, _, _), _))) :-
		imports_category(1, _, _).

	throws(imports_category_3_2, error(type_error(category_identifier, 1), logtalk(imports_category(_, 1, _), _))) :-
		imports_category(_, 1, _).

	throws(imports_category_3_3, error(type_error(scope, 1), logtalk(imports_category(_, _, 1), _))) :-
		imports_category(_, _, 1).

	% instantiates_class/2 tests

	throws(instantiates_class_2_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _), _))) :-
		instantiates_class(1, _).

	throws(instantiates_class_2_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1), _))) :-
		instantiates_class(_, 1).

	% instantiates_class/3 tests

	throws(instantiates_class_3_1, error(type_error(object_identifier, 1), logtalk(instantiates_class(1, _, _), _))) :-
		instantiates_class(1, _, _).

	throws(instantiates_class_3_2, error(type_error(object_identifier, 1), logtalk(instantiates_class(_, 1, _), _))) :-
		instantiates_class(_, 1, _).

	throws(instantiates_class_3_3, error(type_error(scope, 1), logtalk(instantiates_class(_, _, 1), _))) :-
		instantiates_class(_, _, 1).

	% specializes_class/2 tests

	throws(specializes_class_2_1, error(type_error(object_identifier, 1), logtalk(specializes_class(1, _), _))) :-
		specializes_class(1, _).

	throws(specializes_class_2_2, error(type_error(object_identifier, 1), logtalk(specializes_class(_, 1), _))) :-
		specializes_class(_, 1).

	% specializes_class/3 tests

	throws(specializes_class_3_1, error(type_error(object_identifier, 1), logtalk(specializes_class(1, _, _), _))) :-
		specializes_class(1, _, _).

	throws(specializes_class_3_2, error(type_error(object_identifier, 1), logtalk(specializes_class(_, 1, _), _))) :-
		specializes_class(_, 1, _).

	throws(specializes_class_3_3, error(type_error(scope, 1), logtalk(specializes_class(_, _, 1), _))) :-
		specializes_class(_, _, 1).

	% extends_category/2 tests

	throws(extends_category_2_1, error(type_error(category_identifier, 1), logtalk(extends_category(1, _), _))) :-
		extends_category(1, _).

	throws(extends_category_2_2, error(type_error(category_identifier, 1), logtalk(extends_category(_, 1), _))) :-
		extends_category(_, 1).

	% extends_category/3 tests

	throws(extends_category_3_1, error(type_error(category_identifier, 1), logtalk(extends_category(1, _, _), _))) :-
		extends_category(1, _, _).

	throws(extends_category_3_2, error(type_error(category_identifier, 1), logtalk(extends_category(_, 1, _), _))) :-
		extends_category(_, 1, _).

	throws(extends_category_3_3, error(type_error(scope, 1), logtalk(extends_category(_, _, 1), _))) :-
		extends_category(_, _, 1).

	% extends_protocol/2 tests

	throws(extends_protocol_2_1, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(1, _), _))) :-
		extends_protocol(1, _).

	throws(extends_protocol_2_2, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(_, 1), _))) :-
		extends_protocol(_, 1).

	% extends_protocol/3 tests

	throws(extends_protocol_3_1, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(1, _, _), _))) :-
		extends_protocol(1, _, _).

	throws(extends_protocol_3_2, error(type_error(protocol_identifier, 1), logtalk(extends_protocol(_, 1, _), _))) :-
		extends_protocol(_, 1, _).

	throws(extends_protocol_3_3, error(type_error(scope, 1), logtalk(extends_protocol(_, _, 1), _))) :-
		extends_protocol(_, _, 1).

	% extends_object/2 tests

	throws(extends_object_2_1, error(type_error(object_identifier, 1), logtalk(extends_object(1, _), _))) :-
		extends_object(1, _).

	throws(extends_object_2_2, error(type_error(object_identifier, 1), logtalk(extends_object(_, 1), _))) :-
		extends_object(_, 1).

	% extends_object/3 tests

	throws(extends_object_3_1, error(type_error(object_identifier, 1), logtalk(extends_object(1, _, _), _))) :-
		extends_object(1, _, _).

	throws(extends_object_3_2, error(type_error(object_identifier, 1), logtalk(extends_object(_, 1, _), _))) :-
		extends_object(_, 1, _).

	throws(extends_object_3_3, error(type_error(scope, 1), logtalk(extends_object(_, _, 1), _))) :-
		extends_object(_, _, 1).

	% complements_object/2 tests

	throws(complements_object_2_1, error(type_error(category_identifier, 1), logtalk(complements_object(1, _), _))) :-
		complements_object(1, _).

	throws(complements_object_2_2, error(type_error(object_identifier, 1), logtalk(complements_object(_, 1), _))) :-
		complements_object(_, 1).

:- end_object.
