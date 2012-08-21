%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>    
%  
%  Logtalk is free software. You can redistribute it and/or modify it under
%  the terms of the FSF GNU General Public License 3  (plus some additional
%  terms per section 7).        Consult the "LICENSE.txt" file for details.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- protocol(relationp).

	:- info([
		version is 1.0,
		author is 'Paulo Moura',
		date is 2000/7/24,
		comment is 'Relations between objects protocol.']).

	:- private(tuple_/1).
	:- dynamic(tuple_/1).
	:- mode(tuple_(?list), zero_or_more).
	:- info(tuple_/1, [
		comment is 'Stores the relation tuples.',
		argnames is ['Tuple']]).

	:- public(tuple/1).
	:- mode(tuple(?list), zero_or_more).
	:- info(tuple/1, [
		comment is 'Returns a relation tuple.',
		argnames is ['Tuple']]).

	:- public(tuples/1).
	:- mode(tuples(-list), one).
	:- info(tuples/1, [
		comment is 'Returns a list of all relation tuples.',
		argnames is ['Tuples']]).

	:- public(add_tuple/1).
	:- mode(add_tuple(+list), zero_or_one).
	:- info(add_tuple/1, [
		comment is 'Adds a new relation tuple.',
		argnames is ['Tuple']]).

	:- public(remove_tuple/1).
	:- mode(remove_tuple(?list), zero_or_more).
	:- info(remove_tuple/1, [
		comment is 'Removes a matching relation tuple.',
		argnames is ['Tuple']]).

	:- public(remove_all_tuples/0).
	:- mode(remove_all_tuples, one).
	:- info(remove_all_tuples/0, [
		comment is 'Removes all relation tuples.']).

	:- public(number_of_tuples/1).
	:- mode(number_of_tuples(-integer), one).
	:- info(number_of_tuples/1, [
		comment is 'Returns the current number of relation tuples.',
		argnames is ['Number']]).

	:- public(plays_role_in_tuple/3).
	:- mode(plays_role_in_tuple(?object, ?atom, ?list), zero_or_more).
	:- info(plays_role_in_tuple/3, [
		comment is 'List of tuples where an object plays a role.',
		argnames is ['Object', 'Role', 'Tuples']]).

	:- public(plays_roles/2).
	:- mode(plays_roles(?object, -list), zero_or_more).
	:- info(plays_roles/2, [
		comment is 'Returns a list of all roles played by an object in the relation tuples.',
		argnames is ['Object', 'Roles']]).

	:- public(plays_role_n_times/3).
	:- mode(plays_role_n_times(?object, ?atom, -integer), zero_or_more).
	:- info(plays_role_n_times/3, [
		comment is 'Number of times that an object plays a role in the relation tuples.',
		argnames is ['Object', 'Role', 'Times']]).

	:- private(domain_/2).
	:- dynamic(domain_/2).
	:- mode(domain_(?atom, ?object), zero_or_more).
	:- info(domain_/2, [
		comment is 'Table of role domains.',
		argnames is ['Role', 'Domain']]).

	:- public(domains/1).
	:- mode(domains(-list), zero_or_one).
	:- info(domains/1, [
		comment is 'List of domains for all roles.',
		argnames is ['Domains']]).

	:- public(domain/2).
	:- mode(domain(?atom, ?object), zero_or_more).
	:- info(domain/2, [
		comment is 'Role domain.',
		argnames is ['Role', 'Domain']]).

	:- public(set_domains/1).
	:- mode(set_domains(+list), zero_or_one).
	:- info(set_domains/1, [
		comment is 'Set the domains for all roles.',
		argnames is ['Domains']]).

	:- private(descriptor_/1).
	:- dynamic(descriptor_/1).
	:- mode(descriptor_(?list), zero_or_one).
	:- info(descriptor_/1, [
		comment is 'Stores the relation tuple descriptor.',
		argnames is ['Descriptor']]).

	:- public(descriptor/1).
	:- mode(descriptor(?list), zero_or_one).
	:- info(descriptor/1, [
		comment is 'Returns the relation tuple descriptor.',
		argnames is ['Descriptor']]).

	:- public(degree/1).
	:- mode(degree(?integer), zero_or_one).
	:- info(degree/1, [
		comment is 'Descriptor length.',
		argnames is ['Degree']]).

	:- public(set_descriptor/1).
	:- mode(set_descriptor(+list), zero_or_one).
	:- info(set_descriptor/1, [
		comment is 'Sets the relation tuple descriptor.',
		argnames is ['Descriptor']]).

	:- private(key_/1).
	:- dynamic(key_/1).
	:- mode(key_(?list), zero_or_more).
	:- info(key_/1, [
		comment is 'Stores the relation keys.',
		argnames is ['Key']]).

	:- public(key/1).
	:- mode(key(?list), zero_or_more).
	:- info(key/1, [
		comment is 'Returns a relation key.',
		argnames is ['Key']]).

	:- public(keys/1).
	:- mode(keys(-list), one).
	:- info(keys/1, [
		comment is 'Returns a list of all relation keys.',
		argnames is ['Keys']]).

	:- public(set_keys/1).
	:- mode(set_keys(+list), zero_or_one).
	:- info(set_keys/1, [
		comment is 'Sets the relation keys.',
		argnames is ['Keys']]).

	:- private(delete_option_/2).
	:- dynamic(delete_option_/2).
	:- mode(delete_option_(?atom, ?atom), zero_or_more).
	:- info(delete_option_/2, [
		comment is 'Stores role delete options.',
		argnames is ['Role', 'Option']]).

	:- public(delete_options/1).
	:- mode(delete_options(-list), zero_or_one).
	:- info(delete_options/1, [
		comment is 'Returns a list of all role - delete option pairs.',
		argnames is ['Options']]).

	:- public(delete_option/2).
	:- mode(delete_option(?atom, ?atom), zero_or_more).
	:- info(delete_option/2, [
		comment is 'Returns role delete options.',
		argnames is ['Role', 'Option']]).

	:- public(set_delete_options/1).
	:- mode(set_delete_options(+list), zero_or_one).
	:- info(set_delete_options/1, [
		comment is 'Sets the roles delete options.',
		argnames is ['Options']]).

	:- private(cardinality_/3).
	:- dynamic(cardinality_/3).
	:- mode(cardinality_(?atom, ?nonvar, ?nonvar), zero_or_more).
	:- info(cardinality_/3, [
		comment is 'Table of roles minimum and maximum cardinalities.',
		argnames is ['Role', 'Min', 'Max']]).

	:- public(cardinalities/1).
	:- mode(cardinalities(-list), zero_or_one).
	:- info(cardinalities/1, [
		comment is 'List of minimum and maximum cardinalities of all roles.',
		argnames is ['Cardinalities']]).

	:- public(cardinality/3).
	:- mode(cardinality(?atom, ?nonvar, ?nonvar), zero_or_more).
	:- info(cardinality/3, [
		comment is 'Role minimum and maximum cardinalities.',
		argnames is ['Role', 'Min', 'Max']]).

	:- public(set_cardinalities/1).
	:- mode(set_cardinalities(+list), zero_or_one).
	:- info(cardinalities/1, [
		comment is 'Sets the minimum and maximum cardinalities of all roles.',
		argnames is ['Cardinalities']]).

	:- protected(set_monitors/1).
	:- mode(set_monitors(+list), one).

	:- protected(del_all_monitors/0).
	:- mode(del_all_monitors, one).

	:- protected(del_monitors/1).
	:- mode(del_monitors(+list), one).

	:- protected(restore_monitors/0).
	:- mode(restore_monitors, zero_or_one).

:- end_protocol.



:- object(relation,
	implements(relationp, monitoring),
	instantiates(class),
	specializes(object)).

	:- info([
		version is 1.3,
		date is 2006/12/16,
		author is 'Esteban Zimanyi, Paulo Moura',
		comment is 'Enables the representation of relations between independent objects.']).

	:- uses(before_event_registry).
	:- uses(after_event_registry).
	:- uses(list,
		[length/2, member/2, memberchk/2, nth1/3, same_length/2]).

	tuple(Tuple) :-
		::tuple_(Tuple).

	tuples(Tuples) :-
		findall(Tuple, ::tuple_(Tuple), Tuples).

	add_tuple(Tuple) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		\+ same_length(Tuple, Descriptor),
		self(Self),
		sender(Sender),
		throw(error(invalid_length, Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		::key(Key),
		make_tuple_template(Tuple, Descriptor, Key, Template),
		::tuple(Template),
		self(Self),
		sender(Sender),
		throw(error(breaks_key(Key), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::cardinality(Role, _, Maximum),
		::plays_role_n_times(Object, Role, Number),
		Maximum = Number,
		self(Self),
		sender(Sender),
		throw(error(breaks_max_cardinality(Object, Role, Maximum), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::domain(Role, Domain),
		(	Domain::strict_instance ->
			\+ Domain::valid(Object)
		;	\+ Object::ancestor(Domain)
		),
		self(Self),
		sender(Sender),
		throw(error(breaks_domain(Object, Role, Domain), Self::add_tuple(Tuple), Sender)).

	add_tuple(Tuple) :-
		::assertz(tuple_(Tuple)),
		::set_monitors(Tuple).


	make_tuple_template([], [], _, []).
	make_tuple_template([Object| Objects], [Role| Roles], Key, [Var| Rest]) :-
		(	member(Role, Key) ->
			Var = Object
		;	true
		),
	make_tuple_template(Objects, Roles, Key, Rest).

	remove_tuple(Tuple) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::remove_tuple(Tuple), Sender)).

	remove_tuple(Tuple) :-
		::descriptor(Descriptor),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role),
		::cardinality(Role, Minimum, _),
		::plays_role_n_times(Object, Role, Number),
		Minimum = Number,
		self(Self),
		sender(Sender),
		throw(error(breaks_min_cardinality(Object, Role, Minimum), Self::remove_tuple(Tuple), Sender)).

	remove_tuple(Tuple) :-
		::retract(tuple_(Tuple)),
		::del_monitors(Tuple).

	remove_all_tuples :-
		::retractall(tuple_(_)),
		::del_all_monitors.

	number_of_tuples(Number) :-
		findall(1, ::tuple_(_), List),
		length(List, Number).

	plays_roles(Object, Roles) :-
		::descriptor(Descriptor),
		setof(Role,
			Tuple^Position^ (::tuple(Tuple),
                           member(Object, Tuple),
                           nth1(Position, Tuple, Object),
                           once(nth1(Position, Descriptor, Role))),
         Roles).

	plays_role_in_tuple(Object, Role, Tuple) :-
		::descriptor(Descriptor),
		::tuple(Tuple),
		nth1(Position, Tuple, Object),
		nth1(Position, Descriptor, Role).

	plays_role_n_times(Object, Role, Number) :-
		::descriptor(Descriptor),
		nth1(Position, Descriptor, Role),
		setof(Tuple,
			(::tuple(Tuple),
			 nth1(Position, Tuple, Object)), 
			Tuples),
		length(Tuples, Number).

	domains(Domains) :-
		::descriptor(Descriptor),
		domains(Descriptor, Domains).

	domains([], []).
	domains([Role| Roles], [Domain| Domains]) :-
		::domain_(Role, Domain),
		domains(Roles, Domains).

	domain(Role, Domain) :-
		::domain_(Role, Domain).

	set_domains(Domains) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_domains(Domains), Sender)).

	set_domains(Domains) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_domains(Domains), Sender)).

	set_domains(Domains) :-
		::descriptor(Descriptor),
		set_domains(Domains, Descriptor).
		

	set_domains([], []).
	set_domains([Role| Roles], [Domain| Domains]) :-
		::retractall(domain_(Role, _)),
		::assertz(domain_(Role, Domain)),
		set_domains(Roles, Domains).

	descriptor(Descriptor) :-
		::descriptor_(Descriptor).

	degree(Degree) :-
		::descriptor_(Descriptor),
		length(Descriptor, Degree).

	set_descriptor(Descriptor) :-
		\+ ::tuple(_),
		::assertz(descriptor_(Descriptor)),
		::set_keys([Descriptor]),
		set_role_defaults(Descriptor).

	set_role_defaults([]).
	set_role_defaults([Role| Roles]) :-
		::set_domain(Role, object),
		::set_cardinality(Role, 0, n),
		::set_delete_option(Role, cascade),
		set_role_defaults(Roles).

	key(Key) :-
		::key_(Key).

	keys(Keys) :-
		findall(Key, ::key_(Key), Keys).

	set_keys(Keys) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		\+ valid_keys(Keys),
		self(Self),
		sender(Sender),
		throw(error(invalid_key, Self::set_keys(Keys), Sender)).

	set_keys(Keys) :-
		::retractall(key_(_)),
		set_keys2(Keys).
	
	set_keys2([]).
	set_keys2([Key| Keys]) :-
		::assertz(key_(Key)),
		set_keys2(Keys).

	valid_keys(Keys) :-
		::descriptor(Descriptor),
		valid_keys(Keys, Descriptor).

	valid_keys([], _).
	valid_keys([Key| Keys], Descriptor) :-
		forall(
			member(Role, Key),
			memberchk(Role, Descriptor)),
		valid_keys(Keys, Descriptor).

	delete_options(Options) :-
		::descriptor(Descriptor),
		delete_options(Descriptor, Options).

	delete_options([], []).
	delete_options([Role| Roles], [Option| Options]) :-
		::delete_option_(Role, Option),
		delete_options(Roles, Options).

	delete_option(Role, Option) :-
		::delete_option_(Role, Option).

	set_delete_options(Options) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::descriptor(Descriptor),
		\+ same_length(Options, Descriptor),
		self(Self),
		sender(Sender),
		throw(error(invalid_length, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		\+ valid_delete_options(Options),
		self(Self),
		sender(Sender),
		throw(error(invalid_delete_option, Self::set_delete_options(Options), Sender)).

	set_delete_options(Options) :-
		::descriptor(Descriptor),
		set_delete_options(Descriptor, Options).

	set_delete_options([], []).
	set_delete_options([Role| Roles], [Option| Options]) :-
		::retractall(delete_option_(Role, _)),
		::assertz(delete_option_(Role, Option)),
		set_delete_options(Roles, Options).

	valid_delete_options([]).
	valid_delete_options([Option| Options]) :-
		once((Option = restrict; Option = cascade)),
		valid_delete_options(Options).

	cardinalities(Cardinalities) :-
		::descriptor(Descriptor),
		cardinalities(Descriptor, Cardinalities).

	cardinalities([], []).
	cardinalities([Role| Roles], [(Min, Max)| Cardinalities]) :-
		::cardinality_(Role, Min, Max),
		cardinalities(Roles, Cardinalities).

	cardinality(Role, Min, Max) :-
		::cardinality_(Role, Min, Max).

	set_cardinalities(Cardinalities) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		::tuple(_),
		self(Self),
		sender(Sender),
		throw(error(non_empty_relation, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		\+ valid_cardinalities(Cardinalities),
		self(Self),
		sender(Sender),
		throw(error(invalid_cardinality, Self::set_cardinalities(Cardinalities), Sender)).

	set_cardinalities(Cardinalities) :-
		::descriptor(Descriptor),
		set_cardinalities(Cardinalities, Descriptor).

	set_cardinalities([], []).
	set_cardinalities([(Min, Max)| Cardinalities], [Role| Roles]) :-
		::retractall(cardinality_(Role, _, _)),
		::assertz(cardinality_(Role, Min, Max)),
		set_cardinalities(Cardinalities, Roles).

	valid_cardinalities([]).
	valid_cardinalities([Cardinality| Cardinalities]) :-
		nonvar(Cardinality),
		Cardinality = (Min, Max),
		lower_cardinality(Min, Max),
		valid_cardinalities(Cardinalities).

	lower_cardinality(I, n) :-
		integer(I), !.
	lower_cardinality(I, J) :-
		integer(I),
		integer(J),
		I < J. 

	free(Options) :-
		::remove_all_tuples,
		^^free(Options).

	set_monitors([]).
	set_monitors([Object| Objects]) :-
		(	instantiates_class(Object, Class) ->
			self(Self),
			before_event_registry::set_monitor(Class, delete(Object, _), _, Self)
		;	true
		),
		set_monitors(Objects).

	del_monitors([]).
	del_monitors([Object| Objects]) :-
		(	(instantiates_class(Object, Class), \+ (::tuple(Other), member(Object, Other))) ->
			self(Self),
			before_event_registry::del_monitors(Class, delete(Object, _), _, Self)
		;	true
		),
		del_monitors(Objects).

	del_all_monitors :-
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self).

	before(_, delete(Object, Options), _) :-
		!,
		(	(::delete_option(Role, restrict), ::plays_role_in_tuple(Object, Role, Tuple)) ->
			self(Self),
			sender(Sender),
			throw(error(can_not_be_deleted(Tuple, Object, Role), Self::delete(Object, Options), Sender))
		;	forall(
				::plays_role_in_tuple(Object, Role, Tuple),
				::remove_tuple(Tuple))
		).

	before(_, _, _).

	after(_, _, _).

	restore_monitors :-
		self(Self),
		before_event_registry::del_monitors(_, _, _, Self),
		after_event_registry::del_monitors(_, _, _, Self),
		forall(::tuple(Tuple), ::set_monitors(Tuple)).

	print :-
		::descriptor(Descriptor),
		write('descriptor:'), nl, write('  '), writeq(Descriptor), nl,
		::domains(Domains),
		write('domains:'), nl, write('  '), writeq(Domains), nl,
		::cardinalities(Cardinalities),
		write('cardinalities:'), nl, write('  '), writeq(Cardinalities), nl,
		::delete_options(Options),
		write('delete options:'), nl, write('  '), writeq(Options), nl,
		write('keys:'), nl,
		forall(::key(Key), (write('  '), writeq(Key), nl)),
		write('tuples:'), nl,
		forall(::tuple(Tuple), (write('  '), writeq(Tuple), nl)).

:- end_object.



:- object(constrained_relation,
	implements(monitoring),
	instantiates(class),
	specializes(relation)).

	:- info([
		version is 3.3,
		date is 2006/12/16,
		author is 'Paulo Moura',
		comment is 'Enables the representation of relations with constraints on the state of participating objects.']).

	:- uses(list,
		[member/2, memberchk/2, subtract/3]).

	:- private(activ_points_/3).
	:- dynamic(activ_points_/3).
	:- mode(activ_points_(?atom, ?event, -list), zero_or_more).

	:- public(activ_point/3).
	:- mode(activ_point(?atom, ?event, ?callable), zero_or_more).

	:- public(activ_points/3).
	:- mode(activ_points(?atom, ?event, -list), zero_or_more).

	:- public(set_activ_points/3).
	:- mode(set_activ_points(+atom, +event, +list), one).

	:- protected(unique_messages/4).
	:- mode(unique_messages(+list, +atom, +event, -list), one).

	:- protected(propagate/5).
	:- mode(propagate(+atom, +callable, +object, +atom, +list), zero_or_one).

	before(Object, Message, Sender) :-
		self(Self),
		(	Self \= Sender ->
			forall(
				(::activ_point(Role, before, Message),
				 ::plays_role_in_tuple(Object, Role, Tuple)),
				::propagate(before, Message, Object, Role, Tuple))
		;	true
		),
		^^before(Object, Message, Sender).

	after(Object, Message, Sender) :-
		self(Self),
		(	Self \= Sender ->
			forall(
				(::activ_point(Role, after, Message),
				 ::plays_role_in_tuple(Object, Role, Tuple)),
				::propagate(after, Message, Object, Role, Tuple))
		;	true
		),
		^^after(Object, Message, Sender).

	set_monitors(Tuple) :-
		^^set_monitors(Tuple),
		::descriptor(Descriptor),
		set_monitors(Tuple, Descriptor).

	set_monitors([], []).
	set_monitors([Object| Objects], [Role| Roles]) :-
		once(::activ_points(Role, before, Messages1)),	% avoid spurious backtracking
		set_object_before_monitors(Messages1, Object),
		once(::activ_points(Role, after, Messages2)),	% avoid spurious backtracking
		set_object_after_monitors(Messages2, Object),
		set_monitors(Objects, Roles).

	set_object_before_monitors([], _).
	set_object_before_monitors([Message| Messages], Object) :-
		self(Self),
		before_event_registry::set_monitor(Object, Message, _, Self),
		set_object_before_monitors(Messages, Object).

	set_object_after_monitors([], _).
	set_object_after_monitors([Message| Messages], Object) :-
		self(Self),
		after_event_registry::set_monitor(Object, Message, _, Self),
		set_object_after_monitors(Messages, Object).

	del_monitors(Tuple) :-
		^^del_monitors(Tuple),
		::descriptor(Descriptor),
		del_monitors(Tuple, Descriptor). 

	del_monitors([], []).
	del_monitors([Object| Objects], [Role| Roles]) :-
		del_object_monitors(Object, Role),
		del_monitors(Objects, Roles).

	del_object_monitors(Object, Role) :-
		(	::plays_roles(Object, Roles) ->
			(	member(Role, Roles) ->
				true
			;	del_object_monitors(Object, Role, Roles)
			)
		;	del_object_monitors(Object, Role, [])
		).

	del_object_monitors(Object, Role, Roles) :-
		::unique_messages(Roles, Role, before, Messages1),
		del_object_before_monitors(Messages1, Object),
		::unique_messages(Roles, Role, after, Messages2),
		del_object_after_monitors(Messages2, Object).

	del_object_before_monitors([], _).
	del_object_before_monitors([Message| Messages], Object) :-
		self(Self),
		before_event_registry::del_monitors(Object, Message, _, Self),
		del_object_before_monitors(Messages, Object).

	del_object_after_monitors([], _).
	del_object_after_monitors([Message| Messages], Object) :-
		self(Self),
		after_event_registry::del_monitors(Object, Message, _, Self),
		del_object_after_monitors(Messages, Object).

	propagate(Event, Message, Object, Role, Tuple) :-
		self(Self),
		sender(Sender),
		throw(error(desc_responsibility, Self::propagate(Event, Message, Object, Role, Tuple), Sender)).

	activ_point(Role, Event, Message) :-
		::activ_points_(Role, Event, Messages),
		member(Message, Messages).

	activ_points(Role, Event, List) :-
		::activ_points_(Role, Event, List).

	set_activ_points(Role, Event, List) :-
		\+ ::descriptor(_),
		self(Self),
		sender(Sender),
		throw(error(descriptor_not_defined, Self::set_activ_points(Role, Event, List), Sender)).

	set_activ_points(Role, Event, List) :-
		::descriptor(Descriptor),
		memberchk(Role, Descriptor),
		::retractall(activ_points_(Role, Event, _)),
		::assertz(activ_points_(Role, Event, List)).

	unique_messages(Roles, Role, Event, Messages) :-
		::activ_points_(Role, Event, Original),
		filter_messages(Roles, Original, Event, Messages).

	filter_messages([], Messages, _, Messages).
	filter_messages([Role| Roles], Original, Event, Messages) :-
		::activ_points_(Role, Event, Excluded),
		subtract(Original, Excluded, Rest),
		filter_messages(Roles, Rest, Event, Messages).

	set_descriptor(Descriptor) :-
		^^set_descriptor(Descriptor),
		set_default_activ_points(Descriptor).

	set_default_activ_points([]).
	set_default_activ_points([Role| Roles]) :-
		::set_activ_points(Role, before, []),
		::set_activ_points(Role, after, []),
		set_default_activ_points(Roles).

	print :-
		^^print,
		::descriptor(Descriptor),
		write('call activation points:'), nl,
		findall(Messages,
			(member(Role, Descriptor),
             ::activ_points(Role, before, Messages)),
           CallList),
		write('  '), writeq(CallList), nl,
		write('exit activation points:'), nl,
		findall(Messages,
			(member(Role, Descriptor),
			 ::activ_points(Role, after, Messages)),
           ExitList),
		write('  '), writeq(ExitList), nl.

:- end_object.
