%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2017 Ebrahim Azarisooreh <ebrahim.azarisooreh@gmail.com> and
%  Paulo Moura <pmoura@logtalk.org>
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


:- object(coupling_metric,
	imports((code_metrics_utilities, code_metric))).

	:- info([
		version is 0.11,
		author is 'Ebrahim Azarisooreh and Paulo Moura',
		date is 2018/07/13,
		comment is 'Computes entity efferent coupling, afferent coupling, and instability.',
		remarks is [
			'Efferent coupling (Ce)' - 'Number of entities that an entity depends on.',
			'Afferent coupling (Ca)' - 'Number of entities that depend on an entity.',
			'Instability (I)' - 'Computed as Ce / (Ce + Ca). Measures the entity resilience to change. Ranging from 0 to 1, with 0 indicating a maximally stable entity and 1 indicating a maximally unstable entity. Ideally, an entity is either maximally stable or maximally unstable.',
			'Abstractness (A)' - 'Computed as the ratio between the number of static predicates with scope directives without and a local definition and the total number of static predicates with scope directives. Measures the rigidity of an entity. Ranging from 0 to 1, with 0 indicating a fully concrete entity and 1 indicating a fully abstract entity.',
			'Entity score' - 'Represented as the compound term ce_ca_i_a(Ce,Ca,I,A).',
			'Dependencies count' - 'Includes direct entity relations plus calls or dynamic updates to predicates in external objects or categories.'
		]
	]).

	:- uses(list, [
		append/2, length/2, member/2, memberchk/2
	]).

	entity_score(Entity, ce_ca_i_a(Efferent,Afferent,Instability,Abstractness)) :-
		(	var(Entity) ->
			^^current_entity(Entity)
		;	true
		),
		^^entity_kind(Entity, Kind),
		efferent_coupling(Kind, Entity, Efferent),
		afferent_coupling(Kind, Entity, Afferent),
		(	Efferent =:= 0 ->
			Instability = 0.0
		;	Instability is float(Efferent / (Efferent + Afferent))
		),
		abstractness(Kind, Entity, Abstractness).

	% efferent coupling

	efferent_coupling(protocol, Entity, Score) :-
		efferent_coupling_protocol(Entity, 0, Score, []).
	efferent_coupling(category, Entity, Score) :-
		efferent_coupling_other(category, Entity, 0, Score, []).
	efferent_coupling(object, Entity, Score) :-
		efferent_coupling_other(object, Entity, 0, Score, []).

	efferent_coupling_protocol(Protocol, Score0, Score, LoggedEntities) :-
		(	unvisited_ancestor(protocol, Protocol, Ancestor, LoggedEntities)
		->	Score1 is Score0 + 1,
			efferent_coupling_protocol(Protocol, Score1, Score, [Ancestor|LoggedEntities])
		;	Score0 = Score
		).

	% measure the coupling scores for objects and categories
	efferent_coupling_other(Kind, Entity, Score0, Score, LoggedEntities) :-
		(	unvisited_ancestor(Kind, Entity, Ancestor, LoggedEntities)
		->	Score1 is Score0 + 1,
			efferent_coupling_other(Kind, Entity, Score1, Score, [Ancestor| LoggedEntities])
		;	unvisited_call(Entity, Entity2, LoggedEntities)
		->	Score1 is Score0 + 1,
			efferent_coupling_other(Kind, Entity, Score1, Score, [Entity2| LoggedEntities])
		;	Score0 = Score
		).

	% afferent coupling

	afferent_coupling(protocol, Entity, Score) :-
		afferent_coupling_protocol(Entity, Score).
	afferent_coupling(category, Entity, Score) :-
		afferent_coupling_category(Entity, Score).
	afferent_coupling(object, Entity, Score) :-
		afferent_coupling_object(Entity, Score).
	
	afferent_coupling_protocol(Protocol, Score) :-
		(	setof(Entity, implements_protocol(Entity,Protocol), Implementers) ->
			length(Implementers, NumberOfImplementers)
		;	NumberOfImplementers = 0
		),
		(	setof(Descendant, extends_protocol(Descendant,Protocol), Descendants) ->
			length(Descendants, NumberOfDescendants)
		;	NumberOfDescendants = 0
		),
		Score is NumberOfImplementers + NumberOfDescendants.

	afferent_coupling_category(Category, Score) :-
		(	setof(Entity, imports_category(Entity,Category), Entities) ->
			length(Entities, NumberOfImporters)
		;	NumberOfImporters = 0
		),
		(	setof(Descendant, extends_category(Descendant,Category), Descendants) ->
			length(Descendants, NumberOfDescendants)
		;	NumberOfDescendants = 0
		),
		(	setof(Object, complements_object(Category, Object), Objects) ->
			length(Objects, NumberOfComplementedObjects)
		;	NumberOfComplementedObjects = 0
		),
		Score is NumberOfImporters + NumberOfDescendants + NumberOfComplementedObjects.

	afferent_coupling_object(Object, Score) :-
		findall(Prototype, extends_object(Prototype,Object), Prototypes),
		findall(Instance, (instantiates_class(Instance,Object), Instance \= Object), Instances),
		findall(Subclass, specializes_class(Subclass,Object), Subclasses),
		findall(Sender, entity_sends_message_to_object(Sender, Object), Senders),
		findall(Updater, entity_updates_object_predicate(Updater, Object), Updaters),
		append([Prototypes,Instances,Subclasses,Senders,Updaters], Entities),
		sort(Entities, SortedEntities),
		length(SortedEntities, Score).

	% abstractness

	abstractness(protocol, _, 1.0).
	abstractness(category, Entity, Abstractness) :-
		findall(
			DeclaredPredicate,
			(	category_property(Entity, declares(DeclaredPredicate, Properties)),
				memberchk(static, Properties)
			),
			DeclaredPredicates
		),
		length(DeclaredPredicates, Declared),
		findall(
			DefinedPredicate,
			(	member(DefinedPredicate, DeclaredPredicates),
				category_property(Entity, defines(DefinedPredicate, _))
			),
			DefinedPredicates
		),
		length(DefinedPredicates, Defined),
		(	Declared =:= 0 ->
			Abstractness is 0.0
		;	Abstractness is float((Declared - Defined) / Declared)
		).
	abstractness(object, Entity, Abstractness) :-
		findall(
			DeclaredPredicate,
			(	object_property(Entity, declares(DeclaredPredicate, Properties)),
				memberchk(static, Properties)
			),
			DeclaredPredicates
		),
		length(DeclaredPredicates, Declared),
		findall(
			DefinedPredicate,
			(	member(DefinedPredicate, DeclaredPredicates),
				object_property(Entity, defines(DefinedPredicate, _))
			),
			DefinedPredicates
		),
		length(DefinedPredicates, Defined),
		(	Declared =:= 0 ->
			Abstractness is 0.0
		;	Abstractness is float((Declared - Defined) / Declared)
		).

	% auxiliary predicates

	% increment the score if there are any direct ancestors
	unvisited_ancestor(EntityKind, Entity, Ancestor, LoggedEntities) :-
		^^ancestor(EntityKind, Entity, _, Ancestor),
		\+ member(Ancestor, LoggedEntities).

	% score any calls external to the entity being measured
	unvisited_call(Entity, Entity2, LoggedEntities) :-
		external_call(Entity, Entity2, _Caller, _Predicate),
		\+ member(Entity2, LoggedEntities),
		!.

	external_call(Entity, Entity2, Caller, Predicate) :-
		^^entity_calls(Entity, Caller, Predicate),
		external_call_(Predicate, Entity, Entity2).

	external_call(Entity, Entity2, Caller, Predicate) :-
		^^entity_updates(Entity, Caller, Predicate),
		external_call_(Predicate, Entity, Entity2).

	external_call_(Obj::_Name/_Arity, Entity, Obj) :-
		Entity \== Obj.

	external_call_(':'(Module,_Name/_Arity), _, Module).

	entity_sends_message_to_object(Entity, Object) :-
		functor(Object, Functor, Arity),
		functor(Template, Functor, Arity),
		(	object_property(Entity, calls(Target::_, _))
		;	category_property(Entity, calls(Target::_, _))
		),
		nonvar(Target),
		Target = Template,
		% but no explict self messages
		Entity \= Object.

	entity_updates_object_predicate(Entity, Object) :-
		functor(Object, Functor, Arity),
		functor(Template, Functor, Arity),
		(	object_property(Entity, updates(Target::_, _))
		;	category_property(Entity, updates(Target::_, _))
		),
		nonvar(Target),
		Target = Template,
		% but no explict self updates
		Entity \= Object.

	entity_score(_Entity, ce_ca_i_a(Efferent,Afferent,Instability,Abstractness)) -->
		['Efferent coupling: ~w'-[Efferent], nl],
		['Afferent coupling: ~w'-[Afferent], nl],
		['Instability: ~w'-[Instability], nl],
		['Abstractness: ~w'-[Abstractness], nl].

:- end_object.
