%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


:- object(meta_compiler,
	implements(expanding)).

	:- info([
		version is 0.8,
		date is 2015/12/02,
		author is 'Paulo Moura',
		comment is 'Compiler for the "meta" object meta-predicates. Generates auxiliary predicates in order to avoid meta-call overheads.',
		see_also is [meta]
	]).

	:- uses(list, [append/3, length/2]).
	:- uses(gensym, [gensym/2]).

	:- private(generated_predicate/1).
	:- dynamic(generated_predicate/1).

	term_expansion((:- Directive), [(:- Directive)]) :-
		nonvar(Directive),
		functor(Directive, Functor, Arity),
		Arity >= 1,
		(	Functor == object, Arity =< 5 ->
			true
		;	Functor == category, Arity =< 3
		),
		retractall(generated_predicate(_)).

	term_expansion((:- end_object), [(:- end_object)]) :-
		retractall(generated_predicate(_)).

	term_expansion((:- end_protocol), [(:- end_protocol)]) :-
		retractall(generated_predicate(_)).

	term_expansion((:- end_category), [(:- end_category)]) :-
		retractall(generated_predicate(_)).

	goal_expansion(meta::include(Closure, List, Included), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(include, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([include_(List, Args, Included)], include_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead], GGoal),
			Clauses0 = [
				include_([], _, []),
				(include_([GHead| GTail], GArgs, GResult) :-
					(	GGoal ->
						GResult = [GHead| GRest]
					;	GResult = GRest
					),
					include_(GTail, GArgs, GRest))
				],
			replace_functor([include_(List, Args, Included)| Clauses0], include_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::filter(Closure, List, Included), ExpandedGoal) :-
		goal_expansion(meta::include(Closure, List, Included), ExpandedGoal).

	goal_expansion(meta::exclude(Closure, List, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(exclude, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([exclude_(List, Args, Excluded)], exclude_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead], GGoal),
			Clauses0 = [
				exclude_([], _, []),
				(exclude_([GHead| GTail], GArgs, GResult) :-
					(	GGoal ->
						GResult = GRest
					;	GResult = [GHead| GRest]
					),
					exclude_(GTail, GArgs, GRest))
				],
			replace_functor([exclude_(List, Args, Excluded)| Clauses0], exclude_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::findall_member(Member, List, Test, Result), ExpandedGoal) :-
		decompose_closure(Test, 0, Functor, Arity, Args, GArgs),
		aux_predicate_functor(findall_member_, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([findall_member_(List, Member, Args, Result)], findall_member_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [], GGoal),
			Clauses0 = [
				findall_member_([], _, _, []),
				(findall_member_([GHead| GTail], GMember, GArgs, GResult) :-
					\+ (GHead = GMember, GGoal),
					!,
					findall_member_(GTail, GMember, GArgs, GResult)),
				(findall_member_([GHead| GTail], GMember, GArgs, [GHead| GResult]) :-
					findall_member_(GTail, GMember, GArgs, GResult))
				],
			replace_functor([findall_member_(List, Member, Args, Result)| Clauses0], findall_member_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::findall_member(Member, List, Test, Result, Tail), ExpandedGoal) :-
		decompose_closure(Test, 0, Functor, Arity, Args, GArgs),
		aux_predicate_functor(findall_member_, 5, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([findall_member_(List, Member, Args, Result, Tail)], findall_member_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [], GGoal),
			Clauses0 = [
				findall_member_([], _, _, GResult, GResult),
				(findall_member_([GHead| GTail], GMember, GArgs, GResult0, GResult) :-
					\+ (GHead = GMember, GGoal),
					!,
					findall_member_(GTail, GMember, GArgs, GResult0, GResult)),
				(findall_member_([GHead| GTail], GMember, GArgs, [GHead| GResult0], GResult) :-
					findall_member_(GTail, GMember, GArgs, GResult0, GResult))
				],
			replace_functor([findall_member_(List, Member, Args, Result, Tail)| Clauses0], findall_member_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	goal_expansion(meta::partition(Closure, List, Included, Excluded), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(partition, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([partition_(List, Args, Included, Excluded)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead], GGoal),
			Clauses0 = [
				partition_([], _, [], []),
				(partition_([GHead| GTail], GArgs, GIncluded, GExcluded) :-
					(   GGoal ->
						GIncluded = [GHead| GRestIncluded],
						GExcluded = GRestExcluded
					;	GIncluded = GRestIncluded,
						GExcluded = [GHead| GRestExcluded]
					),
					partition_(GTail, GArgs, GRestIncluded, GRestExcluded))
				],
			replace_functor([partition_(List, Args, Included, Excluded)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::partition(Closure, List, Value, Less, Equal, Greater), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(partition, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/6) ->
			replace_functor([partition_(List, Value, Args, Less, Equal, Greater)], partition_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [Order, X, Y], Goal),
			Clauses0 = [
				partition_([], _, _, [], [], []),
				(partition_([X| Xs], Y, GArgs, RLess, REqual, RGreater) :-
					Goal,
					partition_(Order, X, Xs, Y, GArgs, RLess, REqual, RGreater)),
				(partition_(<, X, Xs, Y, GArgs, [X| RLess], REqual, RGreater) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater)),
				(partition_(=, X, Xs, Y, GArgs, RLess, [X| REqual], RGreater) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater)),
				(partition_(>, X, Xs, Y, GArgs, RLess, REqual, [X| RGreater]) :-
					partition_(Xs, Y, GArgs, RLess, REqual, RGreater))
				],
			replace_functor([partition_(List, Value, Args, Less, Equal, Greater)| Clauses0], partition_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/6))
		).

	goal_expansion(meta::map(Closure, List), ExpandedGoal) :-
		decompose_closure(Closure, 1, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 2, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/2) ->
			replace_functor([map_(List, Args)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead], GGoal),
			Clauses0 = [
					map_([], _),
					(map_([GHead| GTail], GArgs) :-
						GGoal, map_(GTail, GArgs))
				],
			replace_functor([map_(List, Args)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/2))
		).

	goal_expansion(meta::succeeds(Closure, List), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List), ExpandedGoal).

	goal_expansion(meta::maplist(Closure, List), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2), ExpandedGoal) :-
		decompose_closure(Closure, 2, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 3, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/3) ->
			replace_functor([map_(List1, Args, List2)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2], GGoal),
			Clauses0 = [
					map_([], _, []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2]) :-
						GGoal, map_(GTail1, GArgs, GTail2))
				],
			replace_functor([map_(List1, Args, List2)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/3))
		).

	goal_expansion(meta::maplist(Closure, List1, List2), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2, List3), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([map_(List1, Args, List2, List3)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2, GHead3], GGoal),
			Clauses0 = [
					map_([], _, [], []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2], [GHead3| GTail3]) :-
						GGoal, map_(GTail1, GArgs, GTail2, GTail3))
				],
			replace_functor([map_(List1, Args, List2, List3)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::maplist(Closure, List1, List2, List3), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2, List3), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4), ExpandedGoal) :-
		decompose_closure(Closure, 4, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 5, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([map_(List1, Args, List2, List3, List4)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2, GHead3, GHead4], GGoal),
			Clauses0 = [
					map_([], _, [], [], []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2], [GHead3| GTail3], [GHead4| GTail4]) :-
						GGoal, map_(GTail1, GArgs, GTail2, GTail3, GTail4))
				],
			replace_functor([map_(List1, Args, List2, List3, List4)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	goal_expansion(meta::maplist(Closure, List1, List2, List3, List4), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2, List3, List4), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5), ExpandedGoal) :-
		decompose_closure(Closure, 5, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 6, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/6) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2, GHead3, GHead4, GHead5], GGoal),
			Clauses0 = [
					map_([], _, [], [], [], []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2], [GHead3| GTail3], [GHead4| GTail4], [GHead5| GTail5]) :-
						GGoal, map_(GTail1, GArgs, GTail2, GTail3, GTail4, GTail5))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/6))
		).

	goal_expansion(meta::maplist(Closure, List1, List2, List3, List4, List5), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6), ExpandedGoal) :-
		decompose_closure(Closure, 6, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 7, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/7) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2, GHead3, GHead4, GHead5, GHead6], GGoal),
			Clauses0 = [
					map_([], _, [], [], [], [], []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2], [GHead3| GTail3], [GHead4| GTail4], [GHead5| GTail5], [GHead6| GTail6]) :-
						GGoal, map_(GTail1, GArgs, GTail2, GTail3, GTail4, GTail5, GTail6))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/7))
		).

	goal_expansion(meta::maplist(Closure, List1, List2, List3, List4, List5, List6), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6), ExpandedGoal).

	goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6, List7), ExpandedGoal) :-
		decompose_closure(Closure, 7, Functor, Arity, Args, GArgs),
		aux_predicate_functor(map, 8, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/8) ->
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6, List7)], map_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GHead2, GHead3, GHead4, GHead5, GHead6, GHead7], GGoal),
			Clauses0 = [
					map_([], _, [], [], [], [], [], []),
					(map_([GHead1| GTail1], GArgs, [GHead2| GTail2], [GHead3| GTail3], [GHead4| GTail4], [GHead5| GTail5], [GHead6| GTail6], [GHead7| GTail7]) :-
						GGoal, map_(GTail1, GArgs, GTail2, GTail3, GTail4, GTail5, GTail6, GTail7))
				],
			replace_functor([map_(List1, Args, List2, List3, List4, List5, List6, List7)| Clauses0], map_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/8))
		).

	goal_expansion(meta::maplist(Closure, List1, List2, List3, List4, List5, List6, List7), ExpandedGoal) :-
		goal_expansion(meta::map(Closure, List1, List2, List3, List4, List5, List6, List7), ExpandedGoal).

	goal_expansion(meta::fold_left(Closure, Acc, List, Result), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(fold_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([fold_left_(List, Args, Acc, Result)], fold_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GAcc, GHead, GAcc2], GGoal),
			Clauses0 = [
					fold_left_([], _, GResult, GResult),
					(fold_left_([GHead| GTail], GArgs, GAcc, GResult) :-
						GGoal, fold_left_(GTail, GArgs, GAcc2, GResult))
				],
			replace_functor([fold_left_(List, Args, Acc, Result)| Clauses0], fold_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::foldl(Closure, Acc, List, Result), ExpandedGoal) :-
		goal_expansion(meta::fold_left(Closure, Acc, List, Result), ExpandedGoal).

	goal_expansion(meta::fold_left_1(Closure, [Head| Tail], Result), ExpandedGoal) :-
		goal_expansion(meta::fold_left(Closure, Head, Tail, Result), ExpandedGoal).

	goal_expansion(meta::foldl1(Closure, [Head| Tail], Result), ExpandedGoal) :-
		goal_expansion(meta::fold_left(Closure, Head, Tail, Result), ExpandedGoal).

	goal_expansion(meta::fold_right(Closure, Acc, List, Result), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(fold_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([fold_right_(List, Args, Acc, Result)], fold_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead, GAcc2, GResult], GGoal),
			Clauses0 = [
					fold_right_([], _, GResult, GResult),
					(fold_right_([GHead| GTail], GArgs, GAcc, GResult) :-
						fold_right_(GTail, GArgs, GAcc, GAcc2), GGoal)
				],
			replace_functor([fold_right_(List, Args, Acc, Result)| Clauses0], fold_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::foldr(Closure, Acc, List, Result), ExpandedGoal) :-
		goal_expansion(meta::fold_right(Closure, Acc, List, Result), ExpandedGoal).

	goal_expansion(meta::fold_right_1(Closure, [Head| Tail], Result), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(fold_right_1, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([fold_right_1_(Tail, Head, Args, _, Result)], fold_right_1_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GAcc2, GResult], GGoal),
			Clauses0 = [
					fold_right_1_([], GResult, _, GResult, GResult),
					(fold_right_1_([GHead2| GTail], GHead1, GArgs, GAcc, GResult) :-
						fold_right_1_(GTail, GHead2, GArgs, GAcc, GAcc2), GGoal)
				],
			replace_functor([fold_right_1_(Tail, Head, Args, _, Result)| Clauses0], fold_right_1_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::foldr1(Closure, List, Result), ExpandedGoal) :-
		goal_expansion(meta::fold_right_1(Closure, List, Result), ExpandedGoal).

	goal_expansion(meta::scan_left(Closure, Acc, List, [Acc| Results]), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(scan_left, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([scan_left_(List, Args, Acc, Results)], scan_left_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GAcc, GHead, GAcc2], GGoal),
			Clauses0 = [
					scan_left_([], _, _, []),
					(scan_left_([GHead| GTail], GArgs, GAcc, [GAcc2| GResults]) :-
						GGoal, scan_left_(GTail, GArgs, GAcc2, GResults))
				],
			replace_functor([scan_left_(List, Args, Acc, Results)| Clauses0], scan_left_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::scanl(Closure, Acc, List, Results), ExpandedGoal) :-
		goal_expansion(meta::scan_left(Closure, Acc, List, Results), ExpandedGoal).

	goal_expansion(meta::scan_left_1(Closure, [Head| Tail], Results), ExpandedGoal) :-
		goal_expansion(meta::scan_left(Closure, Head, Tail, Results), ExpandedGoal).

	goal_expansion(meta::scanl1(Closure, [Head| Tail], Results), ExpandedGoal) :-
		goal_expansion(meta::scan_left(Closure, Head, Tail, Results), ExpandedGoal).

	goal_expansion(meta::scan_right(Closure, Acc, List, Results), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(scan_right, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([scan_right_(List, Args, Acc, Results)], scan_right_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead, GAcc2, GResult], GGoal),
			Clauses0 = [
					scan_right_([], _, GResult, [GResult]),
					(scan_right_([GHead| GTail], GArgs, GAcc, [GResult, GAcc2| GResults]) :-
						scan_right_(GTail, GArgs, GAcc, [GAcc2| GResults]), GGoal)
				],
			replace_functor([scan_right_(List, Args, Acc, Results)| Clauses0], scan_right_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::scanr(Closure, Acc, List, Results), ExpandedGoal) :-
		goal_expansion(meta::scan_right(Closure, Acc, List, Results), ExpandedGoal).

	goal_expansion(meta::scan_right_1(Closure, [Head| Tail], Results), ExpandedGoal) :-
		decompose_closure(Closure, 3, Functor, Arity, Args, GArgs),
		aux_predicate_functor(scan_right_1, 4, Functor, Arity, AuxFunctor),
		(	generated_predicate(AuxFunctor/4) ->
			replace_functor([scan_right_1_(Tail, Head, Args, _, Results)], scan_right_1_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(Functor, GArgs, [GHead1, GAcc2, GResult], GGoal),
			Clauses0 = [
					scan_right_1_([], GResult, _, GResult, [GResult]),
					(scan_right_1_([GHead2| GTail], GHead1, GArgs, GAcc, [GResult, GAcc2| GResults]) :-
						scan_right_1_(GTail, GHead2, GArgs, GAcc, [GAcc2| GResults]), GGoal)
				],
			replace_functor([scan_right_1_(Tail, Head, Args, _, Results)| Clauses0], scan_right_1_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/4))
		).

	goal_expansion(meta::scanr1(Closure, List, Results), ExpandedGoal) :-
		goal_expansion(meta::scan_right_1(Closure, List, Results), ExpandedGoal).

	goal_expansion(meta::map_reduce(Map, Reduce, Acc, List, Result), ExpandedGoal) :-
		decompose_closure(Map, 2, MapFunctor, _, MapArgs, GMapArgs),
		decompose_closure(Reduce, 3, ReduceFunctor, _, ReduceArgs, GReduceArgs),
		atom_concat(MapFunctor, '+', Functor0),
		atom_concat(Functor0, ReduceFunctor, Functor),
		aux_predicate_functor(map_reduce, 5, Functor, 3, AuxFunctor),
		(	generated_predicate(AuxFunctor/5) ->
			replace_functor([map_reduce_(List, MapArgs, ReduceArgs, Acc, Result)], map_reduce_, AuxFunctor, [ExpandedGoal])
		;	extend_closure(MapFunctor, GMapArgs, [GHead, GHead2], GMapGoal),
			extend_closure(ReduceFunctor, GReduceArgs, [GAcc, GHead2, GAcc2], GReduceGoal),
			Clauses0 = [
					map_reduce_([], _, _, GResult, GResult),
					(map_reduce_([GHead| GTail], GMapArgs, GReduceArgs, GAcc, GResult) :-
						GMapGoal,
						GReduceGoal,
						map_reduce_(GTail, GMapArgs, GReduceArgs, GAcc2, GResult))
				],
			replace_functor([map_reduce_(List, MapArgs, ReduceArgs, Acc, Result)| Clauses0], map_reduce_, AuxFunctor, [ExpandedGoal| Clauses]),
			logtalk::compile_aux_clauses(Clauses),
			assertz(generated_predicate(AuxFunctor/5))
		).

	decompose_closure({Free}/Parameters>>Goal, MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Goal),
		length(Parameters, MetaArity),
		gensym('lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		logtalk::compile_aux_clauses([(Head :- Goal)]).
	decompose_closure({Free}/(Object::Closure), MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		\+ control_construct(Goal),
		logtalk::compile_aux_clauses([(Head :- Object::Goal)]).
	decompose_closure({Free}/{Closure}, MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		\+ control_construct(Goal),
		logtalk::compile_aux_clauses([(Head :- {Goal})]).
	decompose_closure({Free}/Closure, MetaArity, Functor, Arity, FreeList, GFreeList) :-
		!,
		callable(Closure),
		gensym('lambda_', Functor),
		conjunction_to_list(Free, FreeList, Arity),
		length(GFreeList, Arity),
		length(Parameters, MetaArity),
		append(FreeList, Parameters, Args),
		Head =.. [Functor| Args],
		Closure =.. [ClosureFunctor| ClosureArgs],
		append(ClosureArgs, Parameters, GoalArgs),
		Goal =.. [ClosureFunctor| GoalArgs],
		\+ control_construct(Goal),
		logtalk::compile_aux_clauses([(Head :- Goal)]).
	decompose_closure(Parameters>>Goal, MetaArity, Functor, 0, [], []) :-
		!,
		callable(Goal),
		length(Parameters, MetaArity),
		gensym('lambda_', Functor),
		Head =.. [Functor| Parameters],
		logtalk::compile_aux_clauses([(Head :- Goal)]).
	decompose_closure(Object::Closure, MetaArity, Object::Functor, Arity, Args, GArgs) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, MetaArity, Functor, Arity, Args, GArgs).
	decompose_closure({Closure}, MetaArity, {Functor}, Arity, Args, GArgs) :-
		!,
		nonvar(Closure),
		decompose_closure(Closure, MetaArity, Functor, Arity, Args, GArgs).
	decompose_closure(Closure, MetaArity, Functor, Arity, Args, GArgs) :-
		callable(Closure),
		Closure =.. [Functor| Args],
		length(ExtraArgs, MetaArity),
		append(Args, ExtraArgs, GoalArgs),
		Goal =.. [Functor| GoalArgs],
		\+ control_construct(Goal),
		functor(Closure, Functor, Arity),
		functor(GClosure, Functor, Arity),
		GClosure =.. [Functor| GArgs].

	extend_closure(Object::Functor, ClosureArgs, ExtraArgs, Object::Goal) :-
		!,
		extend_closure(Functor, ClosureArgs, ExtraArgs, Goal).
	extend_closure({Functor}, ClosureArgs, ExtraArgs, {Goal}) :-
		!,
		extend_closure(Functor, ClosureArgs, ExtraArgs, Goal).
	extend_closure(Functor, ClosureArgs, ExtraArgs, Goal) :-
		append(ClosureArgs, ExtraArgs, Args),
		Goal =.. [Functor| Args].

	replace_functor([], _, _, []).
	replace_functor([(Head0:-Body0)| Clauses0], Functor, AuxFunctor, [(Head:-Body)| Clauses]) :-
		!,
		replace_functor_head(Head0, Functor, AuxFunctor, Head),
		replace_functor_body(Body0, Functor, AuxFunctor, Body),
		replace_functor(Clauses0, Functor, AuxFunctor, Clauses).
	replace_functor([Head0| Clauses0], Functor, AuxFunctor, [Head| Clauses]) :-
		replace_functor_head(Head0, Functor, AuxFunctor, Head),
		replace_functor(Clauses0, Functor, AuxFunctor, Clauses).

	replace_functor_head(Head0, Functor, AuxFunctor, Head) :-
		(	Head0 =.. [Functor| Args] ->
			Head =.. [AuxFunctor| Args]
		;	Head = Head0
		).

	replace_functor_body((Goal01->Goal02;Goal03), Functor, AuxFunctor, (Goal1->Goal2;Goal3)) :-
		!,
		replace_functor_body(Goal01, Functor, AuxFunctor, Goal1),
		replace_functor_body(Goal02, Functor, AuxFunctor, Goal2),
		replace_functor_body(Goal03, Functor, AuxFunctor, Goal3).
	replace_functor_body((Goal01,Goal02), Functor, AuxFunctor, (Goal1,Goal2)) :-
		!,
		replace_functor_body(Goal01, Functor, AuxFunctor, Goal1),
		replace_functor_body(Goal02, Functor, AuxFunctor, Goal2).
	replace_functor_body(Goal0, Functor, AuxFunctor, Goal) :-
		replace_functor_head(Goal0, Functor, AuxFunctor, Goal).

	control_construct((_ , _)).
	control_construct((_ ; _)).
	control_construct((_ -> _)).
	control_construct(\+ _).
	control_construct(^^ _).
	control_construct(_ :: _).
	control_construct(:: _).
	control_construct(_ / _).
	control_construct(_ >> _).
	control_construct(_ << _).

	conjunction_to_list(Conjunction, Terms, N) :-
		conjunction_to_list(Conjunction, Terms, 1, N).

	conjunction_to_list(Term, [Term], N, N) :-
		var(Term),
		!.
	conjunction_to_list((Term, Conjunction), [Term| Terms], N0, N) :-
		!,
		N1 is N0 + 1,
		conjunction_to_list(Conjunction, Terms, N1, N).
	conjunction_to_list(Term, [Term], N, N).

	aux_predicate_functor(MetaFunctor, MetaArity, Object::ClosureFunctor, ClosureArity, AuxFunctor) :-
		!,
		atom_concat(MetaFunctor, '/', AuxFunctor0),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor0, MetaArityAtom, AuxFunctor1),
		atom_concat(AuxFunctor1, '+', AuxFunctor2),
		(	atom(Object) ->
			atom_concat(AuxFunctor2, Object, AuxFunctor3),
			atom_concat(AuxFunctor3, '.', AuxFunctor6)
		;	functor(Object, ObjectFunctor, ObjectArity),
			atom_concat(AuxFunctor2, ObjectFunctor, AuxFunctor3),
			atom_concat(AuxFunctor3, '.', AuxFunctor4),
			number_codes(ObjectArity, ObjectArityCodes),
			atom_codes(ObjectArityAtom, ObjectArityCodes),
			atom_concat(AuxFunctor4, ObjectArityAtom, AuxFunctor5),
			atom_concat(AuxFunctor5, '.', AuxFunctor6)
		),
		atom_concat(AuxFunctor6, ClosureFunctor, AuxFunctor7),
		atom_concat(AuxFunctor7, '#', AuxFunctor8),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor8, ClosureArityAtom, AuxFunctor).
	aux_predicate_functor(MetaFunctor, MetaArity, {ClosureFunctor}, ClosureArity, AuxFunctor) :-
		!,
		atom_concat(MetaFunctor, '/', AuxFunctor0),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor0, MetaArityAtom, AuxFunctor1),
		atom_concat(AuxFunctor1, '+{', AuxFunctor2),
		atom_concat(AuxFunctor2, ClosureFunctor, AuxFunctor3),
		atom_concat(AuxFunctor3, '#', AuxFunctor4),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor4, ClosureArityAtom, AuxFunctor5),
		atom_concat(AuxFunctor5, '}', AuxFunctor).
	aux_predicate_functor(MetaFunctor, MetaArity, ClosureFunctor, ClosureArity, AuxFunctor) :-
		atom_concat(MetaFunctor, '/', AuxFunctor0),
		number_codes(MetaArity, MetaArityCodes),
		atom_codes(MetaArityAtom, MetaArityCodes),
		atom_concat(AuxFunctor0, MetaArityAtom, AuxFunctor1),
		atom_concat(AuxFunctor1, '+', AuxFunctor2),
		atom_concat(AuxFunctor2, ClosureFunctor, AuxFunctor3),
		atom_concat(AuxFunctor3, '#', AuxFunctor4),
		number_codes(ClosureArity, ClosureArityCodes),
		atom_codes(ClosureArityAtom, ClosureArityCodes),
		atom_concat(AuxFunctor4, ClosureArityAtom, AuxFunctor).

:- end_object.
