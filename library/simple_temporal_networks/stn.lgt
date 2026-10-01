%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
%  SPDX-License-Identifier: Apache-2.0
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


:- object(stn,
	implements(stn_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Immutable Simple Temporal Networks with retained source constraints and cached all-pairs bounds.',
		see_also is [interval, interval_constraint_network]
	]).

	:- uses(interval, [
		new/3 as interval_new/3, relation/3 as interval_relation/3
	]).

	:- uses(list, [
		append/3, length/2, member/2, reverse/2, nth1/3 as label_at/3
	]).

	:- uses(type, [
		valid/2
	]).

	new(TimePoints, STN) :-
		valid(list, TimePoints),
		ground(TimePoints),
		fresh_points(TimePoints, [zero]),
		Nodes = [zero| TimePoints],
		build_network(Nodes, 1, [], consistent(STN)).

	time_points(STN, Nodes) :-
		state(STN, Nodes, _, _, _).

	add_time_points(STN0, TimePoints, STN) :-
		state(STN0, Nodes0, NextId, Sources, _),
		valid(list, TimePoints),
		ground(TimePoints),
		fresh_points(TimePoints, Nodes0),
		(	TimePoints == [] ->
			STN = STN0
		;
			append(Nodes0, TimePoints, Nodes),
			build_network(Nodes, NextId, Sources, consistent(STN))
		).

	remove_time_points(STN0, TimePoints, STN) :-
		state(STN0, Nodes0, NextId, Sources0, _),
		valid(list, TimePoints),
		ground(TimePoints),
		\+ member(zero, TimePoints),
		retain_time_points(Nodes0, TimePoints, Nodes),
		( 	Nodes == Nodes0 ->
			STN = STN0
		;
			retain_point_sources(Sources0, TimePoints, Sources),
			build_network(Nodes, NextId, Sources, consistent(STN))
		).

	constraints(STN, Sources) :-
		state(STN, _, _, Sources, _).

	consistent(STN) :-
		state(STN, _, _, _, _).

	schedule(STN, Schedule) :-
		state(STN, [zero| Nodes], _, Sources, Matrix),
		functor(Matrix, _, Size),
		column_minimum(1, Size, 1, Matrix, 0, Reference),
		NegativeReference is -Reference,
		schedule_points(Nodes, 2, Size, Matrix, NegativeReference, Times),
		Assignment = [time(zero, 0)| Times],
		check_schedule_sources(Sources, Assignment),
		Schedule = Assignment.

	earliest_schedule(STN, Schedule) :-
		state(STN, [zero| Nodes], _, Sources, Matrix),
		earliest_schedule_points(Nodes, 2, Matrix, Times),
		Assignment = [time(zero, 0)| Times],
		check_schedule_sources(Sources, Assignment),
		Schedule = Assignment.

	add_constraint(STN0, X, Y, Delta, STN) :-
		add_constraints(STN0, [constraint(X, Y, Delta)], STN).

	add_constraint(STN0, X, Y, Delta, Id, STN) :-
		add_constraints(STN0, [constraint(X, Y, Delta)], [Id], STN).

	add_constraints(STN0, Constraints, STN) :-
		try_add_constraints(STN0, Constraints, _, consistent(STN)).

	add_constraints(STN0, Constraints, Ids, STN) :-
		try_add_constraints(STN0, Constraints, Ids, consistent(STN)).

	try_add_constraints(STN0, Constraints, Ids, Outcome) :-
		state(STN0, Nodes, NextId0, Sources0, _),
		valid(list, Constraints),
		ground(Constraints),
		allocate_constraints(Constraints, Nodes, NextId0, NextId, Added, Ids),
		(	Added == [] ->
			Outcome = consistent(STN0)
		;
			append(Sources0, Added, Sources),
			build_network(Nodes, NextId, Sources, Outcome)
		).

	remove_constraints(STN0, Ids, STN) :-
		state(STN0, Nodes, NextId, Sources0, _),
		valid(list, Ids),
		ground(Ids),
		positive_ids(Ids),
		retain_sources(Sources0, Ids, Sources),
		(	Sources == Sources0 ->
			STN = STN0
		;
			build_network(Nodes, NextId, Sources, consistent(STN))
		).

	distance(STN, X, Y, Upper) :-
		state(STN, Nodes, _, _, Matrix),
		point_index(Nodes, X, From),
		point_index(Nodes, Y, To),
		matrix_cell(Matrix, From, To, cell(Upper, _)).

	distance(STN, X, Y, Upper, Path) :-
		state(STN, Nodes, _, _, Matrix),
		point_index(Nodes, X, From),
		point_index(Nodes, Y, To),
		matrix_cell(Matrix, From, To, cell(Upper, _)),
		number(Upper),
		length(Nodes, Size),
		path_sources(From, To, Matrix, Size, Path).

	entails(STN, X, Y, Delta) :-
		finite_number(Delta),
		distance(STN, X, Y, Upper),
		number(Upper),
		Upper =< Delta.

	entails(STN, X, Y, Delta, Path) :-
		entails(STN, X, Y, Delta),
		distance(STN, X, Y, _, Path).

	earliest(STN, Point, Earliest) :-
		distance(STN, Point, zero, Reverse),
		negate_bound(Reverse, Earliest).

	latest(STN, Point, Latest) :-
		distance(STN, zero, Point, Latest).

	bounds(STN, Point, Earliest, Latest) :-
		difference_bounds(STN, zero, Point, Earliest, Latest).

	difference_bounds(STN, X, Y, Lower, Upper) :-
		distance(STN, X, Y, Upper),
		distance(STN, Y, X, Reverse),
		negate_bound(Reverse, Lower).

	can_precede(STN, X, Y) :-
		distance(STN, X, Y, Upper),
		(	Upper == positive_infinity ->
			true
		;
			Upper > 0
		).

	must_precede(STN, X, Y) :-
		difference_bounds(STN, X, Y, Lower, _),
		number(Lower),
		Lower > 0.

	can_precede_or_equal(STN, X, Y) :-
		distance(STN, X, Y, Upper),
		(	Upper == positive_infinity ->
			true
		;
			Upper >= 0
		).

	must_precede_or_equal(STN, X, Y) :-
		difference_bounds(STN, X, Y, Lower, _),
		number(Lower),
		Lower >= 0.

	window_interval(STN, Point, Interval) :-
		bounds(STN, Point, Start, End),
		proper_numeric_interval(Start, End),
		Interval = i(Start, End).

	event_interval(STN, StartPoint, EndPoint, Interval) :-
		fixed_point(STN, StartPoint, Start),
		fixed_point(STN, EndPoint, End),
		proper_numeric_interval(Start, End),
		Interval = i(Start, End).

	window_relation(STN, Point1, Point2, Relation) :-
		window_interval(STN, Point1, i(Start1, End1)),
		window_interval(STN, Point2, i(Start2, End2)),
		ranked_relation(Start1, End1, Start2, End2, Relation).

	event_relation(STN, StartPoint1, EndPoint1, StartPoint2, EndPoint2, Relation) :-
		event_interval(STN, StartPoint1, EndPoint1, i(Start1, End1)),
		event_interval(STN, StartPoint2, EndPoint2, i(Start2, End2)),
		ranked_relation(Start1, End1, Start2, End2, Relation).

	fixed_point(STN, Point, Value) :-
		bounds(STN, Point, Value, Latest),
		number(Value),
		number(Latest),
		Value =:= Latest.

	proper_numeric_interval(Start, End) :-
		number(Start),
		number(End),
		Start < End.

	ranked_relation(Start1, End1, Start2, End2, Relation) :-
		Values = [Start1, End1, Start2, End2],
		numeric_rank(Start1, Values, StartRank1),
		numeric_rank(End1, Values, EndRank1),
		numeric_rank(Start2, Values, StartRank2),
		numeric_rank(End2, Values, EndRank2),
		interval_new(StartRank1, EndRank1, Interval1),
		interval_new(StartRank2, EndRank2, Interval2),
		interval_relation(Interval1, Interval2, Relation).

	numeric_rank(_Value, [], 0) :-
		!.
	numeric_rank(Value, [Head| Values], Rank) :-
		numeric_rank(Value, Values, Rest),
		(	Head < Value ->
			Rank is Rest + 1
		;
			Rank = Rest
		).

	state(STN, Nodes, NextId, Sources, Matrix) :-
		ground(STN),
		STN = stn(Nodes, NextId, Sources, Matrix),
		Nodes = [zero| _],
		integer(NextId),
		NextId > 0,
		functor(Matrix, distances, _).

	fresh_points([], _Seen) :-
		!.
	fresh_points([Point| Points], Seen) :-
		\+ member(Point, Seen),
		fresh_points(Points, [Point| Seen]).

	positive_ids([]) :-
		!.
	positive_ids([Id| Ids]) :-
		integer(Id),
		Id > 0,
		positive_ids(Ids).

	retain_sources([], _Ids, []) :-
		!.
	retain_sources([Source| Sources0], Ids, Sources) :-
		Source = constraint(Id, _, _, _),
		(	member(Id, Ids) ->
			Sources = Rest
		;
			Sources = [Source| Rest]
		),
		retain_sources(Sources0, Ids, Rest).

	retain_time_points([], _TimePoints, []) :-
		!.
	retain_time_points([Point| Nodes0], TimePoints, Nodes) :-
		( 	member(Point, TimePoints) ->
			Nodes = Rest
		;
			Nodes = [Point| Rest]
		),
		retain_time_points(Nodes0, TimePoints, Rest).

	retain_point_sources([], _TimePoints, []) :-
		!.
	retain_point_sources([Source| Sources0], TimePoints, Sources) :-
		Source = constraint(_, X, Y, _),
		( 	( 	member(X, TimePoints)
			; 	member(Y, TimePoints)
			) ->
			Sources = Rest
		;
			Sources = [Source| Rest]
		),
		retain_point_sources(Sources0, TimePoints, Rest).

	path_sources(From, To, Matrix, Remaining, Sources) :-
		(	From =:= To ->
			Sources = []
		;	Remaining =:= 0 ->
			evaluation_error(stn_numerical_inconsistency)
		;
			matrix_cell(Matrix, From, To, cell(_, edge(From, Next, Source))),
			Sources = [Source| Rest],
			NextRemaining is Remaining - 1,
			path_sources(Next, To, Matrix, NextRemaining, Rest)
		).

	point_index(Nodes, Point, Index) :-
		ground(Point),
		find_point(Nodes, Point, 1, Index).

	find_point([Head| Tail], Point, Position, Index) :-
		(	Point == Head ->
			Index = Position
		;
			NextPosition is Position + 1,
			find_point(Tail, Point, NextPosition, Index)
		).

	finite_number(Value) :-
		(	integer(Value) ->
			true
		;	float(Value),
			catch((Zero is Value - Value, Zero =:= 0), _, fail)
		).

	sum_weights(Left, Right, Sum) :-
		Sum is Left + Right,
		(	finite_number(Sum) ->
			true
		;	evaluation_error(float_overflow)
		).

	column_minimum(Row, Size, Column, Matrix, Minimum0, Minimum) :-
		(	Row > Size ->
			Minimum = Minimum0
		;	matrix_cell(Matrix, Row, Column, cell(Distance, _)),
			(	Distance == positive_infinity ->
				Minimum1 = Minimum0
			;	(	Distance < Minimum0 ->
					Minimum1 = Distance
				;	Minimum1 = Minimum0
				)
			),
			NextRow is Row + 1,
			column_minimum(NextRow, Size, Column, Matrix, Minimum1, Minimum)
		).

	schedule_points([], _Column, _Size, _Matrix, _NegativeReference, []).
	schedule_points([Point| Nodes], Column, Size, Matrix, NegativeReference, [time(Point, Time)| Times]) :-
		column_minimum(1, Size, Column, Matrix, 0, Minimum),
		sum_weights(Minimum, NegativeReference, Time),
		NextColumn is Column + 1,
		schedule_points(Nodes, NextColumn, Size, Matrix, NegativeReference, Times).

	earliest_schedule_points([], _Row, _Matrix, []).
	earliest_schedule_points([Point| Nodes], Row, Matrix, [time(Point, Time)| Times]) :-
		matrix_cell(Matrix, Row, 1, cell(Reverse, _)),
		finite_number(Reverse),
		negate_bound(Reverse, Time),
		finite_number(Time),
		NextRow is Row + 1,
		earliest_schedule_points(Nodes, NextRow, Matrix, Times).

	check_schedule_sources([], _Assignment).
	check_schedule_sources([constraint(_, X, Y, Weight)| Sources], Assignment) :-
		schedule_point_time(Assignment, X, Left),
		schedule_point_time(Assignment, Y, Right),
		sum_weights(Right, -Left, Difference),
		(	Difference =< Weight ->
			true
		;	evaluation_error(stn_numerical_inconsistency)
		),
		check_schedule_sources(Sources, Assignment).

	schedule_point_time([time(Node, Time)| Times], Point, Value) :-
		(	Node == Point ->
			Value = Time
		;	schedule_point_time(Times, Point, Value)
		).

	negate_bound(positive_infinity, negative_infinity) :-
		!.
	negate_bound(Value, Negated) :-
		Negated is -Value.

	allocate_constraints([], _Nodes, NextId, NextId, [], []) :-
		!.
	allocate_constraints([constraint(X, Y, Delta)| Constraints], Nodes, Id, NextId, [constraint(Id, X, Y, Delta)| Sources], [Id| Ids]) :-
		point_index(Nodes, X, _),
		point_index(Nodes, Y, _),
		finite_number(Delta),
		FollowingId is Id + 1,
		allocate_constraints(Constraints, Nodes, FollowingId, NextId, Sources, Ids).

	build_network(Nodes, NextId, Sources, Outcome) :-
		length(Nodes, Size),
		index_sources(Sources, Nodes, Edges),
		initial_labels(1, Size, Labels),
		bellman_ford(Size, Edges, Labels, Check),
		(	Check = cycle(_, _) ->
			Outcome = inconsistent(Check)
		;
			empty_matrix(Size, Matrix0),
			post_edges(Edges, Matrix0, Matrix1),
			close_matrix(1, Size, Matrix1, Matrix),
			check_diagonal(1, Size, Matrix),
			Outcome = consistent(stn(Nodes, NextId, Sources, Matrix))
		).

	index_sources([], _Nodes, []).
	index_sources([Source| Sources], Nodes, [edge(From, To, Source)| Edges]) :-
		Source = constraint(_, X, Y, _),
		point_index(Nodes, X, From),
		point_index(Nodes, Y, To),
		index_sources(Sources, Nodes, Edges).

	initial_labels(Position, Size, Labels) :-
		(	Position > Size ->
			Labels = []
		;	Labels = [label(0, none)| Rest],
			NextPosition is Position + 1,
			initial_labels(NextPosition, Size, Rest)
		).

	bellman_ford(Remaining, Edges, Labels0, Check) :-
		relax_edges(Edges, Labels0, Labels, none, Changed),
		(	Changed == none ->
			Check = consistent
		;	Remaining =:= 1 ->
			length(Labels, Size),
			cycle_entry(Size, Changed, Labels, Start),
			cycle_edges(Start, Start, Labels, Size, Backward),
			reverse(Backward, Sources),
			source_weight(Sources, 0, Weight),
			(	Weight < 0 ->
				Check = cycle(Sources, Weight)
			;	evaluation_error(stn_numerical_inconsistency)
			)
		;
			NextRemaining is Remaining - 1,
			bellman_ford(NextRemaining, Edges, Labels, Check)
		).

	relax_edges([], Labels, Labels, Changed, Changed).
	relax_edges([Edge| Edges], Labels0, Labels, Changed0, Changed) :-
		Edge = edge(From, To, constraint(_, _, _, Weight)),
		label_at(From, Labels0, label(Left, _)),
		label_at(To, Labels0, label(Right, _)),
		sum_weights(Left, Weight, Candidate),
		(	Candidate < Right ->
			replace_label(To, label(Candidate, Edge), Labels0, Labels1),
			Changed1 = To
		;	Labels1 = Labels0,
			Changed1 = Changed0
		),
		relax_edges(Edges, Labels1, Labels, Changed1, Changed).

	replace_label(1, Label, [_| Labels], [Label| Labels]) :-
		!.
	replace_label(Index, Label, [Head| Labels0], [Head| Labels]) :-
		Previous is Index - 1,
		replace_label(Previous, Label, Labels0, Labels).

	cycle_entry(0, Point, _Labels, Point) :-
		!.
	cycle_entry(Remaining, Point, Labels, Start) :-
		label_at(Point, Labels, label(_, edge(Previous, _, _))),
		NextRemaining is Remaining - 1,
		cycle_entry(NextRemaining, Previous, Labels, Start).

	cycle_edges(Point, Start, Labels, Remaining, [Source| Sources]) :-
		Remaining > 0,
		label_at(Point, Labels, label(_, edge(Previous, _, Source))),
		(	Previous =:= Start ->
			Sources = []
		;
			NextRemaining is Remaining - 1,
			cycle_edges(Previous, Start, Labels, NextRemaining, Sources)
		).

	source_weight([], Total, Total).
	source_weight([constraint(_, _, _, Weight)| Sources], Total0, Total) :-
		sum_weights(Weight, Total0, Total1),
		source_weight(Sources, Total1, Total).

	empty_matrix(Size, Matrix) :-
		functor(Matrix, distances, Size),
		empty_rows(1, Size, Matrix).

	empty_rows(Position, Size, Matrix) :-
		(	Position > Size ->
			true
		;	functor(Row, row, Size),
			empty_cells(1, Size, Position, Row),
			arg(Position, Matrix, Row),
			NextPosition is Position + 1,
			empty_rows(NextPosition, Size, Matrix)
		).

	empty_cells(Column, Size, Position, Row) :-
		(	Column > Size ->
			true
		;	(	Column =:= Position ->
				Cell = cell(0, none)
			;	Cell = cell(positive_infinity, none)
			),
			arg(Column, Row, Cell),
			NextColumn is Column + 1,
			empty_cells(NextColumn, Size, Position, Row)
		).

	matrix_cell(Matrix, From, To, Cell) :-
		arg(From, Matrix, Row),
		arg(To, Row, Cell).

	post_edges([], Matrix, Matrix).
	post_edges([Edge| Edges], Matrix0, Matrix) :-
		Edge = edge(From, To, constraint(_, _, _, Weight)),
		matrix_cell(Matrix0, From, To, cell(Old, _)),
		(	tighter(Weight, Old) ->
			arg(From, Matrix0, Row0),
			replace_argument(Row0, To, cell(Weight, Edge), Row),
			replace_argument(Matrix0, From, Row, Matrix1)
		;	Matrix1 = Matrix0
		),
		post_edges(Edges, Matrix1, Matrix).

	tighter(_Value, positive_infinity) :-
		!.
	tighter(Value, Old) :-
		Value < Old.

	replace_argument(Term, Index, Value, Updated) :-
		functor(Term, Functor, Size),
		functor(Updated, Functor, Size),
		copy_arguments(1, Size, Term, Index, Value, Updated).

	copy_arguments(Position, Size, Term, Index, Value, Updated) :-
		(	Position > Size ->
			true
		;	(	Position =:= Index ->
				Argument = Value
			;	arg(Position, Term, Argument)
			),
			arg(Position, Updated, Argument),
			NextPosition is Position + 1,
			copy_arguments(NextPosition, Size, Term, Index, Value, Updated)
		).

	close_matrix(Via, Size, Matrix0, Matrix) :-
		(	Via > Size ->
			Matrix = Matrix0
		;	functor(Matrix1, distances, Size),
			close_rows(1, Size, Via, Matrix0, Matrix1),
			NextVia is Via + 1,
			close_matrix(NextVia, Size, Matrix1, Matrix)
		).

	close_rows(From, Size, Via, Matrix0, Matrix) :-
		(	From > Size ->
			true
		;	functor(Row, row, Size),
			close_cells(1, Size, From, Via, Matrix0, Row),
			arg(From, Matrix, Row),
			NextFrom is From + 1,
			close_rows(NextFrom, Size, Via, Matrix0, Matrix)
		).

	close_cells(To, Size, From, Via, Matrix, Row) :-
		(	To > Size ->
			true
		;	matrix_cell(Matrix, From, To, Old),
			matrix_cell(Matrix, From, Via, cell(Left, Edge)),
			matrix_cell(Matrix, Via, To, cell(Right, _)),
			(	Left == positive_infinity ->
				Cell = Old
			;	Right == positive_infinity ->
				Cell = Old
			;	sum_weights(Left, Right, Candidate),
				Old = cell(Upper, _),
				(	tighter(Candidate, Upper) ->
					Cell = cell(Candidate, Edge)
				;	Cell = Old
				)
			),
			arg(To, Row, Cell),
			NextTo is To + 1,
			close_cells(NextTo, Size, From, Via, Matrix, Row)
		).

	check_diagonal(Position, Size, Matrix) :-
		(	Position > Size ->
			true
		;	matrix_cell(Matrix, Position, Position, cell(Value, _)),
			(	Value < 0 ->
				evaluation_error(stn_numerical_inconsistency)
			;	NextPosition is Position + 1,
				check_diagonal(NextPosition, Size, Matrix)
			)
		).

:- end_object.
