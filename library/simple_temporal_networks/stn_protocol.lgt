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


:- protocol(stn_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-01,
		comment is 'Portable, functional Simple Temporal Networks over closed difference constraints.',
		remarks is [
			'Constraints' - 'A constraint from ``X`` to ``Y`` with weight ``Delta`` means ``time(Y) - time(X) =< Delta``.',
			'Inputs' - 'States must be opaque ground terms returned by this API. Malformed public arguments and unknown time-points fail without instantiating inputs; hand-built internal states are unsupported.',
			'Numbers' - 'Weights are finite integers or floats. Arithmetic errors propagate; floating-point results are approximate, with no implicit tolerance.',
			'Bounds' - 'Unbounded query results use the atoms positive_infinity and negative_infinity, never native numeric infinities.'
		]
	]).

	:- public(new/2).
	:- mode(new(+list, -stn), zero_or_one).
	:- info(new/2, [
		comment is 'Creates an unconstrained network over distinct ground time-points and a reserved reference time-point zero fixed at time zero. Fails for duplicate labels or an explicitly supplied zero.',
		argnames is ['TimePoints', 'STN']
	]).

	:- public(add_time_points/3).
	:- mode(add_time_points(+stn, +list, -stn), zero_or_one_or_error).
	:- info(add_time_points/3, [
		comment is 'Appends distinct fresh ground time-points, rebuilding once and preserving source identifiers. Fails for existing labels or zero. An empty list returns the original state.',
		argnames is ['STN0', 'TimePoints', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(remove_time_points/3).
	:- mode(remove_time_points(+stn, +list, -stn), zero_or_one_or_error).
	:- info(remove_time_points/3, [
		comment is 'Removes time-points and all incident source constraints, rebuilding once and preserving surviving node order and source identifiers. Missing labels and duplicate requests are ignored. Fails for a request containing zero or a malformed or nonground list.',
		argnames is ['STN0', 'TimePoints', 'STN'],
		remarks is [
			'Bounds' - 'Deletion retracts incident sources, not just variables. Bounds supported through removed points may weaken or become unbounded; implied bounds are not preserved as new constraints.',
			'Identity' - 'Labels are compared by term identity. The original state and next source identifier are preserved. A request removing no existing point returns the original state.'
		],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(time_points/2).
	:- mode(time_points(+stn, -list), zero_or_one).
	:- info(time_points/2, [
		comment is 'Returns the declared time-points in insertion order, with zero first.',
		argnames is ['STN', 'TimePoints']
	]).

	:- public(constraints/2).
	:- mode(constraints(+stn, -list(compound)), zero_or_one).
	:- info(constraints/2, [
		comment is 'Returns all source constraints in insertion order as ``constraint(Id, X, Y, Delta)`` terms, including redundant and parallel constraints.',
		argnames is ['STN', 'Constraints']
	]).

	:- public(consistent/1).
	:- mode(consistent(+stn), zero_or_one).
	:- info(consistent/1, [
		comment is 'True if the argument is a library-owned consistent state. All successfully returned states are consistent; this predicate is not an importer for hand-built internal terms.',
		argnames is ['STN']
	]).

	:- public(schedule/2).
	:- mode(schedule(+stn, -list(compound)), zero_or_one_or_error).
	:- info(schedule/2, [
		comment is 'Returns one finite feasible assignment as time(Point, Value) terms in declaration order, with time(zero, 0) first. Fails for invalid states without instantiating inputs.',
		argnames is ['STN', 'Schedule'],
		remarks is [
			'Choice' - 'The assignment is deterministic, not necessarily unique, earliest, non-negative, or optimal. Disconnected and absolutely unbounded points are assigned finite values.',
			'Numbers' - 'Every retained source inequality is checked after normalization using backend arithmetic. Floating-point results remain approximate, with no implicit tolerance.'
		],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(earliest_schedule/2).
	:- mode(earliest_schedule(+stn, -list(compound)), zero_or_one_or_error).
	:- info(earliest_schedule/2, [
		comment is 'Returns the componentwise earliest feasible assignment as time(Point, Value) terms in declaration order, with time(zero, 0) first. Requires every declared point to have a finite absolute lower bound. Fails for invalid states or any unbounded lower bound without instantiating inputs.',
		argnames is ['STN', 'Schedule'],
		remarks is [
			'Bounds' - 'Finite upper bounds are not required. No artificial lower bounds are introduced and the original state and source identifiers are unchanged.',
			'Numbers' - 'Componentwise minimality holds in exact arithmetic. Every retained source inequality is checked using backend arithmetic; floating-point feasibility and optimality are not exact-real guarantees and no tolerance is applied.'
		],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(add_constraint/5).
	:- mode(add_constraint(+stn, @term, @term, +number, -stn), zero_or_one_or_error).
	:- info(add_constraint/5, [
		comment is 'Adds ``Y - X =< Delta``, retaining the source even when redundant. Fails if the result is inconsistent or the inputs are invalid.',
		argnames is ['STN0', 'X', 'Y', 'Delta', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(add_constraint/6).
	:- mode(add_constraint(+stn, @term, @term, +number, -integer, -stn), zero_or_one_or_error).
	:- info(add_constraint/6, [
		comment is 'Adds ``Y - X =< Delta`` and returns its source identifier. Fails for an inconsistent result or invalid inputs. Identifiers are local to a state lineage, not globally unique across forks.',
		argnames is ['STN0', 'X', 'Y', 'Delta', 'Id', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(add_constraints/3).
	:- mode(add_constraints(+stn, +list(compound), -stn), zero_or_one_or_error).
	:- info(add_constraints/3, [
		comment is 'Atomically adds a batch of ``constraint(X, Y, Delta)`` terms, rebuilding once. Fails if any input is invalid or the combined network is inconsistent. An empty batch returns the original state.',
		argnames is ['STN0', 'Constraints', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(add_constraints/4).
	:- mode(add_constraints(+stn, +list(compound), -list(integer), -stn), zero_or_one_or_error).
	:- info(add_constraints/4, [
		comment is 'Atomically adds ``constraint(X, Y, Delta)`` terms and returns their source identifiers in batch order. Fails for invalid inputs or an inconsistent result.',
		argnames is ['STN0', 'Constraints', 'Ids', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(try_add_constraints/4).
	:- mode(try_add_constraints(+stn, +list(compound), -list(integer), -compound), zero_or_one_or_error).
	:- info(try_add_constraints/4, [
		comment is 'Attempts a batch update and returns ``consistent(STN)`` or ``inconsistent(cycle(Sources, Weight))``. The cycle is an ordered closed path of identified source constraints with negative total weight, not necessarily a minimal conflict. Invalid inputs fail.',
		argnames is ['STN0', 'Constraints', 'Ids', 'Outcome'],
		remarks is [
			'Identifiers' - 'Ids identifies candidate constraints in batch order even when rejected. No state is changed; rejected identifiers may coincide with later accepted allocations from the original state.',
			'Arithmetic' - 'Backend arithmetic errors propagate. Non-finite computed sums raise evaluation_error(float_overflow); inconsistent numerical diagnostics raise evaluation_error(stn_numerical_inconsistency).'
		],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(remove_constraints/3).
	:- mode(remove_constraints(+stn, +list(integer), -stn), zero_or_one_or_error).
	:- info(remove_constraints/3, [
		comment is 'Removes source constraints by positive integer identifier, rebuilding once if necessary. Missing identifiers and duplicate requests are ignored. Surviving identifiers are preserved and removed identifiers are never reused in descendant states.',
		argnames is ['STN0', 'Ids', 'STN'],
		exceptions is [
			'Arithmetic cannot be evaluated' - evaluation_error(_),
			'Arithmetic exceeds a backend representation limit' - representation_error(_)
		]
	]).

	:- public(distance/4).
	:- mode(distance(+stn, @term, @term, -bound), zero_or_one).
	:- info(distance/4, [
		comment is 'Returns the tightest upper bound on ``Y - X``, or positive_infinity if no directed path exists.',
		argnames is ['STN', 'X', 'Y', 'Upper']
	]).

	:- public(distance/5).
	:- mode(distance(+stn, @term, @term, -number, -list(compound)), zero_or_one_or_error).
	:- info(distance/5, [
		comment is 'Returns a finite tight upper bound on ``Y - X`` and one ordered supporting source-constraint path. Fails for an unreachable pair. An identity query returns zero and an empty path. Ties select one deterministic witness, not every support.',
		argnames is ['STN', 'X', 'Y', 'Upper', 'Path'],
		exceptions is [
			'A numerical witness cannot be reconstructed' - evaluation_error(stn_numerical_inconsistency)
		]
	]).

	:- public(entails/4).
	:- mode(entails(+stn, @term, @term, +number), zero_or_one).
	:- info(entails/4, [
		comment is 'True if the network entails ``Y - X =< Delta`` for a finite integer or float Delta.',
		argnames is ['STN', 'X', 'Y', 'Delta']
	]).

	:- public(entails/5).
	:- mode(entails(+stn, @term, @term, +number, -list(compound)), zero_or_one).
	:- info(entails/5, [
		comment is 'True if the network entails ``Y - X =< Delta`` and returns one supporting source-constraint path.',
		argnames is ['STN', 'X', 'Y', 'Delta', 'Path'],
		exceptions is [
			'A numerical witness cannot be reconstructed' - evaluation_error(stn_numerical_inconsistency)
		]
	]).

	:- public(earliest/3).
	:- mode(earliest(+stn, @term, -bound), zero_or_one).
	:- info(earliest/3, [
		comment is 'Returns the earliest feasible time relative to zero, or negative_infinity when unbounded.',
		argnames is ['STN', 'TimePoint', 'Earliest']
	]).

	:- public(latest/3).
	:- mode(latest(+stn, @term, -bound), zero_or_one).
	:- info(latest/3, [
		comment is 'Returns the latest feasible time relative to zero, or positive_infinity when unbounded.',
		argnames is ['STN', 'TimePoint', 'Latest']
	]).

	:- public(bounds/4).
	:- mode(bounds(+stn, @term, -bound, -bound), zero_or_one).
	:- info(bounds/4, [
		comment is 'Returns the tightest absolute time window, including symbolic unbounded endpoints. The reference point zero has bounds ``[0, 0]``.',
		argnames is ['STN', 'TimePoint', 'Earliest', 'Latest']
	]).

	:- public(difference_bounds/5).
	:- mode(difference_bounds(+stn, @term, @term, -bound, -bound), zero_or_one).
	:- info(difference_bounds/5, [
		comment is 'Returns tight bounds ``Lower =< Y - X =< Upper``. ``Lower`` is the negated reverse distance, ``Upper`` the forward distance. Either endpoint may be symbolically unbounded.',
		argnames is ['STN', 'X', 'Y', 'Lower', 'Upper']
	]).

	:- public(can_precede/3).
	:- mode(can_precede(+stn, @term, @term), zero_or_one).
	:- info(can_precede/3, [
		comment is 'True if some feasible schedule has ``X`` strictly earlier than ``Y``. Tests whether the upper bound on ``Y - X`` is positive or unbounded.',
		argnames is ['STN', 'X', 'Y']
	]).

	:- public(must_precede/3).
	:- mode(must_precede(+stn, @term, @term), zero_or_one).
	:- info(must_precede/3, [
		comment is 'True if every feasible schedule has ``X`` strictly earlier than ``Y``. Tests whether the lower bound on ``Y - X`` is positive.',
		argnames is ['STN', 'X', 'Y']
	]).

	:- public(can_precede_or_equal/3).
	:- mode(can_precede_or_equal(+stn, @term, @term), zero_or_one).
	:- info(can_precede_or_equal/3, [
		comment is 'True if some feasible schedule has ``X`` no later than ``Y``. Tests whether the upper bound on ``Y - X`` is non-negative or unbounded.',
		argnames is ['STN', 'X', 'Y']
	]).

	:- public(must_precede_or_equal/3).
	:- mode(must_precede_or_equal(+stn, @term, @term), zero_or_one).
	:- info(must_precede_or_equal/3, [
		comment is 'True if every feasible schedule has ``X`` no later than ``Y``. Tests whether the lower bound on ``Y - X`` is non-negative.',
		argnames is ['STN', 'X', 'Y']
	]).

	:- public(window_interval/3).
	:- mode(window_interval(+stn, @term, -compound), zero_or_one).
	:- info(window_interval/3, [
		comment is 'Returns a finite non-degenerate marginal time-point window as i(Start, End), preserving endpoint numeric representations. Fails for unknown points, invalid states, and unbounded or singleton windows.',
		argnames is ['STN', 'TimePoint', 'Interval'],
		remarks is [
			'Numeric ordering' - 'The descriptor uses arithmetic endpoint ordering, not term ordering. It is numeric data, not a portable replacement for the ranked interval bridge.',
			'Envelope' - 'Describes marginal absolute bounds, not an event duration or joint endpoint correlations.'
		]
	]).

	:- public(event_interval/4).
	:- mode(event_interval(+stn, @term, @term, -compound), zero_or_one).
	:- info(event_interval/4, [
		comment is 'Returns a concrete event interval as i(Start, End), preserving endpoint numeric representations. Both points must have finite singleton absolute bounds and the duration must be strictly positive. Fails for invalid states and unknown, uncertain, unbounded, reversed, or zero-duration endpoints.',
		argnames is ['STN', 'StartPoint', 'EndPoint', 'Interval'],
		remarks is [
			'Numeric ordering' - 'The descriptor uses arithmetic endpoint ordering, not term ordering. Use event_relation/6 for the portable ranked Allen-relation bridge.'
		]
	]).

	:- public(window_relation/4).
	:- mode(window_relation(+stn, @term, @term, -atom), zero_or_one).
	:- info(window_relation/4, [
		comment is 'Classifies two finite, non-degenerate marginal time-point windows using Allen relations. Fails for unbounded or singleton windows. This is an envelope diagnostic, not an entailed relation between uncertain events.',
		argnames is ['STN', 'TimePoint1', 'TimePoint2', 'Relation'],
		remarks is [
			'Numeric ordering' - 'Delegates to interval after jointly ranking endpoints by arithmetic value, preserving numeric equality across integers and floats.'
		]
	]).

	:- public(event_relation/6).
	:- mode(event_relation(+stn, @term, @term, @term, @term, -atom), zero_or_one).
	:- info(event_relation/6, [
		comment is 'Classifies two concrete event intervals using Allen relations. All four endpoint time-points must have finite singleton absolute bounds and each event must have strictly positive duration. Fails for uncertain, unbounded, reversed, or zero-duration events.',
		argnames is ['STN', 'Start1', 'End1', 'Start2', 'End2', 'Relation'],
		remarks is [
			'Numeric ordering' - 'Delegates to interval after jointly ranking endpoints by arithmetic value, without converting integer endpoints to floats.'
		]
	]).

:- end_protocol.
