%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(buckets).

	:- info([
		version is 1:4:0,
		author is 'Paulo Moura',
		date is 2024-02-09,
		comment is 'Example of atomic updates as described in the corresponding Rosetta Code task.'
	]).

	:- threaded.

	:- public([start/0, start/4]).

	% for testing
	:- public([sum/1, bucket/1]).
	:- dynamic([sum/1, bucket/1]).

	% bucket representation
	:- private(bucket_/2).
	:- dynamic(bucket_/2).

	% use the same mutex for all the predicates that access the buckets
	:- private([bucket/2, buckets/1, transfer/3]).
	:- synchronized([bucket/2, buckets/1, transfer/3]).

	:- uses(format, [format/2]).
	% use the backend compiler random number generator, assumed to be stateless
	:- uses(backend_random, [random/3]).

	start :-
		% cleanup test support predicates
		retractall(sum(_)),
		retractall(bucket(_)),
		% by default, create ten buckets with initial random integer values
		% in the interval [0, 10[ and print their contents ten times
		start(10, 0, 10, 10).

	start(N, Min, Max, Samples) :-
		% create the buckets with random values in the
		% interval [Min, Max[ and return their sum
		create_buckets(N, Min, Max, Sum),
		format('Sum of all bucket values: ~w~n~n', [Sum]),
		assertz(sum(Sum)),
		% use competitive or-parallelism for the three loops such that
		% the computations terminate when the display loop terminates
		threaded((
				display_loop(Samples)
			;	match_loop(N)
			;	redistribute_loop(N)
		)).

	create_buckets(N, Min, Max, Sum) :-
		% remove all existing buckets
		retractall(bucket_(_,_)),
		% create the new buckets
		create_buckets(N, Min, Max, 0, Sum).

	create_buckets(0, _, _, Sum, Sum) :-
		!.
	create_buckets(N, Min, Max, Sum0, Sum) :-
		random(Min, Max, Value),
		asserta(bucket_(N,Value)),
		M is N - 1,
		Sum1 is Sum0 + Value,
		create_buckets(M, Min, Max, Sum1, Sum).

	bucket(Bucket, Value) :-
		bucket_(Bucket, Value).

	buckets(Values) :-
		findall(Value, bucket_(_, Value), Values).

	transfer(Origin, _, Origin) :-
		!.
	transfer(Origin, Delta, Destination) :-
		retract(bucket_(Origin, OriginValue)),
		retract(bucket_(Destination, DestinationValue)),
		% the buckets may have changed between the access to its
		% values and the calling of this transfer predicate; thus,
		% we must ensure that we're transferring a legal amount
		Amount is min(Delta, OriginValue),
		NewOriginValue is OriginValue - Amount,
		NewDestinationValue is DestinationValue + Amount,
		assertz(bucket_(Origin, NewOriginValue)),
		assertz(bucket_(Destination, NewDestinationValue)).

	match_loop(N) :-
		% randomly select two buckets
		M is N + 1,
		random(1, M, Bucket1),
		random(1, M, Bucket2),
		% access their contents
		bucket(Bucket1, Value1),
		bucket(Bucket2, Value2),
		% make their new values approximately equal
		Delta is truncate(abs(Value1 - Value2)/2),
		(	Value1 > Value2 ->
			transfer(Bucket1, Delta, Bucket2)
		;	Value1 < Value2 ->
			transfer(Bucket2, Delta, Bucket1)
		;	true
		),
		thread_yield,
		match_loop(N).

	redistribute_loop(N) :-
		% randomly select two buckets
		M is N + 1,
		random(1, M, FromBucket),
		random(1, M, ToBucket),
		% access bucket from where we transfer
		bucket(FromBucket, Current),
		Limit is Current + 1,
		random(0, Limit, Delta),
		transfer(FromBucket, Delta, ToBucket),
		thread_yield,
		redistribute_loop(N).

	display_loop(0) :-
		!.
	display_loop(N) :-
		buckets(Values),
		format('~w~n', [Values]),
		assertz(bucket(Values)),
		thread_sleep(0.5),
		M is N - 1,
		display_loop(M).

	:- if(\+ predicate_property(thread_yield, built_in)).
		thread_yield.
	:- endif.

:- end_object.
