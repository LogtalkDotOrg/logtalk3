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


:- object(snowflakeid(_Representation_, _EpochMilliseconds_, _TimeUnitMilliseconds_, _TimestampBits_, _NodeBits_, _SequenceBits_, _Node_),
	implements(snowflakeid_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Snowflake ID generic generator.',
		parameters is [
			'Representation' - 'Snowflake ID representation. Possible values are ``integer``, ``atom``, ``chars``, and ``codes``.',
			'EpochMilliseconds' - 'Custom epoch in Unix milliseconds.',
			'TimeUnitMilliseconds' - 'Timestamp unit in milliseconds.',
			'TimestampBits' - 'Number of timestamp bits.',
			'NodeBits' - 'Number of node bits.',
			'SequenceBits' - 'Number of sequence bits.',
			'Node' - 'Node identifier value.'
		],
		see_also is [
			snowflakeid, snowflakeid_twitter, snowflakeid_sonyflake, snowflakeid_instagram, ksuid(_, _),
			cuid2(_, _, _), nanoid(_, _, _)
		]
	]).

	:- uses(iso8601, [
		date/4
	]).

	:- uses(os, [
		date_time/7, sleep/1
	]).

	:- private(last_time_sequence_/2).
	:- dynamic(last_time_sequence_/2).

	generate(ID) :-
		context(Context),
		check_representation(Context),
		check_configuration(Context),
		current_time_unit(Context, CurrentTime),
		next_time_sequence(Context, CurrentTime, Time, Sequence),
		pack_id(Time, Sequence, IntegerID),
		integer_to_representation(_Representation_, IntegerID, ID).

	check_representation(Context) :-
		(   var(_Representation_) ->
			throw(error(instantiation_error, Context))
		;   _Representation_ == integer ->
			true
		;   _Representation_ == atom ->
			true
		;   _Representation_ == chars ->
			true
		;   _Representation_ == codes ->
			true
		;   throw(error(domain_error(snowflakeid_representation, _Representation_), Context))
		).

	check_configuration(Context) :-
		check_epoch(Context),
		check_time_unit(Context),
		check_bits(Context),
		check_node(Context).

	check_epoch(Context) :-
		(   var(_EpochMilliseconds_) ->
			throw(error(instantiation_error, Context))
		;   integer(_EpochMilliseconds_) ->
			(   _EpochMilliseconds_ >= 0 ->
				true
			;   throw(error(domain_error(not_less_than_zero, _EpochMilliseconds_), Context))
			)
		;   throw(error(type_error(integer, _EpochMilliseconds_), Context))
		).

	check_time_unit(Context) :-
		(   var(_TimeUnitMilliseconds_) ->
			throw(error(instantiation_error, Context))
		;   integer(_TimeUnitMilliseconds_) ->
			(   _TimeUnitMilliseconds_ > 0 ->
				true
			;   throw(error(domain_error(not_less_than_one, _TimeUnitMilliseconds_), Context))
			)
		;   throw(error(type_error(integer, _TimeUnitMilliseconds_), Context))
		).

	check_bits(Context) :-
		check_bit_size(Context, _TimestampBits_),
		check_bit_size(Context, _NodeBits_),
		check_bit_size(Context, _SequenceBits_),
		TotalBits is _TimestampBits_ + _NodeBits_ + _SequenceBits_,
		(   TotalBits =< 64 ->
			true
		;   throw(error(domain_error(snowflakeid_layout, layout(_TimestampBits_, _NodeBits_, _SequenceBits_)), Context))
		).

	check_bit_size(Context, Bits) :-
		(   var(Bits) ->
			throw(error(instantiation_error, Context))
		;   integer(Bits) ->
			(   Bits > 0 ->
				true
			;   throw(error(domain_error(not_less_than_one, Bits), Context))
			)
		;   throw(error(type_error(integer, Bits), Context))
		).

	check_node(Context) :-
		(   var(_Node_) ->
			throw(error(instantiation_error, Context))
		;   integer(_Node_) ->
			MaxNode is (1 << _NodeBits_) - 1,
			(   _Node_ >= 0, _Node_ =< MaxNode ->
				true
			;   throw(error(domain_error(snowflakeid_node, _Node_), Context))
			)
		;   throw(error(type_error(integer, _Node_), Context))
		).

	current_time_unit(Context, TimeUnit) :-
		current_timestamp_milliseconds(CurrentTimestamp),
		DeltaMilliseconds is CurrentTimestamp - _EpochMilliseconds_,
		(   DeltaMilliseconds >= 0 ->
			TimeUnit is DeltaMilliseconds // _TimeUnitMilliseconds_,
			MaxTimestamp is (1 << _TimestampBits_) - 1,
			(   TimeUnit =< MaxTimestamp ->
				true
			;   throw(error(domain_error(snowflakeid_timestamp, TimeUnit), Context))
			)
		;   throw(error(domain_error(snowflakeid_epoch, _EpochMilliseconds_), Context))
		).

	next_time_sequence(Context, CurrentTime, Time, Sequence) :-
		MaxSequence is (1 << _SequenceBits_) - 1,
		(   ::retract(last_time_sequence_(LastTime, LastSequence)) ->
			true
		;   LastTime = -1,
			LastSequence = -1
		),
		next_time_sequence(Context, CurrentTime, LastTime, LastSequence, MaxSequence, Time, Sequence),
		::asserta(last_time_sequence_(Time, Sequence)).

	next_time_sequence(Context, CurrentTime, LastTime, _LastSequence, _MaxSequence, Time, Sequence) :-
		CurrentTime < LastTime,
		!,
		wait_next_time_unit(Context, LastTime, Time),
		Sequence = 0.
	next_time_sequence(_Context, CurrentTime, LastTime, LastSequence, MaxSequence, Time, Sequence) :-
		CurrentTime =:= LastTime,
		!,
		NextSequence is LastSequence + 1,
		(   NextSequence =< MaxSequence ->
			Time = CurrentTime,
			Sequence = NextSequence
		;   wait_next_time_unit(_Context, LastTime, Time),
			Sequence = 0
		).
	next_time_sequence(_Context, CurrentTime, _LastTime, _LastSequence, _MaxSequence, CurrentTime, 0).

	wait_next_time_unit(Context, LastTime, Time) :-
		current_time_unit(Context, CurrentTime),
		(   CurrentTime > LastTime ->
			Time = CurrentTime
		;   sleep(0.001),
			wait_next_time_unit(Context, LastTime, Time)
		).

	pack_id(Time, Sequence, IntegerID) :-
		ShiftTime is _NodeBits_ + _SequenceBits_,
		ShiftNode is _SequenceBits_,
		IntegerID is (Time << ShiftTime) + (_Node_ << ShiftNode) + Sequence.

	integer_to_representation(integer, IntegerID, IntegerID).
	integer_to_representation(atom, IntegerID, ID) :-
		number_codes(IntegerID, Codes),
		atom_codes(ID, Codes).
	integer_to_representation(chars, IntegerID, ID) :-
		number_codes(IntegerID, Codes),
		codes_to_chars(Codes, ID).
	integer_to_representation(codes, IntegerID, ID) :-
		number_codes(IntegerID, ID).

	current_timestamp_milliseconds(Timestamp) :-
		date_time(Year, Month, Day, Hours, Minutes, Seconds, Milliseconds),
		date(UnixEpoch, 1970, 1, 1),
		date(CurrentDate, Year, Month, Day),
		Days is CurrentDate - UnixEpoch,
		Timestamp is Days * 86400000 + Hours * 3600000 + Minutes * 60000 + Seconds * 1000 + Milliseconds.

	codes_to_chars([], []).
	codes_to_chars([Code| Codes], [Char| Chars]) :-
		char_code(Char, Code),
		codes_to_chars(Codes, Chars).

:- end_object.


:- object(snowflakeid_twitter(_Representation_),
	extends(snowflakeid(_Representation_, 1288834974657, 1, 41, 10, 12, 1))).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Twitter-style Snowflake profile.',
		parameters is [
			'Representation' - 'Snowflake ID representation. Possible values are ``integer``, ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [
			snowflakeid(_, _, _, _, _, _, _), snowflakeid_twitter, snowflakeid_sonyflake, snowflakeid_instagram
		]
	]).

:- end_object.


:- object(snowflakeid_twitter,
	extends(snowflakeid_twitter(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Twitter-style Snowflake profile using atom representation.',
		see_also is [snowflakeid_twitter(_), snowflakeid_sonyflake, snowflakeid_instagram]
	]).

:- end_object.


:- object(snowflakeid_sonyflake(_Representation_),
	extends(snowflakeid(_Representation_, 1409529600000, 10, 39, 16, 8, 1))).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Sonyflake-style Snowflake profile.',
		parameters is [
			'Representation' - 'Snowflake ID representation. Possible values are ``integer``, ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [
			snowflakeid(_, _, _, _, _, _, _), snowflakeid_twitter, snowflakeid_sonyflake, snowflakeid_instagram
		]
	]).

:- end_object.


:- object(snowflakeid_sonyflake,
	extends(snowflakeid_sonyflake(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Sonyflake-style Snowflake profile using atom representation.',
		see_also is [snowflakeid_sonyflake(_), snowflakeid_twitter, snowflakeid_instagram]
	]).

:- end_object.


:- object(snowflakeid_instagram(_Representation_),
	extends(snowflakeid(_Representation_, 1314220021721, 1, 41, 13, 10, 1))).

	:- info([
		version is 1:0:1,
		author is 'Paulo Moura',
		date is 2026-02-27,
		comment is 'Instagram-style Snowflake profile.',
		parameters is [
			'Representation' - 'Snowflake ID representation. Possible values are ``integer``, ``atom``, ``chars``, and ``codes``.'
		],
		see_also is [
			snowflakeid(_, _, _, _, _, _, _), snowflakeid_twitter, snowflakeid_sonyflake, snowflakeid_instagram
		]
	]).

:- end_object.


:- object(snowflakeid_instagram,
	extends(snowflakeid_instagram(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Instagram-style Snowflake profile using atom representation.',
		see_also is [snowflakeid_instagram(_), snowflakeid_twitter, snowflakeid_sonyflake]
	]).

:- end_object.


:- object(snowflakeid,
	extends(snowflakeid_twitter(atom))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Snowflake ID generator using the Twitter-style profile and atom representation.',
		see_also is [
			snowflakeid(_, _, _, _, _, _, _), snowflakeid_twitter, snowflakeid_sonyflake, snowflakeid_instagram
		]
	]).

:- end_object.
