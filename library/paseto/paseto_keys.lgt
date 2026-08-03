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


:- object(paseto_keys).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-03,
		comment is 'Validation and selection for native PASETO v4 key sets.'
	]).

	:- public(validate/1).
	:- mode(validate(+compound), one_or_error).
	:- info(validate/1, [
		comment is 'Validates a key_set/1 term containing local/2 and public/2 records.',
		argnames is ['KeySet']
	]).

	:- public(select_keys/4).
	:- mode(select_keys(+compound, +atom, +term, -list(list(byte))), one_or_error).
	:- info(select_keys/4, [
		comment is 'Selects all keys matching Purpose and optional KeyId, preserving key-set order.',
		argnames is ['KeySet', 'Purpose', 'KeyId', 'Keys']
	]).

	:- uses(list, [
		length/2
	]).

	validate(key_set(Keys)) :-
		list::valid(Keys),
		validate_keys(Keys),
		!.
	validate(KeySet) :-
		domain_error(paseto_key_set, KeySet).

	select_keys(KeySet, Purpose, KeyId, Keys) :-
		validate(KeySet),
		once((Purpose == local; Purpose == public)),
		once((KeyId == none; atom(KeyId))),
		KeySet = key_set(Records),
		select_records(Records, Purpose, KeyId, Keys),
		(	Keys = [_| _] ->
			true
		;	existence_error(paseto_key, Purpose-KeyId)
		).

	validate_keys([]) :-
		!.
	validate_keys([Record| Records]) :-
		valid_record(Record),
		validate_keys(Records).

	valid_record(local(KeyId, Key)) :-
		atom(KeyId), valid_key(Key), !.
	valid_record(public(KeyId, Key)) :-
		atom(KeyId), valid_key(Key), !.
	valid_record(Record) :-
		domain_error(paseto_key_record, Record).

	valid_key(Key) :-
		list::valid(Key),
		length(Key, 32),
		valid_bytes(Key).

	valid_bytes([]) :-
		!.
	valid_bytes([Byte| Bytes]) :-
		integer(Byte), Byte >= 0, Byte =< 255,
		valid_bytes(Bytes).

	select_records([], _, _, []) :-
		!.
	select_records([Record| Records], Purpose, KeyId, Keys) :-
		(	matching_record(Record, Purpose, KeyId, Key) ->
			Keys = [Key| Rest]
		;	Keys = Rest
		),
		select_records(Records, Purpose, KeyId, Rest).

	matching_record(local(Id, Key), local, KeyId, Key) :-
		matching_id(Id, KeyId).
	matching_record(public(Id, Key), public, KeyId, Key) :-
		matching_id(Id, KeyId).

	matching_id(_, none) :-
		!.
	matching_id(Id, Id).

:- end_object.
