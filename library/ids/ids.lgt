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


:- object(ids(_Representation_, _Bytes_)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Generator of random identifiers.',
		parameters is [
			'Representation' - 'Text representation for the identifier. Possible values are ``atom``, ``chars``, and ``codes``.',
			'Bytes' - 'Number of bytes of randomness.'
		],
		see_also is [ids, nanoid(_, _, _), uuid(_), ulid(_)]
	]).

	:- public(generate/1).
	:- mode(generate(--textids), one).
	:- info(generate/1, [
		comment is 'Generate a random identifier.',
		argnames is ['Identifier']
	]).

	generate(Identifier) :-
		generate(_Representation_, Identifier).

	generate(atom, Identifier) :-
		random_bytes(_Bytes_, Bytes),
		base64::generate(atom(Identifier), Bytes).
	generate(chars, Identifier) :-
		random_bytes(_Bytes_, Bytes),
		base64::generate(chars(Identifier), Bytes).
	generate(codes, Identifier) :-
		random_bytes(_Bytes_, Bytes),
		base64::generate(codes(Identifier), Bytes).

	random_bytes(N, Bytes) :-
		catch(open('/dev/urandom', read, Stream, [type(binary)]), _, fail),
		list::length(Bytes, N),
		read_random_bytes(Bytes, Stream),
		close(Stream),
		!.
	random_bytes(N, Bytes) :-
		os::wall_time(Time),
		Seed is round(Time),
		fast_random::randomize(Seed),
		fast_random::sequence(N, 0, 255, Bytes).

	read_random_bytes([], _).
	read_random_bytes([Byte| Bytes], Stream) :-
		get_byte(Stream, Byte),
		read_random_bytes(Bytes, Stream).

:- end_object.


:- object(ids,
	extends(ids(atom, 20))).

	:- info([
		version is 1:2:0,
		author is 'Paulo Moura',
		date is 2026-02-26,
		comment is 'Generator of random identifiers represented as atoms with 160 bits (20 bytes) of randomness.',
		see_also is [ids(_, _), nanoid, uuid, ulid]
	]).

:- end_object.
