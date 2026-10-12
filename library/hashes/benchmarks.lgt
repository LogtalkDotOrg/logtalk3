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


:- object(hashes_benchmarks).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-12,
		comment is 'Uninstrumented hash benchmarks using five CPU-time samples after warmup.'
	]).

	:- public(run/3).
	:- mode(run(+object_identifier, +integer, +integer), one).
	:- info(run/3, [
		comment is 'Prints one-shot and incremental CPU-time samples and medians for a byte count and positive chunk size. Load the optimized hashes loader and the os library before this object.',
		argnames is ['Object', 'ByteCount', 'ChunkSize']
	]).

	:- uses(list, [
		take/4, msort/2
	]).

	:- uses(os, [
		cpu_time/1
	]).

	run(Object, ByteCount, ChunkSize) :-
		bytes(ByteCount, 0, Bytes),
		chunks(Bytes, ChunkSize, Chunks),
		Object::hash(Bytes, Expected),
		incremental(Object, Chunks, Expected),
		samples(5, Object, Bytes, Chunks, Times),
		medians(Times, OneShot, Incremental),
		writeq(benchmark(Object, ByteCount, ChunkSize, cpu_seconds(OneShot, Incremental), Times)),
		nl.

	bytes(0, _, []) :-
		!.
	bytes(Count, Byte, [Byte| Bytes]) :-
		NextCount is Count - 1,
		NextByte is (Byte + 1) mod 256,
		bytes(NextCount, NextByte, Bytes).

	chunks([], _, []) :-
		!.
	chunks(Bytes, Size, [Chunk| Chunks]) :-
		(	take(Size, Bytes, Chunk, Rest) ->
			true
		;	Chunk = Bytes,
			Rest = []
		),
		chunks(Rest, Size, Chunks).

	incremental(Object, Chunks, Hash) :-
		Object::new_hash_state(State0),
		updates(Chunks, Object, State0, State),
		Object::final_hash_state(State, Hash).

	updates([], _, State, State).
	updates([Chunk| Chunks], Object, State0, State) :-
		Object::update_hash_state(State0, Chunk, State1),
		updates(Chunks, Object, State1, State).

	samples(0, _, _, _, []) :-
		!.
	samples(Count, Object, Bytes, Chunks, [OneShot-Incremental| Times]) :-
		cpu_time(Start),
		Object::hash(Bytes, _),
		cpu_time(Middle),
		incremental(Object, Chunks, _),
		cpu_time(End),
		OneShot is Middle - Start,
		Incremental is End - Middle,
		NextCount is Count - 1,
		samples(NextCount, Object, Bytes, Chunks, Times).

	medians(Times, OneShot, Incremental) :-
		columns(Times, OneShots, Incrementals),
		msort(OneShots, [_, _, OneShot, _, _]),
		msort(Incrementals, [_, _, Incremental, _, _]).

	columns([], [], []).
	columns([OneShot-Incremental| Times], [OneShot| OneShots], [Incremental| Incrementals]) :-
		columns(Times, OneShots, Incrementals).

:- end_object.
