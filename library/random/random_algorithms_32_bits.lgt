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


	random_seeds(as183, s(S0, S1, S2), s(NewS0, NewS1, NewS2), Random) :-
		NewS0 is (S0*171) mod 30269,
		NewS1 is (S1*172) mod 30307,
		NewS2 is (S2*170) mod 30323,
		% as some Prolog backends may return Float as an integer or a rational
		% number, we explicitly convert the value into a float in the next goal
		Float is S0/30269 + S1/30307 + S2/30323,
		Random is float(Float) - truncate(Float).
	% Core Xoshiro128++ algorithm
	random_seeds(xoshiro128pp, s(S0, S1, S2, S3), s(NewS0, NewS1, NewS2, NewS3), Random) :-
		mask32(Mask),
		% IntRandom = rotl(s0 + s3, 7) + s0
		Sum1 is (S0 + S3) /\ Mask,
		rotl32(Sum1, 7, Rotated),
		IntRandom is (Rotated + S0) /\ Mask,
		% t = s1 << 9
		T is (S1 << 9) /\ Mask,
		% Update state according to reference implementation:
		% s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
		% s[2] ^= t; s[3] = rotl(s[3], 11);
		TmpS2 is xor(S2, S0),
		TmpS3 is xor(S3, S1),
		NewS1 is xor(S1, TmpS2),
		NewS0 is xor(S0, TmpS3),
		NewS2 is xor(TmpS2, T),
		rotl32(TmpS3, 11, NewS3),
		% Convert to float in [0.0, 1.0)
		Random is IntRandom / 4294967296.0.
	% Core Xoshiro128** algorithm
	random_seeds(xoshiro128ss, s(S0, S1, S2, S3), s(NewS0, NewS1, NewS2, NewS3), Random) :-
		mask32(Mask),
		% IntRandom = rotl(s1 * 5, 7) * 9
		Mul1 is (S1 * 5) /\ Mask,
		rotl32(Mul1, 7, Rotated),
		IntRandom is (Rotated * 9) /\ Mask,
		% t = s1 << 9
		T is (S1 << 9) /\ Mask,
		% Update state according to reference implementation:
		% s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
		% s[2] ^= t; s[3] = rotl(s[3], 11);
		TmpS2 is xor(S2, S0),
		TmpS3 is xor(S3, S1),
		NewS1 is xor(S1, TmpS2),
		NewS0 is xor(S0, TmpS3),
		NewS2 is xor(TmpS2, T),
		rotl32(TmpS3, 11, NewS3),
		% Convert to float in [0.0, 1.0)
		Random is IntRandom / 4294967296.0.
	% Core WELL512a algorithm (Panneton, L'Ecuyer, and Matsumoto)
	random_seeds(well512a, s(Index, InnerState), s(NewIndex, NewInnerState), Random) :-
		mask32(Mask),
		well512a_state(Index, InnerState, V0, VM1, VM2, Z0, NewV1, NewV0, NewInnerState, NewIndex),
		% z1 = MAT0NEG(-16, V0) ^ MAT0NEG(-15, VM1)
		Z1 is xor(xor(V0, (V0 << 16) /\ Mask), xor(VM1, (VM1 << 15) /\ Mask)),
		% z2 = MAT0POS(11, VM2)
		Z2 is xor(VM2, VM2 >> 11),
		% newV1 = z1 ^ z2
		NewV1 is xor(Z1, Z2),
		% newV0 = MAT0NEG(-2,z0) ^ MAT0NEG(-18,z1) ^ MAT3NEG(-28,z2) ^ MAT4NEG(-5,0xDA442D24,newV1)
		T1 is xor(Z0, (Z0 << 2) /\ Mask),
		T2 is xor(Z1, (Z1 << 18) /\ Mask),
		T3 is (Z2 << 28) /\ Mask,
		T4 is xor(NewV1, (NewV1 << 5) /\ Mask /\ 3661901092),
		NewV0 is xor(xor(xor(T1, T2), T3), T4) /\ Mask,
		% Output is newV0; convert to float in [0.0, 1.0)
		Random is NewV0 / 4294967296.0.

	randomize(as183, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(as183, _)),
		S0 is Seed mod 30269,
		S1 is Seed mod 30307,
		S2 is Seed mod 30323,
		::assertz(seed_(as183, s(S0, S1, S2))).
	randomize(xoshiro128pp, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(xoshiro128pp, _)),
		mask32(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 32) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 64) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 96) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			::assertz(seed_(xoshiro128pp, s(1, 1, 1, 1)))
		;	::assertz(seed_(xoshiro128pp, s(S0, S1, S2, S3)))
		).
	randomize(xoshiro128ss, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(xoshiro128ss, _)),
		mask32(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 32) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 64) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 96) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			::assertz(seed_(xoshiro128ss, s(1, 1, 1, 1)))
		;	::assertz(seed_(xoshiro128ss, s(S0, S1, S2, S3)))
		).
	randomize(well512a, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(well512a, _)),
		mask32(Mask),
		% Initialize state using MT19937-style initialization (Knuth's multiplicative hash)
		S0 is Seed /\ Mask,
		S1 is (1812433253 * xor(S0, S0 >> 30) + 1) /\ Mask,
		S2 is (1812433253 * xor(S1, S1 >> 30) + 2) /\ Mask,
		S3 is (1812433253 * xor(S2, S2 >> 30) + 3) /\ Mask,
		S4 is (1812433253 * xor(S3, S3 >> 30) + 4) /\ Mask,
		S5 is (1812433253 * xor(S4, S4 >> 30) + 5) /\ Mask,
		S6 is (1812433253 * xor(S5, S5 >> 30) + 6) /\ Mask,
		S7 is (1812433253 * xor(S6, S6 >> 30) + 7) /\ Mask,
		S8 is (1812433253 * xor(S7, S7 >> 30) + 8) /\ Mask,
		S9 is (1812433253 * xor(S8, S8 >> 30) + 9) /\ Mask,
		S10 is (1812433253 * xor(S9, S9 >> 30) + 10) /\ Mask,
		S11 is (1812433253 * xor(S10, S10 >> 30) + 11) /\ Mask,
		S12 is (1812433253 * xor(S11, S11 >> 30) + 12) /\ Mask,
		S13 is (1812433253 * xor(S12, S12 >> 30) + 13) /\ Mask,
		S14 is (1812433253 * xor(S13, S13 >> 30) + 14) /\ Mask,
		S15 is (1812433253 * xor(S14, S14 >> 30) + 15) /\ Mask,
		::assertz(seed_(well512a, s(0, s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15)))).

	reset_seed(as183) :-
		::retractall(seed_(as183, _)),
		::assertz(seed_(as183, s(3172, 9814, 20125))).
	reset_seed(xoshiro128pp) :-
		::retractall(seed_(xoshiro128pp, _)),
		% Default seed values (must be non-zero; [0x12345678, 0x9ABCDEF0, 0xFEDCBA98, 0x76543210])
		::assertz(seed_(xoshiro128pp, s(305419896, 2596069104, 4275878552, 1985229328))).
	reset_seed(xoshiro128ss) :-
		::retractall(seed_(xoshiro128ss, _)),
		% Default seed values (must be non-zero)
		::assertz(seed_(xoshiro128ss, s(305419896, 2596069104, 4275878552, 1985229328))).
	reset_seed(well512a) :-
		::retractall(seed_(well512a, _)),
		% Default seed (MT19937-style initialization from seed 5489)
		::assertz(seed_(well512a, s(0, s(5489, 1301868182, 2938499221, 2950281878, 1875628136, 751856242, 944701696, 2243192071, 694061057, 219885934, 2066767472, 3182869408, 485472502, 2336857883, 1071588843, 3418470598)))).

	% Mask for 32-bit integers (0xFFFFFFFF)
	mask32(4294967295).

	% Rotate left for 32-bit integers
	rotl32(X, K, Result) :-
		mask32(Mask),
		Left is (X << K) /\ Mask,
		Right is X >> (32 - K),
		Result is Left \/ Right.

	% WELL512a state access and update (one clause per index value, first-argument indexing)
	well512a_state(
		0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S0, S13, S9, S15, NewV1, NewV0,
		s(NewV1, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, NewV0),
		15
	).
	well512a_state(
		1,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S1, S14, S10, S0, NewV1, NewV0,
		s(NewV0, NewV1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		0
	).
	well512a_state(
		2,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S2, S15, S11, S1, NewV1, NewV0,
		s(S0, NewV0, NewV1, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		1
	).
	well512a_state(
		3,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S3, S0, S12, S2, NewV1, NewV0,
		s(S0, S1, NewV0, NewV1, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		2
	).
	well512a_state(
		4,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S4, S1, S13, S3, NewV1, NewV0,
		s(S0, S1, S2, NewV0, NewV1, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		3
	).
	well512a_state(
		5,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S5, S2, S14, S4, NewV1, NewV0,
		s(S0, S1, S2, S3, NewV0, NewV1, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		4
	).
	well512a_state(
		6,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S6, S3, S15, S5, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, NewV0, NewV1, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		5
	).
	well512a_state(
		7,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S7, S4, S0, S6, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, NewV0, NewV1, S8, S9, S10, S11, S12, S13, S14, S15),
		6
	).
	well512a_state(
		8,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S8, S5, S1, S7, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, NewV0, NewV1, S9, S10, S11, S12, S13, S14, S15),
		7
	).
	well512a_state(
		9,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S9, S6, S2, S8, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, NewV0, NewV1, S10, S11, S12, S13, S14, S15),
		8
	).
	well512a_state(
		10,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S10, S7, S3, S9, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, NewV0, NewV1, S11, S12, S13, S14, S15),
		9
	).
	well512a_state(
		11,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S11, S8, S4, S10, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, NewV0, NewV1, S12, S13, S14, S15),
		10
	).
	well512a_state(
		12,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S12, S9, S5, S11, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, NewV0, NewV1, S13, S14, S15),
		11
	).
	well512a_state(
		13,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S13, S10, S6, S12, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, NewV0, NewV1, S14, S15),
		12
	).
	well512a_state(
		14,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S14, S11, S7, S13, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, NewV0, NewV1, S15),
		13
	).
	well512a_state(
		15,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, S14, S15),
		S15, S12, S8, S14, NewV1, NewV0,
		s(S0, S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11, S12, S13, NewV0, NewV1),
		14
	).
