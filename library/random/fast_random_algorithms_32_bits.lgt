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

	randomize(as183, Seed) :-
		integer(Seed),
		Seed > 0,
		retractall(seed_(as183, _)),
		S0 is Seed mod 30269,
		S1 is Seed mod 30307,
		S2 is Seed mod 30323,
		assertz(seed_(as183, s(S0, S1, S2))).
	randomize(xoshiro128pp, Seed) :-
		integer(Seed),
		Seed > 0,
		retractall(seed_(xoshiro128pp, _)),
		mask32(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 32) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 64) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 96) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			assertz(seed_(xoshiro128pp, s(1, 1, 1, 1)))
		;	assertz(seed_(xoshiro128pp, s(S0, S1, S2, S3)))
		).
	randomize(xoshiro128ss, Seed) :-
		integer(Seed),
		Seed > 0,
		retractall(seed_(xoshiro128ss, _)),
		mask32(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 32) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 64) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 96) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			assertz(seed_(xoshiro128ss, s(1, 1, 1, 1)))
		;	assertz(seed_(xoshiro128ss, s(S0, S1, S2, S3)))
		).

	reset_seed(as183) :-
		retractall(seed_(as183, _)),
		assertz(seed_(as183, s(3172, 9814, 20125))).
	reset_seed(xoshiro128pp) :-
		retractall(seed_(xoshiro128pp, _)),
		% Default seed values (must be non-zero; [0x12345678, 0x9ABCDEF0, 0xFEDCBA98, 0x76543210])
		assertz(seed_(xoshiro128pp, s(305419896, 2596069104, 4275878552, 1985229328))).
	reset_seed(xoshiro128ss) :-
		retractall(seed_(xoshiro128ss, _)),
		% Default seed values (must be non-zero)
		assertz(seed_(xoshiro128ss, s(305419896, 2596069104, 4275878552, 1985229328))).

	% Mask for 32-bit integers (0xFFFFFFFF)
	mask32(4294967295).

	% Rotate left for 32-bit integers
	rotl32(X, K, Result) :-
		mask32(Mask),
		Left is (X << K) /\ Mask,
		Right is X >> (32 - K),
		Result is Left \/ Right.
