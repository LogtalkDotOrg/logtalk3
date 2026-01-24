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


	% Core Xoshiro256++ algorithm
	% Returns a random 64-bit integer and updates state
	random_seeds(xoshiro256pp, [S0, S1, S2, S3], [NewS0, NewS1, NewS2, NewS3], Random) :-
		mask64(Mask),
		% IntRandom = rotl(s0 + s3, 23) + s0
		Sum1 is (S0 + S3) /\ Mask,
		rotl64(Sum1, 23, Rotated),
		IntRandom is (Rotated + S0) /\ Mask,
		% t = s1 << 17
		T is (S1 << 17) /\ Mask,
		% Update state according to reference implementation:
		% s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
		% s[2] ^= t; s[3] = rotl(s[3], 45);
		TmpS2 is xor(S2, S0),
		TmpS3 is xor(S3, S1),
		NewS1 is xor(S1, TmpS2),
		NewS0 is xor(S0, TmpS3),
		NewS2 is xor(TmpS2, T),
		rotl64(TmpS3, 45, NewS3),
		% Convert to float in [0.0, 1.0)
		Random is IntRandom / 18446744073709551616.0.
	% Xoshiro256** algorithm
	% Returns a random 64-bit integer and updates state
	random_seeds(xoshiro256ss, [S0, S1, S2, S3], [NewS0, NewS1, NewS2, NewS3], Random) :-
		mask64(Mask),
		% IntRandom = rotl(s1 * 5, 7) * 9
		Mul1 is (S1 * 5) /\ Mask,
		rotl64(Mul1, 7, Rotated),
		IntRandom is (Rotated * 9) /\ Mask,
		% t = s1 << 17
		T is (S1 << 17) /\ Mask,
		% Update state according to reference implementation:
		% s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
		% s[2] ^= t; s[3] = rotl(s[3], 45);
		TmpS2 is xor(S2, S0),
		TmpS3 is xor(S3, S1),
		NewS1 is xor(S1, TmpS2),
		NewS0 is xor(S0, TmpS3),
		NewS2 is xor(TmpS2, T),
		rotl64(TmpS3, 45, NewS3),
		% Convert to float in [0.0, 1.0)
		Random is IntRandom / 18446744073709551616.0.
	% SplitMix64 algorithm
	% A simple generator used primarily for seeding other generators
	random_seeds(splitmix64, [State0], [State], Random) :-
		mask64(Mask),
		State1 is State0 + 0x9e3779b97f4a7c15,
		State is State1 /\ Mask,
		Z is (xor(State1, (State1 >> 30)) * 0xbf58476d1ce4e5b9) /\ Mask,
		IntRandom is xor(Z, (Z >> 27)) /\ Mask,
		% Convert to float in [0.0, 1.0)
		Random is IntRandom / 18446744073709551616.0.

	randomize(xoshiro256pp, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(xoshiro256pp, _)),
		mask64(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 16) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 32) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 48) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			::asserta(seed_(xoshiro256pp, [1, 1, 1, 1]))
		;	::asserta(seed_(xoshiro256pp, [S0, S1, S2, S3]))
		).
	randomize(xoshiro256ss, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(xoshiro256ss, _)),
		mask64(Mask),
		S0 is Seed /\ Mask,
		S1 is ((Seed >> 16) \/ (Seed * 7)) /\ Mask,
		S2 is ((Seed >> 32) \/ (Seed * 13)) /\ Mask,
		S3 is ((Seed >> 48) \/ (Seed * 17)) /\ Mask,
		% Ensure non-zero state
		(	S0 =:= 0, S1 =:= 0, S2 =:= 0, S3 =:= 0 ->
			::asserta(seed_(xoshiro256ss, [1, 1, 1, 1]))
		;	::asserta(seed_(xoshiro256ss, [S0, S1, S2, S3]))
		).
	randomize(splitmix64, Seed) :-
		integer(Seed),
		Seed > 0,
		::retractall(seed_(splitmix64, _)),
		mask64(Mask),
		S is Seed /\ Mask,
		::asserta(seed_(splitmix64, [S])).

	reset_seed(xoshiro256pp) :-
		::retractall(seed_(xoshiro256pp, _)),
		% Default seed values (must be non-zero)
		::asserta(seed_(xoshiro256pp, [0x123456789ABCDEF0, 0x0FEDCBA987654321, 0x13579BDF2468ACE0, 0x2468ACE013579BDF])).
	reset_seed(xoshiro256ss) :-
		::retractall(seed_(xoshiro256ss, _)),
		% Default seed values (must be non-zero)
		::asserta(seed_(xoshiro256ss, [0x123456789ABCDEF0, 0x0FEDCBA987654321, 0x13579BDF2468ACE0, 0x2468ACE013579BDF])).
	reset_seed(splitmix64) :-
		::retractall(seed_(splitmix64, _)),
		% Default seed value
		::asserta(seed_(splitmix64, [0x0DFC83DF70B8AB7E])).

	% Mask for 64-bit integers
	mask64(0xFFFFFFFFFFFFFFFF).

	% Rotate left for 64-bit integers
	rotl64(X, K, Result) :-
		mask64(Mask),
		Left is (X << K) /\ Mask,
		Right is X >> (64 - K),
		Result is Left \/ Right.
