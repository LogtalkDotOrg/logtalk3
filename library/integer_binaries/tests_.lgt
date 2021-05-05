%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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

:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 0:1:0,
		author is 'Anonymous',
		date is 2021-02-23,
		comment is 'Integer Binary Formatting'
	]).

	:- uses(integer_binaries, [
		encode/2, decode/2
	]).

	cover(integer_binaries).

	% Encoding
	
	test(test_01, true(Encoding == [0x00])) :-
		encode([0x00], Encoding).

	test(test_02, true(Encoding == [0x7f])) :-
		encode([0x7f], Encoding).

	test(test_03, true(Encoding == [0x80])) :-
		encode([0x81, 0x00], Encoding).

	test(test_04, true(Encoding == [0x2000])) :-
		encode([0xc0,0x00], Encoding).

	test(test_05, true(Encoding == [0x3fff])) :-
		encode([0xff,0x7f], Encoding).

	test(test_06, true(Encoding == [0x4000])) :-
		encode([0x81,0x80,0x00], Encoding).

	test(test_07, true(Encoding == [0x1fffff])) :-
		encode([0xff,0xff,0x7f], Encoding).

	test(test_08, true(Encoding == [0x200000])) :-
		encode([0x81,0x80,0x80,0x00], Encoding).

	test(test_09, true(Encoding == [0x8000000])) :-
		encode([0xc0,0x80,0x80,0x00], Encoding).

	test(test_10, true(Encoding == [0x0fffffff])) :-
		encode([0xff,0xff,0xff,0x7f], Encoding).

	test(test_11, true(Encoding == [0xffffffff])) :-
		encode([0x8f, 0xff, 0xff, 0xff, 0x7f], Encoding).

	% Decoding

	test(test_12, true(Encoding == [0x00])) :-
		decode([0x00], Encoding).

	test(test_13, true(Encoding == [0x7f])) :-
		decode([0x7f], Encoding).

	test(test_14, true(Encoding == [0x81, 0x00])) :-
		decode([0x80], Encoding).

	test(test_15, true(Encoding == [0xc0,0x00])) :-
		decode([0x2000], Encoding).

	test(test_16, true(Encoding == [0xff,0x7f])) :-
		decode([0x3fff], Encoding).

	test(test_17, true(Encoding == [0x81,0x80,0x00])) :-
		decode([0x4000], Encoding).

	test(test_18, true(Encoding == [0xff,0xff,0x7f])) :-
		decode([0x1fffff], Encoding).

	test(test_19, true(Encoding == [0x81,0x80,0x80,0x00])) :-
		decode([0x200000], Encoding).

	test(test_20, true(Encoding == [0xc0,0x80,0x80,0x00])) :-
		decode([0x8000000], Encoding).

	test(test_21, true(Encoding == [0xff,0xff,0xff,0x7f])) :-
		decode([0x0fffffff], Encoding).

	test(test_22, true(Encoding == [0x8f, 0xff, 0xff, 0xff, 0x7f])) :-
		decode([0xffffffff], Encoding).

:- end_object.
