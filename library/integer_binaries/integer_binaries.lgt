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

logtalk_load(list).

:- object(integer_binaries).

	:- info([
		version is 0:1:0,
		author is 'Undecided',
		date is 2021-02-23,
		comment is 'Integer Binary Library'
	]).

	% TODO: methods to detect number formats
	:- public([encode/3]).
	:- private([encode_/4]).

	% Encode an Integer, given a maximum possible output byte size.
	% Output the resulting Octects.
	encode(Integer, Maximum_Output_Size, Output_Bytes) :-
		% when Max =:= infinity; is_integer(Max), Max > 0 ->
		(
			Maximum_Output_Size == infinity; 
			(integer(Maximum_Output_Size), Maximum_Output_Size > 0)
		),
		integer(Integer),
		Integer >= 0,
		encode_(Integer, Maximum_Output_Size, Output_Bytes).

	% This should be an accumulator method using a Head / Tail list.
	% However, I followed the erlang code verbatim, before I realized
	% Prolog would have a much more elegant way to make this happen.
	encode_(Integer, Maximum_Output_Size, Output_Bytes) :-
		(
			Maximum_Output_Size == infinity; 
			(integer(Maximum_Output_Size), Maximum_Output_Size > 0)
		),
		% Octet = Value band 16#7f,
		Octet = Output_Octets /\ 0x7f,

		% This
		% <<_:1, Val:7>> = <<Octet:8>> where Value:Size/TypeSpecifierList
		list::length(Supplement, 1),
		list::length(Value, 7),
		[Supplement, Value] = Octet,
		list::length(Octet, 8),
		% Am I not doing this correctly?
		% Acc1 = <<Bit:1, Val:7, Acc/bytes>>,
		% Acc1= [Bit, VAL, ],
		list::length(Supplement, 1),
		list::length(Value, 7),
		% Not right at all.
		Accumulator_Output = [Integer, Value, Accumulator],
		% TODO
		% 	Max1 = maybe_decrease(Max),
		%	Value1 = Value bsr 7,
		% Recurse... but this is not right.
		encode(Accumulator_Output, Maximum, Value_Output).

		/*
		<<_:1, Val:7>> = <<Octet:8>>,
		Acc1 = <<Bit:1, Val:7, Acc/bytes>>,
		Max1 = maybe_decrease(Max),
		Value1 = Value bsr 7,
		encode(Acc1, Max1, Value1);
		encode_helper(_, _, _, _) ->
		{error, badarg}.*/

	% In retrospect, I should have reverse-engineered this
	% ... I'm fairly awful with using bytes, here.
	% From <https://stackoverflow.com/questions/42862178/converting-a-list-of-bytes-to-integer-in-prolog>

	decode(Bytes, Output_Integer) :-
		decode(Bytes, 0, 0, R).
	
	decode([], _, R, R).
	
	decode([H|T], S0, R0, R) :-
		R1 is R0 + H << S0,
		S1 is S0 + 8,
		decode(T, S1, R1, R).

:- end_object.

%	define(MAX_8BYTE, 0xffffffffffffffff).
%	define(MAX_4BYTE, 0xffffffff).
%	define(MAX_2BYTE, 0xffff).
%	define(MAX_1BYTE, 0xff).

% positive integers
/*
	encode_integer(N) --> {N > 0xffffffff}, !, [0x1b, N].
	encode_integer(N) --> {N > 0xffff}, !, [0x1a, N].
	encode_integer(N) --> {N > 0xff}, !, [0x19, N].
	encode_integer(N) --> {N > 23}, !, [0x18, N].
	encode_integer(N) --> {N > 23}, !, [0x18, N].
	encode_number(N) --> {N >= 0}, !, [N].
*/
% negative integers
/*
	encode_num(N) --> {N > -25}, !, [0x1f-N].
	encode_num(N) --> {N >= -0xff}, !, [0x38, (-1-N)].
	encode_num(N) --> {N >= -0xffff}, !, [0x39, (-1-N)].
	encode_num(N) --> {N >= -0xffffffff}, !, [0x3a, (-1-N)].
	encode_num(N) --> {N >= -0xffffffffffffffff}, !, [0x3b, (-1-N)].
*/
