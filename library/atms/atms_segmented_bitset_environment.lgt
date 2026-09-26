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


:- object(atms_segmented_bitset_environment,
	implements(atms_environment_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-09-26,
		comment is 'ATMS environments represented as sparse ordered lists of nonempty 16-bit blocks.'
	]).

	empty([]).

	singleton(Node, [Block-Bit]) :-
		node_bit(Node, Block, Bit).

	union([], Environment, Environment) :-
		!.
	union(Environment, [], Environment) :-
		!.
	union([Block1-Bits1| Blocks1], [Block2-Bits2| Blocks2], Union) :-
		compare(Order, Block1, Block2),
		union(Order, Block1, Bits1, Blocks1, Block2, Bits2, Blocks2, Union).

	union(<, Block1, Bits1, Blocks1, Block2, Bits2, Blocks2, [Block1-Bits1| Union]) :-
		union(Blocks1, [Block2-Bits2| Blocks2], Union).
	union(=, Block, Bits1, Blocks1, _, Bits2, Blocks2, [Block-Bits| Union]) :-
		Bits is Bits1 \/ Bits2,
		union(Blocks1, Blocks2, Union).
	union(>, Block1, Bits1, Blocks1, Block2, Bits2, Blocks2, [Block2-Bits2| Union]) :-
		union([Block1-Bits1| Blocks1], Blocks2, Union).

	subset([], _).
	subset([Block1-Bits1| Blocks1], [Block2-Bits2| Blocks2]) :-
		compare(Order, Block1, Block2),
		subset(Order, Block1, Bits1, Blocks1, Bits2, Blocks2).

	subset(=, _, Bits1, Blocks1, Bits2, Blocks2) :-
		Bits1 /\ Bits2 =:= Bits1,
		subset(Blocks1, Blocks2).
	subset(>, Block1, Bits1, Blocks1, _, Blocks2) :-
		subset([Block1-Bits1| Blocks1], Blocks2).

	equal(Environment1, Environment2) :-
		Environment1 == Environment2.

	from_list(Nodes, Environment) :-
		sort(Nodes, Sorted),
		nodes_to_blocks(Sorted, Environment).

	nodes_to_blocks([], []).
	nodes_to_blocks([Node| Nodes], [Block-Bits| Blocks]) :-
		node_bit(Node, Block, Bit),
		complete_block(Nodes, Block, Bit, Remaining, Bits),
		nodes_to_blocks(Remaining, Blocks).

	complete_block([], _, Bits, [], Bits).
	complete_block([Node| Nodes], Block, Bits0, Remaining, Bits) :-
		node_bit(Node, NextBlock, Bit),
		(	NextBlock =:= Block ->
			Bits1 is Bits0 \/ Bit,
			complete_block(Nodes, Block, Bits1, Remaining, Bits)
		;	Remaining = [Node| Nodes],
			Bits = Bits0
		).

	to_list(Environment, Nodes) :-
		blocks_to_nodes(Environment, Nodes, []).

	blocks_to_nodes([], Nodes, Nodes).
	blocks_to_nodes([Block-Bits| Blocks], Nodes, Tail) :-
		Id is Block * 16,
		bits_to_nodes(Bits, Id, Nodes, Rest),
		blocks_to_nodes(Blocks, Rest, Tail).

	bits_to_nodes(0, _, Nodes, Nodes) :-
		!.
	bits_to_nodes(Bits, Id, Nodes, Tail) :-
		Bit is Bits mod 2,
		NextBits is Bits // 2,
		NextId is Id + 1,
		(	Bit =:= 1 ->
			Nodes = [node(Id)| Rest]
		;	Nodes = Rest
		),
		bits_to_nodes(NextBits, NextId, Rest, Tail).

	node_bit(node(Id), Block, Bit) :-
		integer(Id),
		Id >= 0,
		Block is Id // 16,
		Offset is Id mod 16,
		Bit is 1 << Offset.

:- end_object.
