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


:- object(haunted_wasteland).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2023-12-10,
		comment is 'Advent of Code 2023 Day 8: Haunted Wasteland.'
	]).

	:- public([
		steps_1/2,
		steps_2/2
	]).

	:- uses(avltree, [new/1, insert/4, lookup/3]).
	:- uses(list, [member/2]).
	:- uses(numberlist, [least_common_multiple/2]).
	:- uses(reader, [line_to_codes/2]).

	% Part 1

	steps_1(File, Steps) :-
		collect_data(File, Instructions, Nodes),
		% use an accumulator pair to compute the number of steps
		steps_1('AAA', Instructions, Nodes, 0, Steps).

	steps_1('ZZZ', _, _, Steps, Steps) :-
		% final label found
		!.
	steps_1(Label, [Instruction| Instructions], Nodes, Steps0, Steps) :-
		% otherwise continue to the next label
		lookup_next_label(Instruction, Label, Nodes, NextLabel),
		Steps1 is Steps0 + 1,
		steps_1(NextLabel, Instructions, Nodes, Steps1, Steps).

	% Part 2

	steps_2(File, Steps) :-
		collect_data(File, Instructions, Nodes),
		% find all labels that end with 'A'
		findall(
			Label,
			(	lookup(Label, _, Nodes),
				sub_atom(Label, _, 1, 0, 'A')
			),
			Labels
		),
		% find the number of steps for each label
		findall(
			LabelSteps,
			(	member(Label, Labels),
				steps_2(Label, Instructions, Nodes, 0, LabelSteps)
			),
			LabelsSteps
		),
		% as the solution path for each label loops, with the lengths
		% of the paths being different, compute the least common multiple
		% of all lengths to find the length of the path when the paths
		% for all labels will reach a final label at the same time
		least_common_multiple(LabelsSteps, Steps).

	steps_2(Label, _, _, Steps, Steps) :-
		sub_atom(Label, _, 1, 0, 'Z'),
		% final label found
		!.
	steps_2(Label, [Instruction| Instructions], Nodes, Steps0, Steps) :-
		% otherwise continue to the next label
		lookup_next_label(Instruction, Label, Nodes, NextLabel),
		Steps1 is Steps0 + 1,
		steps_2(NextLabel, Instructions, Nodes, Steps1, Steps).

	% auxiliary predicates and non-terminals

	collect_data(File, Instructions, Nodes) :-
		open(File, read, Stream),
		% input data is line-based
		line_to_codes(Stream, InstructionsLine),
		phrase(instructions(Instructions, Tail), InstructionsLine),
		% use a cyclic list to repeat the instructions
		Tail = Instructions,
		% skip separator line
		line_to_codes(Stream, _SeparatorLine),
		% create a dictionary to hold the nodes
		new(Nodes0),
		line_to_codes(Stream, NodeLine),
		collect_nodes(NodeLine, Stream, Nodes0, Nodes),
		close(Stream).

	% use an explicit list in the second clause to benefit from first-argument indexing
	collect_nodes(end_of_file, _, Nodes, Nodes).
	collect_nodes([Code| Codes], Stream, Nodes0, Nodes) :-
		phrase(node(Label, Left, Right), [Code| Codes]),
		insert(Nodes0, Label, Left-Right, Nodes1),
		% next node (if any)
		line_to_codes(Stream, NodeLine),
		collect_nodes(NodeLine, Stream, Nodes1, Nodes).

	instructions([Instruction| Instructions], Tail) -->
		instruction(Instruction),
		!,
		instructions(Instructions, Tail).
	instructions(Tail, Tail) -->
		[].

	instruction(left) -->
		"L".
	instruction(right) -->
		"R".

	node(Label, Left, Right) -->
		label(Label), " = (", label(Left), ", ", label(Right), ")".

	label(Label) -->
		[A,B,C], {atom_codes(Label, [A,B,C])}.

	lookup_next_label(left, Label, Nodes, NextLabel) :-
		lookup(Label, NextLabel-_, Nodes).
	lookup_next_label(right, Label, Nodes, NextLabel) :-
		lookup(Label, _-NextLabel, Nodes).

:- end_object.
