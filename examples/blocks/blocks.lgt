%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
%  Copyright 1998-2016 Paulo Moura <pmoura@logtalk.org>
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


:- object(block,
	instantiates(class),
	specializes(object)).

	:- info([
		version is 1.0,
		date is 2016/05/25,
		author is 'Paulo Moura',
		comment is 'Two-dimensional block (or should I say square?) class.'
	]).

	:- public(move/2).
	:- mode(move(+integer, +integer), one).
	:- info(move/2, [
		comment is 'Moves a brick to a new position.',
		argnames is ['X', 'Y']
	]).

	:- public(position/2).
	:- mode(position(?integer, ?integer), zero_or_one).
	:- info(position/2, [
		comment is 'Brick current position.',
		argnames is ['X', 'Y']
	]).

	:- private(position_/2).
	:- dynamic(position_/2).
	:- mode(position_(?integer, ?integer), zero_or_one).
	:- info(position_/2, [
		comment is 'Stores brick current position.',
		argnames is ['X', 'Y']
	]).

	position(X, Y) :-
		::position_(X, Y).

	move(X, Y) :-
		::retractall(position_(_, _)),
		::assertz(position_(X, Y)).

	default_init_option(position-(0, 0)).
	default_init_option(Default) :-
		^^default_init_option(Default).

	process_init_option(position-(X, Y)) :-
		::assertz(position_(X, Y)).
	process_init_option(Option) :-
		^^process_init_option(Option).

	valid_init_option(position-(X, Y)) :-
		!,
		integer(X),
		integer(Y).
	valid_init_option(Option) :-
		^^valid_init_option(Option).

	instance_base_name(b).

:- end_object.


:- object(block_stack,
	implements(monitoring)).

	:- info([
		version is 1.0,
		date is 2016/05/25,
		author is 'Paulo Moura',
		comment is 'Block stacks. A stack is represented by top-bottom tuples.'
	]).

	:- public(tuple/1).
	:- mode(tuple(?pair(object_identifier)), zero_or_more).
	:- info(tuple/1, [
		comment is 'Returns a relation tuple.',
		argnames is ['Tuple']
	]).

	:- public(tuples/1).
	:- mode(tuples(-list(pair(object_identifier))), one).
	:- info(tuples/1, [
		comment is 'Returns a list of all relation tuples.',
		argnames is ['Tuples']
	]).

	:- public(add_tuple/1).
	:- mode(add_tuple(+pair(object_identifier)), zero_or_one).
	:- info(add_tuple/1, [
		comment is 'Adds a new relation tuple.',
		argnames is ['Tuple']
	]).

	:- public(remove_tuple/1).
	:- mode(remove_tuple(?pair(object_identifier)), zero_or_more).
	:- info(remove_tuple/1, [
		comment is 'Removes a matching relation tuple.',
		argnames is ['Tuple']
	]).

	:- public(remove_all_tuples/0).
	:- mode(remove_all_tuples, one).
	:- info(remove_all_tuples/0, [
		comment is 'Removes all relation tuples.'
	]).

	:- private(tuple_/2).
	:- dynamic(tuple_/2).
	:- mode(tuple_(?tuple), zero_or_more).
	:- info(tuple_/2, [
		comment is 'Stores the relation tuples.',
		argnames is ['Top', 'Bottom']
	]).

	add_tuple(Top-Bottom) :-
		% find the bottom block position
		Bottom::position(Xb, Yb),
		% ensure the top block is on top of the bottom block
		Yt is Yb + 1,
		Top::move(Xb, Yt),
		% store the tuple
		assertz(tuple_(Top, Bottom)),
		% start monitoring both blocks movements
		define_events(after, Top, move(_,_), _Sender, block_stack),
		define_events(after, Bottom, move(_,_), _Sender, block_stack).

	remove_tuple(Top-Bottom) :-
		% remove the tuple from the database
		retract(tuple_(Top, Bottom)),
		% check if we must continue to monitor the block movements
		(	\+ tuple_(Top, _), \+ tuple_(_, Top) ->
			% top block no longer part on any tuple
			abolish_events(after, Top, move(_,_), _Sender, block_stack)
		;	% continue to monitor top block movements
			true
		),
		(	\+ tuple_(Bottom, _), \+ tuple_(_, Bottom) ->
			% bottom block no longer part of any tuple
			abolish_events(after, Bottom, move(_,_), _Sender, block_stack)
		;	% continue to monitor bottom block movements
			true
		).

	remove_all_tuples :-
		retractall(tuple_(_, _)),
		abolish_events(_, _, _, _, block_stack).
	
	tuple(Top-Bottom) :-
		tuple_(Top, Bottom).

	tuples(Tuples) :-
		findall(Top-Bottom, tuple_(Top, Bottom), Tuples).

	after(Block, move(X, Y), _) :-
		(	tuple_(Block, Bottom) ->
			% moved block plays the role of top block in a tuple
			(	Yb is Y - 1,
				Bottom::position(X, Yb) ->
				% bottom block is already below the new position of the moved block
				true
			;	% botom block didn't move along 
				remove_tuple(Block-Bottom)
			)
		;	true
		),
		(	tuple_(Top, Block) ->
			% moved block plays the role of bottom block; move the block on top of it
			Yt is Y + 1,
			Top::move(X, Yt)
		;	% moved block is at the top of its stack
			true
		).

:- end_object.


:- object(stack_monitor,
	implements(monitoring)).

	:- info([
		version is 1.0,
		date is 2016/05/25,
		author is 'Paulo Moura',
		comment is 'Block stack monitor. Prints an ASCII representation of all block stacks everytime a block is moved.'
	]).

	:- uses(loop, [forto/4, fordownto/4]).
	:- uses(list, [member/2, last/2]).

	after(_, move(_, _), _) :-
		% find the position of all blocks
		findall(
			(Block, X, Y),
			(instantiates_class(Block, block), Block::position(X, Y)),
			Blocks
		),
		% find the larger X coordinate
		setof(X, Block^Y^ member((Block,X,Y), Blocks), Xs),
		last(Xs, Xmax),
		% find the larger Y coordinate
		setof(Y, Block^X^ member((Block,X,Y), Blocks), Ys),
		last(Ys, Ymax),
		% draw a representation of the position of all the blocks
		fordownto(Y, Ymax, 1,
			(write('|'),
			 forto(X, 1, Xmax,
				(member((Block, X, Y), Blocks) -> write(Block); write('.'))
			 ),
			 nl)
		),
		write('-'),
		forto(X, 1, Xmax, write('-')), nl.

:- end_object.
