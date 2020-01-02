%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2020 Paulo Moura <pmoura@logtalk.org>
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


% a slide/2 table defining slide order and contents

slide(1, 'First slide').
slide(2, 'Second slide').
slide(3, 'Third slide').
slide(4, 'Fourth slide').
slide(5, 'Fifth slide').
slide(6, 'Last slide').


% a slides object supporting displaying and navigating slides

:- object(slides).

	:- public(show/2).
	:- meta_predicate(show(*, 2)).

	:- uses(zlist, [
		zip/3, next/3, previous/3, rewind/3, forward/3
	]).

	:- uses(logtalk, [
		ask_question/5, print_message/3
	]).

	show(Slides, Closure) :-
		% create a zipper from a list of slide indexes
		zip(Slides, Zipper, First),
		% display the first slide by calling the user-provided
		% closure with the slide index as argument
		display(First, Closure),
		% do an interactive slideshow by reading
		% and acting on the remote commands
		remote(Command),
		interact(Command, Zipper, Closure).

	% next slide
	interact(n, Zipper, Closure) :-
		next(Zipper, UpdatedZipper, Next),
		display(Next, Closure),
		remote(Command),
		interact(Command, UpdatedZipper, Closure).
	% previous slide
	interact(p, Zipper, Closure) :-
		previous(Zipper, UpdatedZipper, Previous),
		display(Previous, Closure),
		remote(Command),
		interact(Command, UpdatedZipper, Closure).
	% first slide
	interact(f, Zipper, Closure) :-
		rewind(Zipper, UpdatedZipper, First),
		display(First, Closure),
		remote(Command),
		interact(Command, UpdatedZipper, Closure).
	% last slide
	interact(l, Zipper, Closure) :-
		forward(Zipper, UpdatedZipper, Last),
		display(Last, Closure),
		remote(Command),
		interact(Command, UpdatedZipper, Closure).
	% end slide show
	interact(e, _, _).

	:- meta_predicate(display(*, 2)).
	display(Index, Closure) :-
		call(Closure, Index, Contents),
		print_message(information, slides, contents(Contents)).

	remote(Command) :-
		ask_question(question, slides, remote, valid, Command).

	% table of valid remote commands
	valid(n).
	valid(p).
	valid(f).
	valid(l).
	valid(e).

	:- multifile(logtalk::message_prefix_stream/4).
	:- dynamic(logtalk::message_prefix_stream/4).
	% set the prefix to the empty atom so that the output only contains
	% the actual slide contents
	logtalk::message_prefix_stream(information, slides, '', user_output).

	:- multifile(logtalk::message_tokens//2).
	:- dynamic(logtalk::message_tokens//2).

	logtalk::message_tokens(contents(Contents), slides) -->
		['~w'-[Contents], nl].
	logtalk::message_tokens(remote, slides) -->
		['remote '-[]].

:- end_object.
