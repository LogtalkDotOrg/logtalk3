%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2022 Paulo Moura <pmoura@logtalk.org>
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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Mediator design pattern:
%
% https://en.wikipedia.org/wiki/Mediator_pattern


% in this example, our mediator object coordinates three collaborating
% buttons; pressing of a button is communicated to the mediator, which
% then can set the status of all buttons and a text display
%
% for simplicity, we use a mediator prototype with static references to
% the colleagues and colleagues with a static reference to the mediator


:- object(mediator).

	% mediator must knows all colleagues
	% to coordinate their interaction

	:- public(book/0).
	book :-
		book_button::set_enabled_status(false),
		view_button::set_enabled_status(true),
		search_button::set_enabled_status(true),
		display::show('booking...').

	:- public(view/0).
	view :-
		book_button::set_enabled_status(true),
		view_button::set_enabled_status(false),
		search_button::set_enabled_status(true),
		display::show('viewing...').

	:- public(search/0).
	search :-
		book_button::set_enabled_status(true),
		view_button::set_enabled_status(true),
		search_button::set_enabled_status(false),
		display::show('searching...').

:- end_object.


% use a category for the common functionality of all buttons,
% which play the role of colleagues

:- category(button).

	:- public(click/0).

	:- public(set_enabled_status/1).
	set_enabled_status(true) :-
		self(Self),
		write(Self), write(' enabled'), nl.
	set_enabled_status(false) :-
		self(Self),
		write(Self), write(' disabled'), nl.

:- end_category.


:- object(book_button,
	imports(button)).

	click :-
		% colleague knows the mediator but not
		% the other colleagues
		mediator::book.

:- end_object.


:- object(view_button,
	imports(button)).

	click :-
		% colleague knows the mediator but not
		% the other colleagues
		mediator::view.

:- end_object.


:- object(search_button,
	imports(button)).

	click :-
		% colleague knows the mediator but not
		% the other colleagues
		mediator::search.

:- end_object.


% we also have a display object

:- object(display).

	:- public(show/1).
	show(Text) :-
		write(Text), nl.

:- end_object.
