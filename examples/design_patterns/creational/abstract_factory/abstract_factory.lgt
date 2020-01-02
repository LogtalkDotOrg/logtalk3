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


% this example is an adaptation of the sample code found on the Wikipedia
% page on the Builder design pattern:
%
% https://en.wikipedia.org/wiki/Abstract_factory_pattern


% this example uses GUI buttons whose appearance depends on the
% operating-system; the client uses the abstract factory protocol
% to create new buttons thus making the client independent on how
% those buttons are created; the client would also use the button
% protocol independently of the concrete type of the button


% use a category to define the button protocol for clients, illustrated
% by the paint/0 predicate, and a button create predicate, new/1, for
% use by the button factories

:- category(button).

	:- public(new/1).
	new(Button) :-
		self(Self),
		create_object(Button, [extends(Self)], [], []).

	:- public(paint/0).

:- end_category.


% define three types of buttons for Linux, Windows, and macOS
% operating-systems

:- object(linux_button,
	imports(button)).

	paint :-
		write('Render a button in a Linux style'), nl.

:- end_object.


:- object(windows_button,
	imports(button)).

	paint :-
		write('Render a button in a Windows style'), nl.

:- end_object.


:- object(macos_button,
	imports(button)).

	paint :-
		write('Render a button in a macOS style'), nl.

:- end_object.


% define the abstract factory protocol that will be used by
% the clients, illustrated by the create_button/1 predicate

:- protocol(gui_factory).

	:- public(create_button/1).

:- end_protocol.


% define three concrete button factories for Linux, Windows,
% and macOS operating-systems

:- object(linux_factory,
	implements(gui_factory)).

	create_button(Button) :-
		linux_button::new(Button).

:- end_object.


:- object(windows_factory,
	implements(gui_factory)).

	create_button(Button) :-
		windows_button::new(Button).

:- end_object.


:- object(macos_factory,
	implements(gui_factory)).

	create_button(Button) :-
		macos_button::new(Button).

:- end_object.


% to simplify requests from clients, we define a parametric
% factory object where the parameter is used to specify the
% operating-system

:- object(factory(_Appearance_),
	implements(gui_factory)).

	create_button(Button) :-
		appearance_factory(_Appearance_, Factory),
		[Factory::create_button(Button)].

	appearance_factory(linux,   linux_factory).
	appearance_factory(macos,   macos_factory).
	appearance_factory(windows, windows_factory).

:- end_object.
