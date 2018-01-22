%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <https://logtalk.org/>  
%  Copyright 1998-2018 Paulo Moura <pmoura@logtalk.org>
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


% this example illustrates (using laptops and its basic components) object-
% based composition where an object, representing some kind of assembly, is
% composed by other objects, its parts
%
% the main difference between object-based composition and category-based
% composition is that, with categories, their protocols add to the protocol
% of the object importing them while with objects we have a level of
% indirection when accessing the parts protocols


% first, we define a protocol for describing laptops and laptop components

:- protocol(describe).

	:- public(describe/0).

:- end_protocol.


% second, we define a set of categories for the laptop mainboard, display,
% and keyboard; we use categories instead of protocols in order to define
% the describe/0 predicate

:- category(mainboard,
	implements(describe)).

	:- public([
		cpu/1, memory/1
	]).

	describe :-
		::cpu(CPU),
		write('CPU: '), write(CPU), nl,
		::memory(Memory),
		write('Memory: '), write(Memory), write(' GB'), nl.

:- end_category.


:- category(display,
	implements(describe)).

	:- public(resolution/2).

	describe :-
		::resolution(Width, Height),
		write('Display: '),
		write(Width), write(' x '), write(Height), write(' pixels'), nl.

:- end_category.


:- category(keyboard,
	implements(describe)).

	:- public(layout/1).

	describe :-
		::layout(Layout),
		write('Keyboard: '), write(Layout), nl.

:- end_category.


% third, we define some objects importing the above categories so that
% we have some laptop components to play with

:- object(fast,
	imports(mainboard)).

	cpu(i5).
	memory(8).

:- end_object.


:- object(faster,
	imports(mainboard)).

	cpu(i7).
	memory(16).

:- end_object.


:- object(thirteen,
	imports(display)).

	resolution(1440, 900).

:- end_object.


:- object(fifteen,
	imports(display)).

	resolution(2560, 1600).

:- end_object.


:- object(qwerty,
	imports(keyboard)).

	layout(qwerty).

:- end_object.


:- object(dvorak,
	imports(keyboard)).

	layout(dvorak).

:- end_object.


% now we can assemble some laptops; again we start with a category in
% order to provide an implementation for the describe/0 predicate and
% declarations for the public predicates

:- category(laptop,
	implements(describe)).

	:- public([
		mainboard/1, display/1, keyboard/1
	]).

	% as we are using objects for composition, thus we have a
	% level of indirection when going from assembly to parts
	describe :-
		::mainboard(Mainboard), Mainboard::describe,
		::display(Display), Display::describe,
		::keyboard(Keyboard), Keyboard::describe.		

:- end_category.


:- object(basic,
	imports(laptop)).

	mainboard(fast).
	display(thirteen).
	keyboard(qwerty).

:- end_object.


:- object(business,
	imports(laptop)).

	mainboard(faster).
	display(fifteen).
	keyboard(qwerty).

:- end_object.


:- object(custom,
	imports(laptop)).

	:- public(new/4).

	% assume that the components are final and thus cannot be swapped
	new(Mainboard, Display, Keyboard, Laptop) :-
		create_object(
			Laptop,
			% same protocol as the predefined laptops
			[imports(laptop)],
			[],
			% specify the components for the new laptop
			[mainboard(Mainboard), display(Display), keyboard(Keyboard)]
		).

:- end_object.
