%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 1998-2021 Paulo Moura <pmoura@logtalk.org>
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
% page on the Proxy design pattern:
%
% https://en.wikipedia.org/wiki/Proxy_pattern


% first we define a protocol that will be implemented by both the real
% object and by its proxy

:- protocol(carp).

	:- public(drive/0).

:- end_protocol.


% the real object represents cars that can be driven

:- object(car,
	implements(carp)).

	drive :-
		write('Car has been driven!'), nl.

:- end_object.


% the proxy object checks that the driver is old enough to actually
% drive a car

:- object(car_proxy(_Car_, _Driver_),
	implements(carp)).

	drive :-
		(	_Driver_::age(Age), Age < 16 ->
			_Driver_::name(Name),
			write('Sorry, '), write(Name), write(' is too young to drive!'), nl
		;	_Car_::drive
		).

:- end_object.


% we now need to define drivers; to show how we can use prototypes and
% classes together and freely mix static and dynamic objects, we define
% "driver" as a class using a meta-class to hold a convenient predicate
% for creating new dynamic "driver" instances

:- object(meta_driver,
	instantiates(meta_driver)).

	:- public(new/3).
	
	new(Driver, Name, Age) :-
		self(Self),
		create_object(Driver, [instantiates(Self)], [], [name(Name), age(Age)]).

:- end_object.


:- object(driver,
	instantiates(meta_driver)).

	:- public(name/1).
	% default definition
	name('Jane Doe').

	:- public(age/1).
	% default definition
	age(21).

:- end_object.
