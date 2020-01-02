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


:- object(travel).

	:- public(journey/0).

	:- protected([
		book/0, pack/0, travel/0, unpack/0
	]).

	% journey algorithm defined as a sequence of steps
	journey :-
		::book,
		::pack,
		::travel,
		::unpack.

	% default definition for the pack step
	pack :-
		write('Put book, pajamas, and toothbrush in luggage'), nl.

	% default definition for the unpack step
	unpack :-
		write('Put book and pajamas in nightstand and toothbrush in bathroom'), nl.

	% the book/0 and travel/0 predicates don't have default definitions and
	% thus must be defined in the descendant objects; if no definitions are
	% found the ::book and ::travel will fail and thus the call to the 
	% journey/0 predicate will also fail; an alternative is to add default
	% definitions that throw an error if they are not overridden in the
	% descendant objects; for example:
	%
	% book :-
	%	context(Context),
	%	throw(error(descendant_responsibility, Context)).

:- end_object.


:- object(travel_by_car,
	extends(travel)).

	book :-
		write('Rent a car'), nl.

	travel :-
		write('Drive car to destination'), nl.

:- end_object.


:- object(travel_by_airplane,
	extends(travel)).

	book :-
		write('Make a flight reservation'), nl.

	% specialize the inherited book/0 predicate
	% definition with an additional step
	pack :-
		^^pack,
		write('Check luggage weight against airline policy'), nl.

	travel :-
		write('Go to the airport and board the airplane'), nl.

:- end_object.
