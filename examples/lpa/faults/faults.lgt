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


:- object(fault,
	imports(proto_hierarchy)).

	:- info([
		author is 'Paulo Moura',
		version is 1.0,
		date is 2000/4/22,
		comment is 'Expert system for automobile fault diagnosis.',
		source is 'Example adapted from the LPA Prolog++ documentation.'
	]).

	:- public(findall/0).
	:- mode(findall, one).

	:- private(told_by_user_/2).
	:- dynamic(told_by_user_/2).
	:- mode(told_by_user_(?nonvar, ?nonvar), zero_or_more).

	:- public(find/1).
	:- mode(find(?nonvar), zero_or_more).

	:- private(exhibited/1).
	:- mode(exhibited(+nonvar), zero_or_one).

	:- public(contrary/2).
	:- mode(contrary(?nonvar, ?nonvar), zero_or_more).

	:- public(fault/2).
	:- mode(fault(?nonvar, ?nonvar), zero_or_more).

	:- public(effect/2).
	:- mode(effect(?nonvar, ?nonvar), zero_or_more).

	:- public(symptom/2).
	:- mode(symptom(?nonvar, ?nonvar), zero_or_more).

	findall :-
		retractall(told_by_user_(_, _)),
		write('Please answer all questions with yes or no.'), nl, nl,
		forall(
			(::descendant(Where), Where::find(Description)),
			(nl, write('Location      : '), write(Where), nl,
			 write('Possible Fault: '), write(Description), nl)),
		nl, write('No (more) explanations found.').

	find(Description) :-
		::fault(Fault, Description),
		forall(::effect(Fault, Symptom), exhibited(Symptom)).

	exhibited(Symptom) :-
		told_by_user_(Symptom, Reply) ->
			Reply = yes
			;
			::symptom(Symptom, Description),
			write(Description), write('? '),
			read(Reply),
			asserta(told_by_user_(Symptom, Reply)),
			Reply = yes,
			forall(
				(::contrary(Symptom, Contrary); ::contrary(Contrary, Symptom)),
				asserta(told_by_user_(Contrary, no))).

:- end_object.



/* electrical sub-system:

	electrical
		lights
		starting
			sparking
				distributor
				plugs
			starter_motor

*/


:- object(electrical,
	extends(fault)).

:- end_object.


:- object(lights,
	extends(electrical)).

:- end_object.


:- object(starting,
	extends(electrical)).

:- end_object.


:- object(sparking,
	extends(starting)).

:- end_object.


:- object(distributor,
	extends(sparking)).

	fault(f1001, 'Condensation in the distributor cap').
	fault(f1002, 'Faulty distributor arm').
	fault(f1003, 'Worn distributor brushes').

	symptom(s1001, 'The starter turns but the engine doesnt fire').
	symptom(s1002, 'The engine has difficulty starting').
	symptom(s1003, 'The engine cuts out shortly after starting').
	symptom(s1004, 'The engine cuts out at speed').

	effect(f1001, s1001).
	effect(f1002, s1001).
	effect(f1002, s1004).
	effect(f1003, s1002).
	effect(f1003, s1003).

	contrary(s1002, s1001).
	contrary(s1003, s1001).

:- end_object.


:- object(plugs,
	extends(sparking)).

:- end_object.


:- object(starter_motor,
	extends(starting)).

:- end_object.



/* mechanical sub-system:

	mechanical
		engine
			cylinders
*/


:- object(mechanical,
	extends(fault)).

:- end_object.


:- object(engine,
	extends(mechanical)).

:- end_object.


:- object(cylinders,
	extends(engine)).

:- end_object.



/* fuel_system sub-system:

	fuel_system
		...
*/


:- object(fuel_system,
	extends(fault)).

:- end_object.
