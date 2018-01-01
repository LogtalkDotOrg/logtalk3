%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  
%  This file is part of Logtalk <http://logtalk.org/>  
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


:- initialization(
	catch((client_a_variant::double([1,2,3], Doubles), (write(Doubles), nl)), Error, (writeq(Error), nl))
).


:- object(library_a_variant).

	:- meta_predicate(map_(*, 2, *)).
	map_([], _, []).
	map_([X| Xs], Closure, [Y| Ys]) :-
		call(Closure, X, Y),
		map_(Xs, Closure, Ys).

	:- public(map/3).
	:- meta_predicate(map(*, 2, *)).
	map(In, Closure, Out) :-
		(	Closure = scale(_) ->
			% the second argument will trigger a runtime error
			% as scale/3 is not defined in "library_a_variant"
			map_(In, scale(3), Out)
		;	map_(In, Closure, Out)
		).

:- end_object.


:- object(client_a_variant).

	:- public(double/2).
	double(Ints, Doubles) :-
		library_a_variant::map(Ints, scale(2), Doubles).

	scale(Scale, X, Xscaled) :-
		Xscaled is X*Scale.

:- end_object.
