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


:- object(functions2d).

	:- info([
		version is 1.0,
		author is 'Paul Crocker',
		date is 2008/07/19,
		comment is 'Example functions of two variables'
	]).

	:- public(eval/4).
	:- mode(eval(+atom, +float,+float, -float), one).
	:- info(eval/4, [
		comment is 'Calculates the function value.',
		argnames is ['Function', 'X', 'Y', 'Fxy']
	]).

	:- public(integral/2).
	:- mode(integral(+atom, -float), one).
	:- info(integral/2, [
		comment is 'Function integral.',
		argnames is ['Function', 'Integral']
	]).

	integral(circle,   -21.33333).
	integral(poly6,      5.88698).
	integral(i14,      11.947778).
	integral(i15,         7.7359).
	integral(bailey1,   0.765196).
	integral(bailey2,    1.07664).
	integral(bailey3,    3.17344).
	integral(bailey4,    4.66927).
	integral(bailey5,    1.82396).

	% [-2,2-2,2]
	% Integral = 21.33333
	eval(circle, X, Y, Fxy) :-
		Fxy is X*X + Y*Y - 2.0*2.0.

	% [-2,2-2,2]
	% Integral =  5.88698
	eval(poly6, X, Y, Fxy) :-
		Fxy is X**6.0/200.0 - X*Y**5.0 - 0.1*X*X*Y*Y  + 0.5.

	% [-2,2-2,2]
	% Integral = 11.947778
	eval(i14, X, Y, Fxy) :-
		Fxy is  5.0 / (1.0 + 4.0*(X*X + Y*Y)).

	% [-2,2-2,2]
	% Integral = 7.7359
	eval(i15, X, Y, Fxy) :-
		F is sin(6.0*X*Y), Fxy is F*F.

	% [0,1,0,1]   
	% Integral is sqrt(2)/3 - log(2)/6 + log(2+sqrt(2))/3.
	% Integral = 0.765196.
	eval(bailey1, X, Y, Fxy) :-
		Fxy is sqrt(X*X + Y*Y).

	% [0,1,0,1] 
	% Integral is (-sqrt(2)/3.0 - log(sqrt(2)-1)/2 + log(sqrt(2)+1)/2+2/3 ).
	% Integral = 1.07664.
	eval(bailey2, X, Y, Fxy) :-
		Fxy is sqrt(1.0 + (X-Y) * (X-Y)).

	% [-1,1,-1,1]
	% Integral is (4*log(2+sqrt(3))-2*pi/3).
	% Integral = 3.17344.
	eval(bailey3, X, Y, Fxy) :-
		Fxy is 1.0 / sqrt(1.0 + (X*X) + (Y*Y)).

	% [0,pi,0,pi]
	% Integral is (4*pi*G-pi*pi*log(2))
	% G is Catalans Constant =  0.915965594177...  
	% Integral is 4*pi*0.915965594177-pi*pi*log(2).
	% Integral = 4.66927.
	eval(bailey4, X, Y, Fxy) :-
		Fxy is log(2.0 - cos(X) - cos(Y)).

	% [0,inf,0,inf] --> use a valor for infinite such as, inf=100, for example
	% A note of interest values much larger may give strange result due to floating point arithmetic 
	% The actual values which give strange values vary wrt to OS etc.
	% Integral is 1 + 3*log(3)/4.
	% Integral = 1.82396.
	eval(bailey5, X, Y, Fxy) :-
		Fxy is sqrt(X*X + Y*Y + Y*X) * exp(-X-Y).

:- end_object.
