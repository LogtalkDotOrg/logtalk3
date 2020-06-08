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


:- object(figures_split(
	% vertices
	_A_, _B_, _C_, _D_, _E_, _F_, _G_,
	% edge slopes
	_S1_, _S2_, _S3_, _S4_,
	% edge lengths
	_L1_, _L2_, _L3_, _L4_,
	% perpendicularity
	_P_
)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2020-06-08,
		comment is 'Extended example of network modeling for recognizing polyhedra represented as graphs.',
		source is '"A framework for network modeling in Prolog", Zdravko I. Markov, IJCAI, 1989.'
	]).

	:- uses(coroutining, [dif/1]).

	:- public(edge/4).
	edge(_A_, _B_, _S1_, _L1_).
	edge(_B_, _C_, _S2_, _L1_).
	edge(_C_, _D_, _S1_, _L1_).
	edge(_D_, _A_, _S2_, _L1_).
	edge(_B_, _E_, _S2_, _L2_).
	edge(_E_, _F_, _S1_, _L1_).
	edge(_F_, _A_, _S2_, _L2_).
	edge(_E_, _G_, _S3_, _L3_).
	edge(_G_, _A_, _S4_, _L4_).

	% classes of figures; the last four arguments are the vertices
	:- public(class/5).

	% general case
	class(four_side_figure, _A_, _B_, _E_, _G_) :-
		dif, ground(vars(_A_, _B_, _E_, _G_)).
	% non-perpendicular figures
	class(parallelogram,    _A_, _B_, _E_, _F_) :-
		dif, var(_P_), ground(vars(_A_, _B_, _E_, _F_, _S1_, _S2_)).
	class(rhombus,          _A_, _B_, _C_, _D_) :-
		dif, var(_P_), ground(vars(_A_, _B_, _C_, _D_, _S1_, _S2_)).
	% perpendicular figures
	class(rectangular, _A_, _B_, _E_, _F_) :-
		dif, ground(vars(_P_, _A_, _B_, _E_, _F_, _S1_, _S2_)).
	class(square, _A_, _B_, _C_, _D_) :-
		dif, ground(vars(_P_, _A_, _B_, _C_, _D_, _S1_, _S2_)).

	:- public(class/1).
	class(Class) :-
		once(class(Class, _, _, _, _)).

	:- public(perpendicularity/0).
	perpendicularity :-
		dif, ground(vars(_S1_, _S2_)),
		(	(_S1_ - _S2_) mod 90 =:= 0 ->
			_P_ = true
		;	true
		).

	% ensure that vertices, edge slopes, and
	% edge lengths bindings are distinct
	dif :-
		dif([_A_, _B_, _C_, _D_, _E_, _F_, _G_]),
		dif([_S1_, _S2_, _S3_, _S4_]),
		dif([_L1_, _L2_, _L3_, _L4_]).

:- end_object.
