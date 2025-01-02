%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2025 Paulo Moura <pmoura@logtalk.org>
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


:- object(figures(
	% vertices
	_A_, _B_, _C_, _D_, _E_, _F_, _G_,
	% edge slopes
	_S1_, _S2_, _S3_, _S4_,
	% edge lengths
	_L1_, _L2_, _L3_, _L4_
)).

	:- info([
		version is 1:1:0,
		author is 'Paulo Moura',
		date is 2021-11-27,
		comment is 'Example of network modeling for recognizing polyhedra represented as a graph.',
		source is '"A framework for network modeling in Prolog", Zdravko I. Markov, IJCAI, 1989.'
	]).

	:- uses(dif, [
		dif/1
	]).

	% graph representation of polyhedra
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
	class(Class, A, B, C, D) :-
		% vertices must be distinct
		dif([_A_, _B_, _C_, _D_, _E_, _F_, _G_]),
		% classify the polyhedra
		class_(Class, A, B, C, D),
		% ensure all constraints are solved
		ground(vars(A, B, C, D)).

	class_(four_side_figure, _A_, _B_, _E_, _G_).
	class_(parallelogram,    _A_, _B_, _E_, _F_).
	class_(rhombus,          _A_, _B_, _C_, _D_).

	:- public(class/1).
	class(Class) :-
		class(Class, _, _, _, _).

:- end_object.
