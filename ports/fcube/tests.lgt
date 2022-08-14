%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  Copyright 2012 Mauro Ferrari      <mauro.ferrari@uninsubria.it>
%  Copyright 2012 Camillo Fiorentini <fiorenti@dsi.unimi.it>
%  Copyright 2012 Guido Fiorino      <guido.fiorino@unimib.it>
%  Copyright 2020-2021 Paulo Moura   <pmoura@logtalk.org>
%  SPDX-License-Identifier: GPL-2.0-or-later
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- object(tests,
	extends(lgtunit)).

	:- info([
		version is 4:1:0,
		author is 'Tests from Joseph Vidal-Rosset - Personal Blog (https://www.vidal-rosset.net/fCube/); ported to Logtalk by Paulo Moura.',
		date is 2022-08-14,
		comment is 'Tests for FCube: An Efficient Prover for Intuitionistic Propositional Logic.'
	]).

	:- uses(fcube, [
		decide/2
	]).

	cover(fcube).

	% tests from the examples available at https://www.vidal-rosset.net/fCube/

	test(fcube_excluded_middle, true(CounterModel == [swff(f,a),[swff(t,a)]])) :-
		^^suppress_text_output,
		decide((~ a | a), CounterModel).

	test(fcube_double_negation_elimination, true(CounterModel == [[swff(f, a), [swff(t, a)]]])) :-
		^^suppress_text_output,
		decide((~ ~ a => a), CounterModel).

	test(fcube_pierceformula, true(CounterModel == [swff(fc, b), [swff(f, a), [swff(t, a)]]])) :-
		^^suppress_text_output,
		decide((((a => b) => a) => a), CounterModel).

	test(fcube_dummett_formula, true(CounterModel == [[swff(fc, b), swff(t, a)], [swff(fc, a), swff(t, b)]])) :-
		^^suppress_text_output,
		decide(((a => b) | (b => a)), CounterModel).

	test(fcube_classical_de_morgan_implication, true(CounterModel == [[[swff(fc, a), swff(t, b)], [swff(fc, b), swff(t, a)]]])) :-
		^^suppress_text_output,
		decide((~ (a & b) => (~ a | ~ b)), CounterModel).

	test(fcube_intuitionistic_de_morgan_implication, true(CounterModel == [valida])) :-
		^^suppress_text_output,
		decide(((~ a | ~ b) => ~ (a & b)), CounterModel).

	test(fcube_intuitionistic_de_morgan_equivalence, true(CounterModel == [valida])) :-
		^^suppress_text_output,
		decide((~ (a | b) <=> (~ a & ~ b)), CounterModel).

	test(fcube_intuitionistic_equivalence, true(CounterModel ==  [valida])) :-
		^^suppress_text_output,
		decide(((~ a => a) <=> ~ ~ a), CounterModel).

	test(fcube_pelletier_problem_13_SYN045_plus_1, true(CounterModel == [valida])) :-
		^^suppress_text_output,
		decide((( ( p | ( q & r ) ) <=> ( ( p | q ) & ( p | r ) ) )), CounterModel).

	test(fcube_pelletier_problem_17_SYN047_plus_1, true(CounterModel == [[swff(f, s), swff(fc, r), swff(f, q), [swff(t, p), swff(t, q), swff(t, s)]]])) :-
		^^suppress_text_output,
		decide((( ( ( p & ( q => r ) ) => s ) <=> ( ( ~ p | q | s ) & ( ~ p | ~ r | s ) ) )), CounterModel).

	test(fcube_formula_1, true(CounterModel == [[swff(fc, a), swff(fc, c), swff(t, b)]])) :-
		^^suppress_text_output,
		decide((((a | b) => c) <=> ((a => c) & (b => b))), CounterModel).

	test(fcube_formula_2, true(CounterModel == [[[swff(fc, a), swff(fc, c), swff(t, b)], [swff(fc, b), swff(fc, c), swff(t, a)]]])) :-
		^^suppress_text_output,
		decide((((a & b) => c) <=> ((a => c) | (b => c))), CounterModel).

:- end_object.
