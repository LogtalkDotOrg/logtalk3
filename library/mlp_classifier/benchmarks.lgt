%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  This file is part of Logtalk <https://logtalk.org/>
%  SPDX-FileCopyrightText: 1998-2026 Paulo Moura <pmoura@logtalk.org>
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


:- object(mlp_classifier_benchmarks).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Fixed-workload training and prediction benchmarks for the mlp_classifier library.'
	]).

	:- public(run/0).
	:- mode(run, one).
	:- info(run/0, [
		comment is 'Prints five CPU and wall time samples per warmed-up workload.'
	]).

	run :-
		Options = [hidden_layers([16, 8]), maximum_iterations(200), tolerance(0.0), random_seed(42), learning_schedule(constant)],
		measure(training_momentum, mlp_classifier::learn(xor, _, [momentum(0.9)| Options])),
		measure(training_plain, mlp_classifier::learn(xor, _, [momentum(0.0)| Options])),
		measure(training_unregularized, mlp_classifier::learn(xor, _, [momentum(0.0), l2_regularization(0.0)| Options])),
		measure(training_loss, mlp_classifier::learn(xor, _, [convergence_criterion(loss), momentum(0.9)| Options])),
		mlp_classifier::learn(xor, Classifier, [maximum_iterations(1), hidden_layers([16, 8])]),
		measure(prediction, predictions(Classifier)).

	:- meta_predicate(measure(*, 0)).

	measure(Name, Goal) :-
		call(Goal),
		forall(
			list::member(Sample, [1, 2, 3, 4, 5]),
			(	os::cpu_time(CPU0),
				os::wall_time(Wall0),
				call(Goal),
				os::cpu_time(CPU1),
				os::wall_time(Wall1),
				CPU is CPU1 - CPU0,
				Wall is Wall1 - Wall0,
				write(benchmark(Name, Sample, CPU, Wall)), nl
			)
		).

	predictions(Classifier) :-
		predictions(2000, Classifier).

	predictions(0, _) :-
		!.
	predictions(Count, Classifier) :-
		mlp_classifier::predict_probabilities(Classifier, [x1-0.0, x2-1.0], _),
		Next is Count - 1,
		predictions(Next, Classifier).

:- end_object.
