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


:- object(mlp_classifier_fixtures).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Custom-functor and malformed model fixtures for the mlp_classifier library.'
	]).

	:- public([
		custom_functor/1, invalid_model/1, incomplete_model/1
	]).

	custom_functor(custom_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).

	incomplete_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers(_), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).

	invalid_model(mlp_classifier(a)).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(other), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([b, a]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(1), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(sigmoid), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(sigmoid), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([2]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(3), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([c-1.0]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[1.0], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, a], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, a]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [continuous(x, 0.0, 0.0)], tanh, [layer([[0.1, 0.2], [0.3, 0.4]], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(2), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], malformed_diagnostics)).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([a-1.0, a-2.0]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).
	invalid_model(mlp_classifier([a, b], [], tanh, [layer([[], []], [0.0, 0.0])], [model(other), model(mlp_classifier), classes([a, b]), encoded_features(0), hidden_layers([]), activation(tanh), iterations(1), options([class_weights([]), hidden_layers([]), activation(tanh), maximum_iterations(2)])])).

:- end_object.


:- object(mlp_no_features,
	implements(dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Single-example bias-only dataset for the mlp_classifier library.'
	]).

	class(result).

	class_values([a, b]).

	attribute_values(_, _) :-
		fail.

	example(1, a, []).

:- end_object.


:- object(mlp_empty_dataset,
	extends(mlp_no_features)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-10-13,
		comment is 'Empty dataset for the mlp_classifier library.'
	]).

	example(_, _, _) :-
		fail.

:- end_object.
