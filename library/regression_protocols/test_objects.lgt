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


:- object(sample_regressor,
	imports([options, regressor_common])).

	learn(Dataset, sample_regressor(TargetName, Attributes, Target, Diagnostics), UserOptions) :-
		^^check_options(UserOptions),
		^^merge_options(UserOptions, Options),
		Dataset::target(TargetName),
		^^dataset_attributes(Dataset, Attributes),
		^^dataset_examples(Dataset, Examples),
		^^check_examples(Dataset, Examples),
		Examples = [example(_Id, Target, _AttributeValues)| _],
		length(Examples, TrainingExampleCount),
		build_diagnostics(TargetName, TrainingExampleCount, Options, Diagnostics).

	predict(sample_regressor(_TargetName, _Attributes, Target, _Diagnostics), _Instance, Target).

	build_diagnostics(TargetName, TrainingExampleCount, Options, Diagnostics) :-
		^^base_regressor_diagnostics(sample_regressor, TargetName, TrainingExampleCount, Options, [], Diagnostics).

	check_regressor(Regressor) :-
		(	Regressor = sample_regressor(TargetName, Attributes, Target, Diagnostics),
			atom(TargetName),
			^^valid_attribute_declarations(Attributes),
			number(Target),
			^^valid_regressor_metadata(sample_regressor, Diagnostics) ->
			true
		;	domain_error(regressor, Regressor)
		).

	regressor_export_template(_Dataset, _Regressor, Functor, Template) :-
		Template =.. [Functor, 'Regressor'].

	regressor_term_template(sample_regressor(_TargetName, _Attributes, _Target, _Diagnostics), sample_regressor('TargetName', 'Attributes', 'Target', 'Diagnostics')).

	export_to_clauses(_Dataset, Regressor, Functor, [Clause]) :-
		Clause =.. [Functor, Regressor].

	print_regressor(Regressor) :-
		^^print_regressor_template(Regressor),
		writeq(Regressor), nl.

	default_option(sample_option(enabled)).

	valid_option(sample_option(Value)) :-
		once((Value == enabled; Value == disabled)).



:- end_object.
