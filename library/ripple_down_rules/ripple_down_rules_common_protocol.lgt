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


:- protocol(ripple_down_rules_common_protocol).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-08-08,
		comment is 'Common orotocol for Ripple-Down Rules models.'
	]).

	:- public(new/1).
	:- mode(new(-compound), one).
	:- info(new/1, [
		comment is 'Creates an empty Ripple-Down Rules model using default options.',
		argnames is ['Model']
	]).

	:- public(new/2).
	:- mode(new(-compound, +list(compound)), one_or_error).
	:- info(new/2, [
		comment is 'Creates an empty Ripple-Down Rules model using the given options.',
		argnames is ['Model', 'Options'],
		exceptions is [
			'``Options`` is a variable or a partial list' - instantiation_error,
			'``Options`` is neither a variable nor a list' - type_error(list, 'Options'),
			'An element ``Option`` of the list ``Options`` is neither a variable nor a compound term' - type_error(compound, 'Option'),
			'An element ``Option`` of the list ``Options`` is a compound term but not a valid option' - domain_error(option, 'Option')
		]
	]).

	:- public(classify/3).
	:- mode(classify(+compound, @term, --term), one).
	:- info(classify/3, [
		comment is 'Classifies a case using a Ripple-Down Rules model.',
		argnames is ['Model', 'Case', 'Conclusion']
	]).

	:- public(classify/4).
	:- mode(classify(+compound, @term, --term, -list(compound)), one).
	:- info(classify/4, [
		comment is 'Classifies a case and returns an ordered inference trace.',
		argnames is ['Model', 'Case', 'Conclusion', 'Trace']
	]).

	:- public(revise/6).
	:- meta_predicate(revise(*, *, *, 2, 3, *)).
	:- mode(revise(+compound, @term, +atom, +callable, +callable, -compound), one_or_error).
	:- info(revise/6, [
		comment is 'Returns a new model after applying one correction. The ``scrdr`` implementation accepts the correction atom ``replace``. The ``mcrdr`` implementation accepts the correction atoms ``add``, ``remove``, and ``filter``. The ``grdr`` implementation accepts the correction atoms supported by the keyed submodel and requires the second argument to be a ``Key-Case`` pair. The condition closure is called with the case and current conclusions. The conclusion closure is called with the case and current conclusions plus an output argument.',
		argnames is ['Model', 'Case', 'Correction', 'Condition', 'Conclusion', 'NewRDR'],
		exceptions is [
			'``Correction`` is a variable' - instantiation_error,
			'``Correction`` is not supported by the receiving implementation or keyed submodel' - domain_error(ripple_down_rule_correction, 'Correction')
		]
	]).

	:- public(as_list/2).
	:- mode(as_list(+compound, -list(compound)), one).
	:- info(as_list/2, [
		comment is 'Returns stable preorder descriptors for all rules in a model.',
		argnames is ['Model', 'Rules']
	]).

	:- public(size/2).
	:- mode(size(+compound, -integer), one).
	:- info(size/2, [
		comment is 'Returns the number of rules in a model.',
		argnames is ['Model', 'Size']
	]).

	:- public(check_model/1).
	:- mode(check_model(@compound), one_or_error).
	:- info(check_model/1, [
		comment is 'Checks that a term is a structurally valid model for the receiving implementation.',
		argnames is ['Model'],
		exceptions is [
			'``Model`` is a variable' - instantiation_error,
			'``Model`` is neither a variable nor a valid model' - domain_error(ripple_down_rules_model, 'Model')
		]
	]).

	:- public(valid_model/1).
	:- mode(valid_model(@compound), zero_or_one).
	:- info(valid_model/1, [
		comment is 'True iff the argument is a structurally valid model for the receiving implementation.',
		argnames is ['Model']
	]).

:- end_protocol.
