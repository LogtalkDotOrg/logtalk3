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


:- object(layered_baskets,
	implements(transaction_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-13,
		comment is 'Transaction dataset with overlapping co-occurrence layers suitable for support-count and pruning tests.'
	]).

	items([beer, bread, butter, diapers, milk]).

	transaction(1, [beer, diapers, milk]).
	transaction(2, [bread, butter, milk]).
	transaction(3, [beer, bread, diapers, milk]).
	transaction(4, [bread, diapers, milk]).
	transaction(5, [beer, bread, butter]).
	transaction(6, [bread, butter, diapers, milk]).
	transaction(7, [beer, bread, diapers]).

:- end_object.
