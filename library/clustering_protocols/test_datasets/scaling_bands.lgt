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


:- object(scaling_bands,
	implements(clustering_dataset_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-04-23,
		comment is 'Synthetic continuous dataset with two horizontal bands and large x-range variation, suitable for feature-scaling sensitivity tests.'
	]).

	attribute_values(x, continuous).
	attribute_values(y, continuous).

	example(1, [x-0.0, y-0.0]).
	example(2, [x-10.0, y-0.1]).
	example(3, [x-20.0, y-(-0.1)]).
	example(4, [x-0.0, y-5.0]).
	example(5, [x-10.0, y-5.1]).
	example(6, [x-20.0, y-4.9]).

:- end_object.
