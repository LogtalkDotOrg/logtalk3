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


% Legacy heap protocol and objects using the binary heap implementation
% for backward-compatibility


:- protocol(heapp,
	extends(heap_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Heap protocol. Deprecated. Use the ``heap_protocol`` protocol instead.',
		see_also is [heap_protocol]
	]).

:- end_protocol.



:- object(heap(Order),
	extends(binary_heap(Order))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2026-01-28,
		comment is 'Heap implementation, parameterized by the order to be used to compare keys (``<`` or ``>``). Deprecated. Use the ``binary_heap/1`` object instead.',
		parameters is [
			'Order' - 'Either ``<`` for a min heap or ``>`` for a max heap.'
		],
		see_also is [binary_heap(_)]
	]).

:- end_object.


:- object(minheap,
	extends(binary_heap(<))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2010-02-19,
		comment is 'Min-heap implementation. Uses standard order to compare keys. Deprecated. Use the ``binary_heap_min`` object instead.',
		see_also is [binary_heap_min]
	]).

:- end_object.


:- object(maxheap,
	extends(binary_heap(>))).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2010-02-19,
		comment is 'Max-heap implementation. Uses standard order to compare keys. Deprecated. Use the ``binary_heap_max`` object instead.',
		see_also is [binary_heap_max]
	]).

:- end_object.
