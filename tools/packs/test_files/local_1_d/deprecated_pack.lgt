%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
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


:- object(deprecated_pack,
	implements(pack_protocol)).

	:- info([
		version is 1:0:0,
		author is 'Paulo Moura',
		date is 2025-05-23,
		comment is 'A local deprecated pack for testing.'
	]).

	name(deprecated).

	description('A local deprecated pack for testing').

	license('Apache-2.0').

	home('file://test_files/foo').

	version(
		2:0:0,
		deprecated,
		'file://test_files/deprecated',
		none,
		[logtalk @>= 3:42:0],
		all
	).

	version(
		1:0:0,
		deprecated,
		'file://test_files/deprecated',
		none,
		[logtalk @>= 3:42:0],
		all
	).

:- end_object.
